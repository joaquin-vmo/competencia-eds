# 03_paneles.R
#
# Construye los paneles de estaciones que usan las estimaciones:
#   1. Identidad: un mismo local puede cambiar de código en la plataforma. Si no se
#      corrige, la recodificación aparece como una entrada y "trata" a sus vecinas.
#      - sucesión: un código hereda la estación del código ya apagado más cercano que
#        empezó antes, si está a menos de RADIO_MISMO_LOCAL_M.
#      - duplicación: un código con letra final (co1234a) cuyo código base existe a
#        <= RADIO_MISMO_LOCAL_M.
#   2. Panel semanal: último precio de cada semana (jueves a miércoles). Una semana sin
#      reporte arrastra el último precio hasta MAX_ARRASTRE_SEM semanas; un vacío mayor saca
#      a la estación. Margen = precio - costo mayorista con MEPCO.
#   3. Panel mensual: la última semana de cada mes.
#   4. Por ventana (VENTANAS): incumbentes, entradas y asignación de tratamiento, más las
#      variables de estación que usan las estimaciones (distancia a autopista, entrante que
#      trata a cada tratada, estaciones vecinas por mes, precio relativo a la media del mes).
#
# Entrada: data/processed/{precios,costo_mepco}.csv, data/raw/mapas/red_vial
# Salida:  data/processed/identidad_estaciones.csv y, por ventana, en data/processed/panel_<ventana>/:
#          panel_mensual.csv, entradas.csv y (solo 2012_2026) panel_semanal.csv.gz

source(here::here("code", "00_configuracion.R"))
library(sf)

# ==============================================================================
# Base diaria: una fila por estación-fecha-combustible, la última lectura del día
# ==============================================================================

db <- fread(ruta_proc("precios.csv"),
            select = c("id", "date", "fuel", "price", "distributor", "municipality", "region",
                       "latitud", "longitud", "time"),
            colClasses = list(character = c("id", "fuel"), IDate = "date"))
setnames(db, c("distributor", "municipality"), c("distribuidor", "comuna"))
db[, time := as.ITime(time)]

# cadena: con bandera y más de una estación
db[, is_franchise := distribuidor != "sin bandera" & uniqueN(id) > 1L, by = distribuidor]
setorder(db, id, date, fuel, time, na.last = TRUE)
db <- unique(db, by = c("id", "date", "fuel"), fromLast = TRUE)[, time := NULL]
ULT_DIA <- max(db$date)   # la última semana y el último mes incompletos se recortan

# ==============================================================================
# 1. Identidad de estaciones
# ==============================================================================

idinfo <- db[, .(first_date = min(date), last_date = max(date),
                 lat = median(latitud, na.rm = TRUE), lon = median(longitud, na.rm = TRUE)), by = id]
setorder(idinfo, first_date, id)

# sucesión: el padre de i es el código ya apagado más cercano que empezó antes
cand <- idinfo[!is.na(lat)]
D_link <- dist_propia(cand$lat, cand$lon) * 1000
padre_sucesion <- rep(NA_character_, nrow(cand))
for (i in seq_len(nrow(cand))) {
  previos <- which(cand$last_date <= cand$first_date[i] & cand$first_date < cand$first_date[i])
  if (!length(previos)) next
  j <- previos[which.min(D_link[i, previos])]
  if (D_link[i, j] < RADIO_MISMO_LOCAL_M) padre_sucesion[i] <- cand$id[j]
}
idinfo[data.table(id = cand$id, padre_sucesion), on = "id", padre_sucesion := i.padre_sucesion]

# duplicación: código con letra final cuyo código base existe
dup <- idinfo[grepl("[0-9][a-z]$", id), .(id, base_codigo = sub("[a-z]$", "", id), lat, lon)]
dup <- dup[idinfo[, .(base_codigo = id, b_lat = lat, b_lon = lon)], on = "base_codigo", nomatch = NULL]
dup[, metros_a_base := round(1000 * diag(dist_km(lat, lon, b_lat, b_lon)), 1)]
idinfo[dup, on = "id", `:=`(base_codigo = i.base_codigo, metros_a_base = i.metros_a_base)]

idinfo[, regla := fcase(!is.na(metros_a_base) & metros_a_base <= RADIO_MISMO_LOCAL_M, "duplicacion",
                        !is.na(padre_sucesion), "sucesion")]
idinfo[, padre := fifelse(regla %in% "duplicacion", base_codigo, padre_sucesion)]

padres <- setNames(idinfo$padre, idinfo$id)
raiz <- function(x) {   # sigue la cadena de padres; el tope corta un ciclo
  for (k in 1:100) {
    if (is.na(padres[[x]])) return(x)
    x <- padres[[x]]
  }
  stop("cadena de identidad sin raíz (posible ciclo) en: ", x)
}
idinfo[, station_key := vapply(id, raiz, character(1))]

fwrite(idinfo[, .(id, station_key, regla, padre, padre_sucesion, base_codigo, metros_a_base,
                  first_date, last_date, lat, lon)],
       ruta_proc("identidad_estaciones.csv"))
message(sprintf("03_paneles.R: %d códigos -> %d estaciones (%d sucesiones, %d duplicaciones)",
                nrow(idinfo), uniqueN(idinfo$station_key), sum(idinfo$regla %in% "sucesion"),
                sum(idinfo$regla %in% "duplicacion")))

db[idinfo, on = "id", station_key := i.station_key]

# atributos de estación: el valor más frecuente entre sus lecturas. La marca queda constante.
sattr <- db[, .(id = modal(id), distribuidor = modal(distribuidor), comuna = modal(comuna),
                region = modal(region), lat = median(latitud, na.rm = TRUE), lon = median(longitud, na.rm = TRUE),
                is_franchise = as.logical(modal(as.character(is_franchise)))), by = station_key]
setorder(sattr, station_key)

# distancia a la red vial clase 1 del MOP (autopistas y rutas nacionales), en metros
red <- st_union(st_transform(st_read(ruta_raw("mapas", "red_vial", "redvial2019.shp"), quiet = TRUE,
                                     query = "SELECT Nom_Ruta FROM redvial2019 WHERE Clase_Ruta = 1"), CRS_M))
con_coord <- !is.na(sattr$lat)
sattr[con_coord, d_autopista := as.numeric(st_distance(
  st_transform(st_as_sf(sattr[con_coord], coords = c("lon", "lat"), crs = 4326), CRS_M), red))]

# ==============================================================================
# 2. Panel semanal
# ==============================================================================

setorder(db, station_key, fuel, date)
db[, wi := semana(date)]
obs <- unique(db[wi >= 0L, .(station_key, fuel, wi, price)], by = c("station_key", "fuel", "wi"), fromLast = TRUE)

mepco <- fread(ruta_proc("costo_mepco.csv"))
costo <- melt(mepco[, .(wi = semana(date), `93` = `93_w/`, `97` = `97_w/`, di = `di_w/`)],
              id.vars = "wi", variable.name = "fuel", value.name = "cost", variable.factor = FALSE)

# grilla completa entre la primera y la última semana de cada estación-combustible
panel_w <- obs[, .(wi = seq(min(wi), max(wi))), by = .(station_key, fuel)]
panel_w[obs, on = .(station_key, fuel, wi), `:=`(price = i.price, obs_wi = i.wi)]
panel_w[, observed := !is.na(price)]
panel_w[, c("price", "obs_wi") := lapply(.SD, nafill, type = "locf"), by = .(station_key, fuel),
        .SDcols = c("price", "obs_wi")]
panel_w <- panel_w[wi - obs_wi <= MAX_ARRASTRE_SEM][, obs_wi := NULL]
panel_w[costo, on = .(fuel, wi), cost := i.cost]
panel_w[, `:=`(margin = price - cost, wk = SEMANA_0 + wi * 7L)]
panel_w[, ym := mi2date(mi(wk))]
panel_w <- panel_w[wk + 6L <= ULT_DIA]
if (max(costo$wi) < max(panel_w$wi)) warning("la serie MEPCO termina antes que el panel: el margen final queda NA")
panel_w <- sattr[, !"d_autopista"][panel_w, on = "station_key"]
setcolorder(panel_w, c("station_key", "fuel", "wi", "wk", "ym", "price", "observed", "cost", "margin"))
setorder(panel_w, station_key, fuel, wi)

# ==============================================================================
# 3. Panel mensual: la última semana de cada mes
# ==============================================================================

ult <- unique(panel_w, by = c("station_key", "fuel", "ym"), fromLast = TRUE)
panel_m <- dcast(ult, station_key + ym ~ fuel, value.var = c("price", "margin"))
setnames(panel_m, c(paste0("price_", c("93", "95", "97", "di")), paste0("margin_", c("93", "97", "di"))),
         c("p93", "p95", "p97", "pdi", "m93", "m97", "mdi"))
panel_m[, margin_95 := NULL]   # la 95 no tiene costo MEPCO
panel_m[ult[, .(observed = any(observed)), by = .(station_key, ym)], on = .(station_key, ym), observed := i.observed]
panel_m <- sattr[, !"d_autopista"][panel_m, on = "station_key"]
panel_m[, miym := mi(ym)]
panel_m <- panel_m[mi2date(miym + 1L) - 1L <= ULT_DIA]   # solo meses completos
setorder(panel_m, station_key, ym)

primera <- panel_m[, .(primera_ym = min(ym)), by = station_key]   # primera aparición en toda la historia
inicio_id <- idinfo[, .(fecha_entrante = min(first_date)), by = station_key]   # primer día con reporte

# ==============================================================================
# 4. Ventanas: incumbentes, entradas y tratamiento
# ==============================================================================

# Para cada estación, con las entradas de la ventana:
#   no cuentan su propia entrada, las entradas fuera de su vida en el panel y las de su misma
#   marca (expansión de la propia cadena, no un competidor).
#   g_entry: mes de la primera entrada competidora a <= R_TRATADA; g2_entry: la segunda.
#   role_entry por la distancia mínima a la que alguna vez hubo una entrada:
#     treated (<= 2 km), buffer (2 a 3 km), ctrl_a (3 a 5 km), ctrl_b (nunca a <= 5 km).
#   *5_entry: la misma asignación con radio 5 km (primera entrada y, en el mismo mes, la más cercana).
#   entrante_*: las entrantes que tratan a la estación (mes g_entry, <= 2 km, otra marca).
min_o_na <- function(x) if (all(is.na(x))) NA_integer_ else as.integer(min(x, na.rm = TRUE))

asignar_tratamiento <- function(sloc, entradas) {
  D_ev <- dist_km(sloc$lat, sloc$lon, entradas$elat, entradas$elon)
  g_ev <- mi(entradas$g)
  D_ev[cbind(match(entradas$station_key, sloc$station_key), seq_len(nrow(entradas)))] <- Inf
  D_ev[!(outer(mi(sloc$entry_ym), g_ev, "<") & outer(mi(sloc$last_ym), g_ev, ">="))] <- Inf
  misma_marca <- outer(sloc$distribuidor, entradas$edist, "==")
  D_ev[!is.na(misma_marca) & misma_marca] <- Inf

  rbindlist(lapply(seq_len(nrow(sloc)), function(i) {
    d <- D_ev[i, ]
    meses <- sort(unique(g_ev[d <= R_TRATADA]))
    g <- if (length(meses)) meses[1] else NA_integer_
    w5 <- which(d <= R_CTRL_EST)
    w5 <- w5[order(g_ev[w5], d[w5])]
    meses5 <- unique(g_ev[w5])
    mindist <- round(min(d), 3)
    trata <- if (is.na(g)) integer(0) else which(d <= R_TRATADA & g_ev == g)
    cercana <- trata[which.min(d[trata])]
    data.table(
      station_key = sloc$station_key[i],
      mindist     = mindist,
      g_entry     = mi2date(g),
      g2_entry    = mi2date(if (length(meses) >= 2) meses[2] else NA_integer_),
      role_entry  = fcase(!is.na(g), "treated", mindist <= R_CTRL_AMP, "buffer",
                          mindist <= R_CTRL_EST, "ctrl_a", default = "ctrl_b"),
      dist_entry  = if (is.na(g)) NA_real_ else round(min(d[trata]), 3),
      g5_entry    = mi2date(if (length(w5)) meses5[1] else NA_integer_),
      g2_5_entry  = mi2date(if (length(meses5) >= 2) meses5[2] else NA_integer_),
      dist_entry5 = if (length(w5)) round(d[w5[1]], 3) else NA_real_,
      ring_entry  = as.character(cut(if (length(w5)) d[w5[1]] else NA_real_, breaks = 0:5, include.lowest = TRUE,
                                     labels = c("0-1 km", "1-2 km", "2-3 km", "3-4 km", "4-5 km"))),
      role5_entry = if (length(w5)) "treated" else "ctrl_b",
      entrante_key        = if (length(trata)) entradas$station_key[cercana] else NA_character_,
      entrante_autopista  = if (length(trata)) any(entradas$d_autopista[trata] <= AUTOPISTA_M) else NA,
      entrante_semana     = min_o_na(entradas$semana_1[trata])
    )
  }))
}

# estaciones con registro a <= R_TRATADA km en cada mes, sin contarse a sí misma.
# n_vecinas_previas no cuenta a las que aparecieron desde el mes de entrada de la tratada.
vecinas_mes <- function(pm, asg) {
  s <- unique(pm[!is.na(lat), .(station_key, lat, lon)])
  ix <- which(dist_propia(s$lat, s$lon) <= R_TRATADA, arr.ind = TRUE)
  pares <- data.table(station_key = s$station_key[ix[, 1]], vecina = s$station_key[ix[, 2]])
  pares[pm[, .(ini = min(miym)), by = .(vecina = station_key)], on = "vecina", ini := i.ini]
  pares[asg[role_entry == "treated", .(station_key, g = mi(g_entry))], on = "station_key", g := i.g]
  x <- pares[unique(pm[, .(vecina = station_key, miym)]), on = "vecina", allow.cartesian = TRUE, nomatch = NULL]
  x[, .(n_vecinas = .N, n_vecinas_previas = sum(is.na(g) | ini < g)), by = .(station_key, miym)]
}

construir_ventana <- function(nombre, v) {
  inicio <- as.IDate(sprintf("%d-01-01", v$desde))
  pm <- panel_m[ym >= inicio & ym <= v$hasta]
  pw <- panel_w[wk >= inicio & wk <= v$hasta]

  # incumbente (base): su primera aparición en toda la historia es a más tardar el año `desde`
  st <- pm[, .(entry_ym = min(ym), last_ym = max(ym)), by = station_key]
  st <- sattr[st, on = "station_key"][primera, on = "station_key", nomatch = NULL]
  st[, base := year(primera_ym) <= v$desde]

  entradas <- st[base == FALSE & !is.na(lat), .(station_key, g = entry_ym, elat = lat, elon = lon,
                                                 eregion = region, edist = distribuidor, efranchise = is_franchise,
                                                 d_autopista)]
  setorder(entradas, g, station_key)
  entradas[, event_id := paste0("EN", .I)]
  entradas[pw[!is.na(price), .(semana_1 = min(wi)), by = station_key], on = "station_key", semana_1 := i.semana_1]

  asg <- asignar_tratamiento(st[!is.na(lat)], entradas)
  asg[inicio_id, on = c(entrante_key = "station_key"), fecha_entrante := i.fecha_entrante]
  estacion <- st[, .(station_key, base)][asg, on = "station_key"][sattr[, .(station_key, d_autopista)],
                                                                  on = "station_key", nomatch = NULL]

  pm <- estacion[pm, on = "station_key"]
  pm[vecinas_mes(pm, asg), on = .(station_key, miym), `:=`(n_vecinas = i.n_vecinas, n_vecinas_previas = i.n_vecinas_previas)]
  pm[is.na(n_vecinas), `:=`(n_vecinas = 0L, n_vecinas_previas = 0L)]
  # precio menos la media nacional del mes (todas las estaciones de la ventana con precio)
  for (p in names(PRECIOS)) pm[, (paste0(p, "_dev")) := get(p) - mean(get(p), na.rm = TRUE), by = miym]
  setorder(pm, station_key, ym)

  dir.create(dir_ventana(nombre), recursive = TRUE, showWarnings = FALSE)
  fwrite(pm, file.path(dir_ventana(nombre), "panel_mensual.csv"))
  fwrite(entradas, file.path(dir_ventana(nombre), "entradas.csv"))
  if (nombre == "2012_2026")   # el panel semanal solo lo usan las estimaciones de la ventana principal
    fwrite(estacion[pw, on = "station_key"], file.path(dir_ventana(nombre), "panel_semanal.csv.gz"))

  message(sprintf("  ventana %s: %d estaciones (%d incumbentes), %d entradas (%d desde %d)",
                  nombre, nrow(st), sum(st$base), nrow(entradas), sum(year(entradas$g) >= v$focal), v$focal))
}

for (nombre in names(VENTANAS)) construir_ventana(nombre, VENTANAS[[nombre]])
