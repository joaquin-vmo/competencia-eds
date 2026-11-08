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
#      reporte arrastra el último precio hasta MAX_ARRASTRE_SEM semanas; pasado ese plazo, las semanas
#      sin reporte quedan fuera hasta que la estación vuelve a reportar. Margen = precio - costo mayorista con MEPCO.
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

db <- leer_csv(ruta_proc("precios.csv"),
               col_select = c(id, date, fuel, price, distributor, municipality, region, latitud, longitud, time),
               col_types = cols(id = "c", fuel = "c", date = "D")) |>
  rename(distribuidor = distributor, comuna = municipality) |>
  # cadena: con bandera y más de una estación
  mutate(is_franchise = distribuidor != "sin bandera" & n_distinct(id) > 1L, .by = distribuidor) |>
  arrange(id, date, fuel, time) |>
  filter(!duplicated(consecutive_id(id, date, fuel), fromLast = TRUE)) |>
  select(-time)
ULT_DIA <- max(db$date)   # la última semana y el último mes incompletos se recortan

# ==============================================================================
# 1. Identidad de estaciones
# ==============================================================================

idinfo <- db |>
  summarise(first_date = min(date), last_date = max(date),
            lat = median(latitud, na.rm = TRUE), lon = median(longitud, na.rm = TRUE), .by = id) |>
  arrange(first_date, id)

# sucesión: el padre de i es el código ya apagado más cercano que empezó antes
cand <- filter(idinfo, !is.na(lat))
D_link <- dist_propia(cand$lat, cand$lon) * 1000
padre_sucesion <- rep(NA_character_, nrow(cand))
for (i in seq_len(nrow(cand))) {
  previos <- which(cand$last_date <= cand$first_date[i] & cand$first_date < cand$first_date[i])
  if (!length(previos)) next
  j <- previos[which.min(D_link[i, previos])]
  if (D_link[i, j] < RADIO_MISMO_LOCAL_M) padre_sucesion[i] <- cand$id[j]
}
idinfo <- left_join(idinfo, tibble(id = cand$id, padre_sucesion), by = "id")

# duplicación: código con letra final cuyo código base existe
dup <- idinfo |>
  filter(grepl("[0-9][a-z]$", id)) |>
  transmute(id, base_codigo = sub("[a-z]$", "", id), lat, lon) |>
  inner_join(transmute(idinfo, base_codigo = id, b_lat = lat, b_lon = lon), by = "base_codigo") |>
  mutate(metros_a_base = round(1000 * diag(dist_km(lat, lon, b_lat, b_lon)), 1))
idinfo <- idinfo |>
  left_join(select(dup, id, base_codigo, metros_a_base), by = "id") |>
  mutate(regla = case_when(!is.na(metros_a_base) & metros_a_base <= RADIO_MISMO_LOCAL_M ~ "duplicacion",
                           !is.na(padre_sucesion) ~ "sucesion"),
         padre = if_else(regla %in% "duplicacion", base_codigo, padre_sucesion))

padres <- setNames(idinfo$padre, idinfo$id)
raiz <- function(x) {   # sigue la cadena de padres; el tope corta un ciclo
  for (k in 1:100) {
    if (is.na(padres[[x]])) return(x)
    x <- padres[[x]]
  }
  stop("cadena de identidad sin raíz (posible ciclo) en: ", x)
}
idinfo <- mutate(idinfo, station_key = map_chr(id, raiz))

idinfo |>
  select(id, station_key, regla, padre, padre_sucesion, base_codigo, metros_a_base, first_date, last_date, lat, lon) |>
  escribir_csv(ruta_proc("identidad_estaciones.csv"))
message(sprintf("03_paneles.R: %d códigos -> %d estaciones (%d sucesiones, %d duplicaciones)",
                nrow(idinfo), n_distinct(idinfo$station_key), sum(idinfo$regla %in% "sucesion"),
                sum(idinfo$regla %in% "duplicacion")))

db <- left_join(db, select(idinfo, id, station_key), by = "id")

# atributos de estación: el valor más frecuente entre sus lecturas. La marca queda constante.
sattr <- db |>
  summarise(id = modal(id), distribuidor = modal(distribuidor), comuna = modal(comuna),
            region = modal(region), lat = median(latitud, na.rm = TRUE), lon = median(longitud, na.rm = TRUE),
            is_franchise = as.logical(modal(as.character(is_franchise))), .by = station_key) |>
  arrange(station_key)

# distancia a la red vial clase 1 del MOP (autopistas y rutas nacionales), en metros
red <- st_union(st_transform(st_read(ruta_raw("mapas", "red_vial", "redvial2019.shp"), quiet = TRUE,
                                     query = "SELECT Nom_Ruta FROM redvial2019 WHERE Clase_Ruta = 1"), CRS_M))
con_coord <- !is.na(sattr$lat)
sattr$d_autopista <- NA_real_
sattr$d_autopista[con_coord] <- as.numeric(st_distance(
  st_transform(st_as_sf(sattr[con_coord, ], coords = c("lon", "lat"), crs = 4326), CRS_M), red))

# ==============================================================================
# 2. Panel semanal
# ==============================================================================

db <- db |> arrange(station_key, fuel, date) |> mutate(wi = semana(date))
obs <- db |>
  filter(wi >= 0L) |>
  select(station_key, fuel, wi, price) |>
  filter(!duplicated(consecutive_id(station_key, fuel, wi), fromLast = TRUE))

costo <- leer_csv(ruta_proc("costo_mepco.csv")) |>
  transmute(wi = semana(date), `93` = `93_w/`, `97` = `97_w/`, di = `di_w/`) |>
  pivot_longer(-wi, names_to = "fuel", values_to = "cost")

# grilla completa entre la primera y la última semana de cada estación-combustible
panel_w <- obs |>
  reframe(wi = seq(min(wi), max(wi)), .by = c(station_key, fuel)) |>
  left_join(mutate(obs, obs_wi = wi), by = c("station_key", "fuel", "wi")) |>
  mutate(observed = !is.na(price)) |>
  group_by(station_key, fuel) |>
  fill(price, obs_wi) |>
  ungroup() |>
  filter(wi - obs_wi <= MAX_ARRASTRE_SEM) |>
  select(-obs_wi) |>
  left_join(costo, by = c("fuel", "wi")) |>
  mutate(margin = price - cost, wk = SEMANA_0 + wi * 7L) |>
  mutate(ym = mi2date(mi(wk))) |>
  filter(wk + 6L <= ULT_DIA)
if (max(costo$wi) < max(panel_w$wi)) warning("la serie MEPCO termina antes que el panel: el margen final queda NA")
panel_w <- panel_w |>
  left_join(select(sattr, -d_autopista), by = "station_key") |>
  relocate(station_key, fuel, wi, wk, ym, price, observed, cost, margin) |>
  arrange(station_key, fuel, wi)

# ==============================================================================
# 3. Panel mensual: la última semana de cada mes
# ==============================================================================

ult <- filter(panel_w, !duplicated(consecutive_id(station_key, fuel, ym), fromLast = TRUE))
panel_m <- ult |>
  pivot_wider(id_cols = c(station_key, ym), names_from = fuel, values_from = c(price, margin)) |>
  rename(p93 = price_93, p95 = price_95, p97 = price_97, pdi = price_di, m93 = margin_93, m97 = margin_97, mdi = margin_di) |>
  select(-margin_95) |>   # la 95 no tiene costo MEPCO
  arrange(station_key, ym) |>
  left_join(summarise(ult, observed = any(observed), .by = c(station_key, ym)), by = c("station_key", "ym")) |>
  left_join(select(sattr, -d_autopista), by = "station_key") |>
  relocate(all_of(names(select(sattr, -d_autopista)))) |>
  mutate(miym = mi(ym)) |>
  filter(mi2date(miym + 1L) - 1L <= ULT_DIA) |>   # solo meses completos
  arrange(station_key, ym)

primera <- summarise(panel_m, primera_ym = min(ym), .by = station_key)   # primera aparición en toda la historia
inicio_id <- summarise(idinfo, fecha_entrante = min(first_date), .by = station_key)   # primer día con reporte

# ==============================================================================
# 4. Ventanas: incumbentes, entradas y tratamiento
# ==============================================================================

# Para cada estación, con las entradas de la ventana:
#   no cuentan su propia entrada, las entradas fuera de su vida en el panel y las de su misma
#   marca (expansión de la propia cadena, no un competidor).
#   g_entry: mes de la primera entrada competidora a <= R_TRATADA; g2_entry: la segunda.
#   role_entry por la distancia mínima a la que alguna vez hubo una entrada:
#     treated (<= 2 km), buffer (2 a 3 km), misma_marca (no tratada, con una entrada de su propia
#     marca a <= 3 km: no sirve de control), ctrl_a (alguna entrada, de otra marca o de la propia,
#     entre 3 y 5 km), ctrl_b (ninguna entrada de ninguna marca a <= 5 km). Así el control amplio
#     no tiene entradas a <= 3 km y el estricto no tiene entradas a <= 5 km, de ninguna marca.
#   *5_entry: la misma asignación con radio 5 km (primera entrada y, en el mismo mes, la más cercana).
#   g_entry_r<k>, g2_entry_r<k> (k en RADIOS_EXTRA): primera y segunda entrada competidora a <= k km,
#     para usar el radio k como definición de tratada.
#   entrante_*: las entrantes que tratan a la estación (mes g_entry, <= 2 km, otra marca); entrante_marca
#     es la marca de la más cercana de ellas.
#   entrante5_marca: marca de la entrante que define g5_entry y ring_entry (la más cercana del primer mes
#     con una entrada a <= 5 km).
min_o_na <- function(x) if (all(is.na(x))) NA_integer_ else as.integer(min(x, na.rm = TRUE))

asignar_tratamiento <- function(sloc, entradas) {
  D_ev <- dist_km(sloc$lat, sloc$lon, entradas$elat, entradas$elon)
  g_ev <- mi(entradas$g)
  D_ev[cbind(match(entradas$station_key, sloc$station_key), seq_len(nrow(entradas)))] <- Inf
  D_ev[!(outer(mi(sloc$entry_ym), g_ev, "<") & outer(mi(sloc$last_ym), g_ev, ">="))] <- Inf
  misma_marca <- outer(marca_red(sloc$distribuidor), marca_red(entradas$edist), "==")
  misma_marca <- !is.na(misma_marca) & misma_marca
  D_mm <- D_ev
  D_mm[!misma_marca] <- Inf   # solo entradas de la propia marca
  D_ev[misma_marca] <- Inf

  nombres_extra <- paste0(rep(c("g_entry_r", "g2_entry_r"), length(RADIOS_EXTRA)), rep(RADIOS_EXTRA, each = 2))
  map(seq_len(nrow(sloc)), function(i) {
    d <- D_ev[i, ]
    meses <- sort(unique(g_ev[d <= R_TRATADA]))
    g <- if (length(meses)) meses[1] else NA_integer_
    w5 <- which(d <= R_CTRL_EST)
    w5 <- w5[order(g_ev[w5], d[w5])]
    meses5 <- unique(g_ev[w5])
    mindist <- round(min(d), 3)
    mindist_mm <- min(D_mm[i, ])
    extra <- lapply(RADIOS_EXTRA, function(r) {
      m <- sort(unique(g_ev[d <= r]))
      mi2date(c(if (length(m)) m[1] else NA_integer_, if (length(m) >= 2) m[2] else NA_integer_))
    })
    trata <- if (is.na(g)) integer(0) else which(d <= R_TRATADA & g_ev == g)
    cercana <- trata[which.min(d[trata])]
    tibble(
      station_key = sloc$station_key[i],
      mindist     = mindist,
      g_entry     = mi2date(g),
      g2_entry    = mi2date(if (length(meses) >= 2) meses[2] else NA_integer_),
      role_entry  = case_when(!is.na(g) ~ "treated", mindist <= R_CTRL_AMP ~ "buffer",
                              mindist_mm <= R_CTRL_AMP ~ "misma_marca",
                              pmin(mindist, mindist_mm) <= R_CTRL_EST ~ "ctrl_a", .default = "ctrl_b"),
      dist_entry  = if (is.na(g)) NA_real_ else round(min(d[trata]), 3),
      g5_entry    = mi2date(if (length(w5)) meses5[1] else NA_integer_),
      g2_5_entry  = mi2date(if (length(meses5) >= 2) meses5[2] else NA_integer_),
      dist_entry5 = if (length(w5)) round(d[w5[1]], 3) else NA_real_,
      ring_entry  = as.character(cut(if (length(w5)) d[w5[1]] else NA_real_, breaks = 0:5, include.lowest = TRUE,
                                     labels = c("0-1 km", "1-2 km", "2-3 km", "3-4 km", "4-5 km"))),
      role5_entry = if (length(w5)) "treated" else if (mindist_mm <= R_CTRL_EST) "misma_marca" else "ctrl_b",
      entrante_key        = if (length(trata)) entradas$station_key[cercana] else NA_character_,
      entrante_marca      = if (length(trata)) entradas$edist[cercana] else NA_character_,
      entrante_autopista  = if (length(trata)) any(entradas$d_autopista[trata] <= AUTOPISTA_M) else NA,
      entrante_semana     = min_o_na(entradas$semana_1[trata]),
      entrante5_marca     = if (length(w5)) entradas$edist[w5[1]] else NA_character_,
      !!!set_names(as.list(do.call(c, extra)), nombres_extra)
    )
  }) |>
    list_rbind()
}

# estaciones con registro a <= R_TRATADA km en cada mes, sin contarse a sí misma.
# n_vecinas_previas no cuenta a las que aparecieron desde el mes de entrada de la tratada.
vecinas_mes <- function(pm, asg) {
  s <- pm |> filter(!is.na(lat)) |> distinct(station_key, lat, lon)
  ix <- which(dist_propia(s$lat, s$lon) <= R_TRATADA, arr.ind = TRUE)
  pares <- tibble(station_key = s$station_key[ix[, 1]], vecina = s$station_key[ix[, 2]]) |>
    left_join(summarise(pm, ini = min(miym), .by = station_key) |> rename(vecina = station_key), by = "vecina") |>
    left_join(asg |> filter(role_entry == "treated") |> transmute(station_key, g = mi(g_entry)), by = "station_key")
  pares |>
    inner_join(distinct(pm, vecina = station_key, miym), by = "vecina", relationship = "many-to-many") |>
    summarise(n_vecinas = n(), n_vecinas_previas = sum(is.na(g) | ini < g), .by = c(station_key, miym))
}

construir_ventana <- function(nombre, v) {
  inicio <- as.Date(sprintf("%d-01-01", v$desde))
  pm <- filter(panel_m, ym >= inicio, ym <= v$hasta)
  pw <- filter(panel_w, wk >= inicio, wk <= v$hasta)

  # incumbente (base): su primera aparición en toda la historia es a más tardar el año `desde`
  st <- pm |>
    summarise(entry_ym = min(ym), last_ym = max(ym), .by = station_key) |>
    inner_join(sattr, y = _, by = "station_key") |>
    inner_join(primera, by = "station_key") |>
    mutate(base = year(primera_ym) <= v$desde)

  entradas <- st |>
    filter(base == FALSE, !is.na(lat)) |>
    transmute(station_key, g = entry_ym, elat = lat, elon = lon, eregion = region, edist = distribuidor,
              efranchise = is_franchise, d_autopista) |>
    arrange(g, station_key) |>
    mutate(event_id = paste0("EN", row_number())) |>
    left_join(pw |> filter(!is.na(price)) |> summarise(semana_1 = min(wi), .by = station_key), by = "station_key")

  asg <- asignar_tratamiento(filter(st, !is.na(lat)), entradas) |>
    left_join(rename(inicio_id, entrante_key = station_key), by = "entrante_key")
  estacion <- asg |>
    left_join(select(st, station_key, base), by = "station_key") |>
    relocate(base, .after = station_key) |>
    inner_join(select(sattr, station_key, d_autopista), by = "station_key")
  unir_est <- function(d) left_join(d, estacion, by = "station_key") |> relocate(all_of(names(estacion)))

  pm <- pm |>
    unir_est() |>
    left_join(vecinas_mes(pm, asg), by = c("station_key", "miym")) |>
    mutate(n_vecinas = coalesce(n_vecinas, 0L), n_vecinas_previas = coalesce(n_vecinas_previas, 0L)) |>
    # precio menos la media nacional del mes (todas las estaciones de la ventana con precio)
    mutate(across(all_of(names(PRECIOS)), \(x) x - mean(x, na.rm = TRUE), .names = "{.col}_dev"), .by = miym) |>
    arrange(station_key, ym)

  dir.create(dir_ventana(nombre), recursive = TRUE, showWarnings = FALSE)
  escribir_csv(pm, file.path(dir_ventana(nombre), "panel_mensual.csv"))
  escribir_csv(entradas, file.path(dir_ventana(nombre), "entradas.csv"))
  if (nombre == "2012_2026")   # el panel semanal solo lo usan las estimaciones de la ventana principal
    escribir_csv(unir_est(pw), file.path(dir_ventana(nombre), "panel_semanal.csv.gz"))

  message(sprintf("  ventana %s: %d estaciones (%d incumbentes), %d entradas (%d desde %d)",
                  nombre, nrow(st), sum(st$base), nrow(entradas), sum(year(entradas$g) >= v$focal), v$focal))
}

for (nombre in names(VENTANAS)) construir_ventana(nombre, VENTANAS[[nombre]])
