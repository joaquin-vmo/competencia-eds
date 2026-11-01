# 06_comovimiento.R
#
# Base de la respuesta de las rivales a un cambio de precio (Gráficos 7 y B.9), con el registro
# de cambios de la CNE con fecha y hora (régimen de reporte actual, 2015-06 a 2026).
#   1. Eventos: cambios efectivos de precio por estación-combustible, sin reversiones A -> B -> A
#      de menos de REVERSION_H horas (errores de digitación).
#   2. Estaciones: atributos modales desde INI y marca (Copec, Shell, Aramco u Otras).
#   3. Pares: cada cambio de precio "líder" de domingo a martes (días sin cambio de costo) entre
#      INI_SIM y FIN_SIM, con cada rival de otra marca a <= 10 km. Para cada par:
#        sigue_24: el siguiente cambio de la rival es en la misma dirección y dentro de 24 h,
#        iguala:   sigue_24 y fija el mismo precio que la líder (+-0,5 $/L).
#      Solo pares estación-combustible con reporte en todo INI..FIN (panel balanceado).
#
# Entrada: data/raw/bencina_en_linea/{2023..2026}.csv, data/processed/{identidad_estaciones,costo_mepco}.csv
# Salida:  data/processed/comovimiento_pares.csv.gz (una fila por cambio líder y rival)

source(here::here("code", "00_configuracion.R"))

DESDE_EVENTOS <- as.IDate("2015-06-01")
INI <- as.IDate("2021-01-07")    # panel balanceado: dos años de régimen semanal
FIN <- as.IDate("2025-12-31")    # antes del salto del MEPCO de marzo de 2026
INI_SIM <- REFORMA_MEPCO - 364L  # 52 semanas a cada lado de la reforma
FIN_SIM <- REFORMA_MEPCO + 363L
HOLGURA_BAL <- 90L               # días: primer evento <= INI + 90 y último >= FIN - 90
REVERSION_H <- 2                 # horas
RADIO_RIVAL <- 10                # km
HORAS_RESPUESTA <- 24
TOL_IGUALA <- 0.5                # $/L
COMB <- c("93", "95", "97", "DI")
COLS <- c("codigo", "distribuidor", "latitud", "longitud", "nom_comuna", "nom_region",
          "combustible", "precio", "fecha_actualizacion", "hora_actualizacion")

# ==============================================================================
# 1. Eventos
# ==============================================================================

# 2023.csv trae también fechas anteriores (desde 2015); 2024..2026.csv, solo las de su año
leer <- function(anio, desde, hasta) {
  x <- fread(ruta_raw("bencina_en_linea", paste0(anio, ".csv")), select = COLS, colClasses = "character")
  setnames(x, c("id", "distribuidor", "lat", "lon", "comuna", "region", "fuel", "precio", "fecha", "hora"))
  x <- x[fuel %in% COMB]
  u <- unique(x$fecha)
  x[, fecha := as.IDate(u)[match(fecha, u)]]
  x[fecha >= desde & fecha <= hasta]
}
x <- rbind(leer(2023, DESDE_EVENTOS, as.IDate("2023-12-31")),
           rbindlist(lapply(2024:2026, function(a) leer(a, as.IDate(sprintf("%d-01-01", a)), as.IDate(sprintf("%d-12-31", a))))))
x <- x[!grepl("prueba|cne", id, ignore.case = TRUE)]

h <- unique(x$hora)
seg <- as.numeric(as.POSIXct(paste("1970-01-01", h), tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
# texto no numérico en precio o coordenadas pasa a NA (y sale abajo o en `est`)
x[, `:=`(precio = suppressWarnings(as.numeric(precio)), lat = suppressWarnings(as.numeric(lat)), lon = suppressWarnings(as.numeric(lon)),
         ts = .POSIXct(as.numeric(fecha) * 86400 + seg[match(hora, h)], tz = "UTC"))]
x <- x[!is.na(ts) & !is.na(precio)]

# precios dentro de [0,6; 1,5] x la mediana del combustible-mes
x[, med := median(precio), by = .(fuel, m = format(fecha, "%Y-%m"))]
x <- x[precio >= 0.6 * med & precio <= 1.5 * med][, med := NULL]

# código -> estación, con las recodificaciones de 03_paneles.R
idn <- fread(ruta_proc("identidad_estaciones.csv"), select = c("id", "station_key"))
x <- idn[x, on = "id"]
x[is.na(station_key), station_key := id]

# un precio por estación-combustible-instante (el último leído); solo cambios efectivos
setorder(x, station_key, fuel, ts)
x <- unique(x, by = c("station_key", "fuel", "ts"), fromLast = TRUE)
x[, prev := shift(precio), by = .(station_key, fuel)]
x <- x[is.na(prev) | precio != prev]

# reversiones A -> B -> A en menos de REVERSION_H horas: se botan B y la vuelta
x[, `:=`(p_sig = shift(precio, -1L), t_sig = shift(ts, -1L), p_ant = shift(precio)), by = .(station_key, fuel)]
x[, error := !is.na(p_sig) & !is.na(p_ant) & p_sig == p_ant & as.numeric(difftime(t_sig, ts, units = "hours")) < REVERSION_H]
x[, vuelta := shift(error, fill = FALSE), by = .(station_key, fuel)]
x <- x[!error & !vuelta]
x[, prev := shift(precio), by = .(station_key, fuel)]
x <- x[is.na(prev) | precio != prev]
x[, dp := precio - prev]

# ==============================================================================
# 2. Estaciones
# ==============================================================================

modal_alfa <- function(v) {   # valor más frecuente; empate: el primero en orden alfabético
  v <- v[!is.na(v) & v != ""]
  if (!length(v)) NA else names(which.max(table(v)))
}
est <- x[fecha >= INI, .(distribuidor = modal_alfa(toupper(trimws(distribuidor))),
                         lat = as.numeric(modal_alfa(round(lat, 5))), lon = as.numeric(modal_alfa(round(lon, 5)))),
         by = station_key]
est[, marca := fcase(distribuidor == "COPEC", "Copec", distribuidor == "SHELL", "Shell",
                     distribuidor %in% c("ARAMCO", "PETROBRAS"), "Aramco", default = "Otras")]
est <- est[!is.na(lat) & lat < 0 & lon < 0]
e <- x[station_key %in% est$station_key, .(station_key, fuel, ts, fecha, precio, dp)]

# ==============================================================================
# 3. Pares líder-rival
# ==============================================================================

# panel balanceado: estación-combustible con reporte en todo INI..FIN y en ambos regímenes
bal <- e[, .(ini = min(fecha), fin = max(fecha),
             n_pre = sum(fecha >= INI & fecha < REFORMA_MEPCO), n_post = sum(fecha >= REFORMA_MEPCO & fecha <= FIN)),
         by = .(station_key, fuel)][ini <= INI + HOLGURA_BAL & fin >= FIN - HOLGURA_BAL & n_pre > 0 & n_post > 0,
                                    .(station_key, fuel)]

# día del ciclo: días desde la última actualización del costo (cualquier combustible)
costo <- fread(ruta_proc("costo_mepco.csv"))
costo <- melt(costo[, .(jueves = as.IDate(date), `93` = `93_w/`, `97` = `97_w/`, di = `di_w/`)],
              id.vars = "jueves", variable.name = "fuel", value.name = "c")
setorder(costo, fuel, jueves)
costo[, dc := c - shift(c), by = fuel]
upd <- data.table(fecha = sort(unique(costo[dc != 0, jueves])))[, f_upd := fecha]
ciclo <- upd[data.table(fecha = seq(INI, FIN, by = 1L)), on = "fecha", roll = TRUE][, .(fecha, dia_ciclo = as.integer(fecha - f_upd))]

e <- e[bal, on = .(station_key, fuel), nomatch = NULL][fecha >= INI_SIM - 7L & fecha <= FIN_SIM + 7L & !is.na(dp)]
e <- ciclo[e, on = "fecha", nomatch = NULL]
e[, `:=`(sgn = sign(dp), reg = fifelse(fecha >= REFORMA_MEPCO, "tri", "sem"))]
setorder(e, fecha)

# líderes: cambios de domingo a martes (día 3 a 5 del ciclo semanal, que parte el jueves)
lid <- e[fecha >= INI_SIM & fecha <= FIN_SIM - 2L & (dia_ciclo %% 7L) %in% 3:5,
         .(i = station_key, fuel, ts0 = ts, p_lid = precio, sgn, reg, dia_ciclo)]
lid[, ev := .I]

# rivales: otra marca a <= RADIO_RIVAL km (dos estaciones "Otras" también son rivales)
D <- dist_propia(est$lat, est$lon)
misma <- outer(est$marca, est$marca, "==") & est$marca != "Otras"
ix <- which(D <= RADIO_RIVAL & !misma, arr.ind = TRUE)
rivales <- data.table(i = est$station_key[ix[, 1]], j = est$station_key[ix[, 2]], d = D[ix])
rivales[, dist := cut(d, c(0, 1, 2, 3, 5, 10), labels = c("0-1", "1-2", "2-3", "3-5", "5-10"))]
rivales <- rivales[!is.na(dist)]   # d = 0: dos códigos en el mismo sitio, sin anillo

pares <- lid[rivales, on = "i", allow.cartesian = TRUE, nomatch = NULL]
pares <- pares[bal[, .(j = station_key, fuel)], on = .(j, fuel), nomatch = NULL]

# siguiente cambio de la rival en o después del cambio líder (join rodante hacia adelante)
ej <- e[, .(j = station_key, fuel, ts, p_j = precio, sgn_j = sgn)]
setkey(ej, j, fuel, ts)
sig <- ej[pares[, .(j, fuel, ts = ts0, ev)], on = .(j, fuel, ts), roll = -Inf,
          .(ev = i.ev, j = i.j, ts_sig = x.ts, p_sig = p_j, sgn_sig = sgn_j)]
pares <- sig[pares, on = .(ev, j)]

horas <- as.numeric(difftime(pares$ts_sig, pares$ts0, units = "hours"))
pares[, sigue_24 := 100 * fcoalesce(!is.na(ts_sig) & horas <= HORAS_RESPUESTA & sgn_sig == sgn, FALSE)]
pares[, iguala := 100 * fcoalesce(sigue_24 > 0 & abs(p_sig - p_lid) < TOL_IGUALA, FALSE)]

# muestra de la tesis: régimen semanal y semanas 2 y 3 del ciclo trisemanal (sin el traspaso por etapas)
pares[, muestra := fcase(reg == "sem", "Semanal (2022)", dia_ciclo >= 7L, "Trisemanal, semanas 2 y 3 (2023)")]
stopifnot("un cambio líder o una respuesta cae en jueves o viernes" =
            !any(wday(pares$ts0) %in% 5:6) && !any(wday(pares[sigue_24 > 0, ts_sig]) %in% 5:6))

fwrite(pares[!is.na(muestra), .(ev, i, j, fuel, reg, dia_ciclo, muestra, dist, sigue_24, iguala)],
       ruta_proc("comovimiento_pares.csv.gz"))
message(sprintf("06_comovimiento.R: %s cambios líderes, %s pares líder-rival",
                format(uniqueN(pares$ev), big.mark = ","), format(nrow(pares), big.mark = ",")))
