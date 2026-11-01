# 05_traspaso.R
#
# Cambio de precio de cada tratada en cada actualización del costo MEPCO de la gasolina 93,
# frente al cambio medio de los controles estrictos de su región en la misma actualización.
# En la actualización u el cambio se mide entre el día previo (u - 1) y u + DIAS_DESPUES,
# con el precio vigente (último reporte de a lo más VIGENCIA días antes).
# La entrada se fecha en el primer día con reporte de la entrante más cercana que trata a la
# estación (ventana 2012_2026), y se toman las actualizaciones a +-K_SEMANAS de esa fecha.
#
# Entrada: data/processed/{precios,costo_mepco,identidad_estaciones}.csv, panel_2012_2026/panel_mensual.csv
# Salida:  data/processed/traspaso_actualizaciones.csv (una fila por tratada y actualización)

source(here::here("code", "00_configuracion.R"))

DESDE_ENTRADA <- as.IDate("2014-10-01")   # primer trimestre completo con costo MEPCO
HASTA_ENTRADA <- as.IDate("2025-12-31")
FIN_COSTO     <- as.IDate("2026-02-28")   # el 26-mar-2026 se desactiva la estabilización y el costo salta
K_SEMANAS     <- 68L   # semanas a cada lado de la entrada (cubre la ventana de un año del Gráfico B.4)
DIAS_DESPUES  <- 6L    # días tras la actualización en que se mide el precio
MIN_CTRL_REG  <- 5L    # controles mínimos de la región en cada actualización
VIGENCIA      <- 91L   # días máximos desde el último reporte (13 semanas, como el panel)

est <- unique(fread(file.path(dir_ventana("2012_2026"), "panel_mensual.csv"),
                    select = c("station_key", "region", "role_entry", "entrante_key", "fecha_entrante")),
              by = "station_key")
trat <- est[role_entry == "treated" & fecha_entrante >= DESDE_ENTRADA & fecha_entrante <= HASTA_ENTRADA,
            .(station_key, region, fecha_e = as.IDate(fecha_entrante))]
ctrl <- est[role_entry == "ctrl_b", .(station_key, region)]

# actualizaciones del costo de la 93
costo <- fread(ruta_proc("costo_mepco.csv"))[, .(u = as.IDate(date), c = `93_w/`)]
setorder(costo, u)
costo[, dc := c - shift(c)]
upd <- costo[!is.na(dc) & dc != 0 & u >= DESDE_ENTRADA & u <= FIN_COSTO, .(u, dc)]

# precio del último reporte de cada día, por estación
idn <- fread(ruta_proc("identidad_estaciones.csv"), select = c("id", "station_key"))
p <- fread(ruta_proc("precios.csv"), select = c("id", "price", "date", "time", "fuel"),
           colClasses = list(character = c("id", "fuel"), IDate = "date"))[fuel == "93"]
p <- idn[p, on = "id", nomatch = NULL][station_key %in% c(trat$station_key, ctrl$station_key)]
p[, time := as.ITime(time)]
setorder(p, station_key, date, time, na.last = TRUE)
p <- unique(p, by = c("station_key", "date"), fromLast = TRUE)[, .(station_key, date, price)]
setkey(p, station_key, date)

vigente <- function(sk, d) p[data.table(station_key = sk, date = d), on = .(station_key, date), roll = VIGENCIA, x.price]
cambio_log <- function(x) {   # x: station_key, u
  x[, `:=`(antes = vigente(station_key, u - 1L), despues = vigente(station_key, u + DIAS_DESPUES))]
  x[!is.na(antes) & !is.na(despues), .(station_key, u, dlp = 100 * (log(despues) - log(antes)))]
}

# tratadas: actualizaciones a +-K_SEMANAS de la entrada; k = semanas desde la entrada
tu <- trat[, .(u = upd$u), by = .(station_key, fecha_e, region)]
tu[, k := as.integer(u - fecha_e) %/% 7L]
tu <- tu[abs(as.integer(u - fecha_e)) <= 7L * K_SEMANAS + 6L & k >= -K_SEMANAS & k <= K_SEMANAS]
td <- cambio_log(tu[, .(station_key, u)])[tu, on = .(station_key, u), nomatch = NULL]

# controles de la misma región: cambio medio en cada actualización que usan las tratadas
cc <- cambio_log(CJ(station_key = ctrl$station_key, u = unique(td$u)))[ctrl, on = "station_key", nomatch = NULL]
cr <- cc[, .(dlp_cr = mean(dlp), n_cr = .N), by = .(u, region)][n_cr >= MIN_CTRL_REG]

td <- cr[td, on = .(u, region)][upd, on = "u", nomatch = NULL]
td[, dif_log_reg := dlp - dlp_cr]   # NA si la región tiene menos de MIN_CTRL_REG controles
setorder(td, station_key, u)

fwrite(td[, .(station_key, region, fecha_e, u, k, dc, dlp, dlp_cr, n_cr, dif_log_reg)],
       ruta_proc("traspaso_actualizaciones.csv"))
message(sprintf("05_traspaso.R: %d tratadas, %s tratada-actualizaciones", uniqueN(td$station_key),
                format(nrow(td), big.mark = ",")))
