# 16_mecanismo.R
#
# Mecanismo, gasolina 93, muestra principal (tratadas a <= 2 km, controles estrictos).
#   Convergencia al precio de la entrante (Tabla C.13). Desvíos de la media nacional ($/L):
#      cambio de la incumbente entre los meses -6 a -1 y 6 a 12 desde la entrada, sobre su propio
#      desvío previo (reversión a la media) y el de la entrante en los meses 0 y 1. Los controles
#      con las mismas fechas de entrada ficticias dan la reversión a la media sin entrada.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/tabla_C13_convergencia.tex

source(here::here("code", "00_configuracion.R"))
library(fixest)

V <- VENTANAS[["2012_2026"]]
panel <- leer_panel("2012_2026")
d <- muestra_estacion(panel, CTRL_ESTRICTO, V$focal)[!is.na(p93)]

# ---- Convergencia al precio de la entrante ----

dev <- panel[, .(station_key, miym, dev = p93_dev)][!is.na(dev)]
ventana <- function(x) x[, .(antes = mean(dev[k %between% c(-6L, -1L)]), despues = mean(dev[k %between% c(6L, 12L)])),
                         by = .(station_key, comuna, g)][is.finite(antes) & is.finite(despues)][, cambio := despues - antes][]

tr <- unique(d[treated == 1L, .(station_key, entrante_key, comuna, g = mi(g_evento))])
x <- dev[tr, on = "station_key", allow.cartesian = TRUE][, k := miym - g]
inc <- ventana(x)
pe <- dev[tr, on = c(station_key = "entrante_key"), nomatch = NULL][, k := miym - g]   # i.station_key: la incumbente
inc <- pe[k %between% c(0L, 1L), .(entrante = mean(dev)), by = .(station_key = i.station_key)][inc, on = "station_key", nomatch = NULL]

# controles: cada fecha de entrada de las tratadas como fecha ficticia
ct <- unique(d[treated == 0L, .(station_key, comuna)])
y <- dev[ct, on = "station_key", nomatch = NULL][, .(g = unique(tr$g)), by = .(station_key, comuna, miym, dev)]
y <- ventana(y[, k := miym - g][k %between% c(-6L, 12L)])

message("Desvío medio de la media nacional ($/L), meses desde la entrada: incumbente y entrante")
print(merge(x[k %between% c(-6L, 12L), .(incumbente = round(mean(dev), 2)), keyby = k],
            pe[k %between% c(0L, 12L), .(entrante = round(mean(dev), 2)), keyby = k], all.x = TRUE))
modelos <- list(feols(cambio ~ antes, inc, cluster = ~comuna),
                feols(cambio ~ antes + entrante, inc, cluster = ~comuna),
                feols(cambio ~ antes, y, cluster = ~comuna))
stopifnot(nobs(modelos[[1]]) == nobs(modelos[[2]]))
etable_tex(modelos, headers = c("Tratadas", "Tratadas", "Controles"), replace = TRUE, fitstat = ~n + r2,
           dict = c(antes = "Desvío de la incumbente, meses $-6$ a $-1$", entrante = "Desvío de la entrante, meses 0 y 1",
                    `(Intercept)` = "Constante"),
           extralines = list(Estaciones = c(nrow(inc), nrow(inc), uniqueN(y$station_key))),
           archivo = "tabla_C13_convergencia.tex")
# terciles del desvío de la entrante, que no contiene el precio de la incumbente
inc[, terc := ceiling(3 * frank(entrante, ties.method = "first") / .N)]
message("Por tercil del desvío de la entrante: n, desvío de la entrante, desvío previo y cambio de la incumbente")
print(inc[, .(n = .N, entrante = round(mean(entrante), 2), antes = round(mean(antes), 2), cambio = round(mean(cambio), 2)), keyby = terc])
