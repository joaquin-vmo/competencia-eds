# 11_autopista.R
#
# Efecto de la entrada en estaciones a <= 100 m de la red vial clase 1 (autopistas y rutas
# nacionales) frente al resto, con Sun y Abraham en una sola regresión (un grupo por ubicación,
# cohortes_sunab()). Se excluyen las tratadas cuya entrante está sobre la red.
#   Tabla C.6: ATT en el resto, en autopista y la diferencia.
#   Gráfico B.6: estudio de eventos de cada grupo.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/tabla_C06_autopista.tex, outputs/grafico_B06_autopista.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

GRUPOS <- c(sprintf("Resto (más de %d m)", AUTOPISTA_M), sprintf("Autopista (hasta %d m)", AUTOPISTA_M))

# ---- Muestra: sin tratadas con entrante de autopista ----

d <- muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal)
message(sprintf("Tratadas fuera por entrante a %d m o menos de la red: %d de %d", AUTOPISTA_M,
                uniqueN(d[entrante_autopista == TRUE, station_key]), uniqueN(d[treated == 1L, station_key])))
d <- d[!(entrante_autopista %in% TRUE)]

d[, cerca := as.integer(d_autopista <= AUTOPISTA_M)]
d <- cohortes_sunab(d, "cerca")

# ---- Estimación por grupo y diferencia ----

res <- lapply(names(PRECIOS), function(fv) {
  x <- d[!is.na(get(fv))]
  m <- estimar_sa(x, fv)
  u <- x[obs(m)]
  list(es = agregar_sa(m, x)[, .(event_time, grupo = GRUPOS[grupo + 1L], estimate, se, combustible = PRECIOS[[fv]])],
       att = att_sa(m, x),
       n = c(resto = uniqueN(u[treated == 1L & cerca == 0L, station_key]), autopista = uniqueN(u[treated == 1L & cerca == 1L, station_key]),
             controles = uniqueN(u[treated == 0L, station_key]), comunas = uniqueN(u$comuna)))
})
att <- lapply(res, `[[`, "att")
n <- sapply(res, `[[`, "n")
print(rbindlist(lapply(seq_along(att), function(j) att[[j]][, fv := names(PRECIOS)[j]])))   # cifras que cita el texto

# ---- Tabla C.6 ----

fila_att <- function(nombre, ef) {
  r <- rbindlist(lapply(att, function(a) a[efecto == ef]))
  filas_coef(nombre, r$est, r$se, r$p)
}
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule",
                          fila_att(GRUPOS[1], "grupo0"), fila_att(GRUPOS[2], "grupo1"),
                          fila_att("Diferencia (autopista $-$ resto)", "dif1"), "\\midrule",
                          fila("Tratadas resto", n["resto", ]), fila("Tratadas autopista", n["autopista", ]),
                          fila("Controles", n["controles", ]), fila("Comunas", n["comunas", ])),
               "tabla_C06_autopista.tex")

# ---- Gráfico B.6 ----

es <- rbindlist(lapply(res, `[[`, "es"))
es[, `:=`(combustible = factor(combustible, levels = PRECIOS), grupo = factor(grupo, levels = GRUPOS))]
guardar(grafico_es(es, "grupo"), "grafico_B06_autopista.pdf", alto = 3.2)
