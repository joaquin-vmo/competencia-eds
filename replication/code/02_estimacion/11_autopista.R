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
                n_distinct(d$station_key[d$entrante_autopista %in% TRUE]), n_distinct(d$station_key[d$treated == 1L])))
d <- d |>
  filter(!(entrante_autopista %in% TRUE)) |>
  mutate(cerca = as.integer(d_autopista <= AUTOPISTA_M)) |>
  cohortes_sunab("cerca")

# ---- Estimación por grupo y diferencia ----

res <- map(names(PRECIOS), function(fv) {
  x <- filter(d, !is.na(.data[[fv]]))
  m <- estimar_sa(x, fv)
  u <- slice(x, obs(m))
  list(es = agregar_sa(m, x) |> transmute(event_time, grupo = GRUPOS[grupo + 1L], estimate, se, combustible = PRECIOS[[fv]]),
       att = att_sa(m, x),
       n = c(resto = n_distinct(u$station_key[u$treated == 1L & u$cerca == 0L]), autopista = n_distinct(u$station_key[u$treated == 1L & u$cerca == 1L]),
             controles = n_distinct(u$station_key[u$treated == 0L]), comunas = n_distinct(u$comuna)))
})
att <- map(res, "att")
n <- sapply(res, `[[`, "n")
map2(att, names(PRECIOS), \(a, f) mutate(a, fv = f)) |> list_rbind() |> print()   # cifras que cita el texto

# ---- Tabla C.6 ----

fila_att <- function(nombre, ef) {
  r <- list_rbind(map(att, \(a) filter(a, efecto == ef)))
  filas_coef(nombre, r$est, r$se, r$p)
}
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule",
                          fila_att(GRUPOS[1], "grupo0"), fila_att(GRUPOS[2], "grupo1"),
                          fila_att("Diferencia (autopista $-$ resto)", "dif1"), "\\midrule",
                          fila("Tratadas resto", n["resto", ]), fila("Tratadas autopista", n["autopista", ]),
                          fila("Controles", n["controles", ]), fila("Comunas", n["comunas", ])),
               "tabla_C06_autopista.tex")

# ---- Gráfico B.6 ----

es <- list_rbind(map(res, "es")) |>
  mutate(combustible = factor(combustible, levels = PRECIOS), grupo = factor(grupo, levels = GRUPOS))
guardar(grafico_es(es, "grupo"), "grafico_B06_autopista.pdf", alto = 3.2)
