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
d <- muestra_estacion(panel, CTRL_ESTRICTO, V$focal) |> filter(!is.na(p93))

# ---- Convergencia al precio de la entrante ----

dev <- panel |> select(station_key, miym, dev = p93_dev) |> filter(!is.na(dev))
ventana <- function(x) {
  x |>
    summarise(antes = mean(dev[between(k, -6L, -1L)]), despues = mean(dev[between(k, 6L, 12L)]),
              .by = c(station_key, comuna, g)) |>
    filter(is.finite(antes), is.finite(despues)) |>
    mutate(cambio = despues - antes)
}

tr <- d |> filter(treated == 1L) |> transmute(station_key, entrante_key, comuna, g = mi(g_evento)) |> distinct()
x <- tr |> left_join(dev, by = "station_key") |> mutate(k = miym - g)
inc <- ventana(x)
pe <- tr |> inner_join(dev, by = c(entrante_key = "station_key")) |> mutate(k = miym - g)   # station_key: la incumbente
inc <- inc |>
  inner_join(pe |> filter(between(k, 0L, 1L)) |> summarise(entrante = mean(dev), .by = station_key), by = "station_key")

# controles: cada fecha de entrada de las tratadas como fecha ficticia
ct <- d |> filter(treated == 0L) |> distinct(station_key, comuna)
y <- ct |>
  inner_join(dev, by = "station_key") |>
  cross_join(tibble(g = unique(tr$g))) |>
  mutate(k = miym - g) |>
  filter(between(k, -6L, 12L)) |>
  ventana()

message("Desvío medio de la media nacional ($/L), meses desde la entrada: incumbente y entrante")
x |> filter(between(k, -6L, 12L)) |> summarise(incumbente = round(mean(dev), 2), .by = k) |> arrange(k) |>
  left_join(pe |> filter(between(k, 0L, 12L)) |> summarise(entrante = round(mean(dev), 2), .by = k), by = "k") |>
  print(n = Inf)
modelos <- list(feols(cambio ~ antes, inc, cluster = ~comuna),
                feols(cambio ~ antes + entrante, inc, cluster = ~comuna),
                feols(cambio ~ antes, y, cluster = ~comuna))
stopifnot(nobs(modelos[[1]]) == nobs(modelos[[2]]))
etable_tex(modelos, headers = c("Tratadas", "Tratadas", "Controles"), replace = TRUE, fitstat = ~n + r2,
           dict = c(antes = "Desvío de la incumbente, meses $-6$ a $-1$", entrante = "Desvío de la entrante, meses 0 y 1",
                    `(Intercept)` = "Constante"),
           extralines = list(Estaciones = c(nrow(inc), nrow(inc), n_distinct(y$station_key))),
           archivo = "tabla_C13_convergencia.tex")
# terciles del desvío de la entrante, que no contiene el precio de la incumbente
inc <- mutate(inc, terc = ceiling(3 * rank(entrante, ties.method = "first") / n()))
message("Por tercil del desvío de la entrante: n, desvío de la entrante, desvío previo y cambio de la incumbente")
inc |> summarise(n = n(), entrante = round(mean(entrante), 2), antes = round(mean(antes), 2), cambio = round(mean(cambio), 2), .by = terc) |>
  arrange(terc) |> print()
