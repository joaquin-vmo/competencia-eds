# 05_comovimiento.R
#
# Respuesta de las rivales a un cambio de precio por distancia (ecuación comov): para cada
# combustible y muestra, MCO de sigue_24 e iguala sobre indicadores de anillo de distancia,
# con 5-10 km como referencia, efecto fijo de cambio líder y errores agrupados en dos dimensiones,
# por la estación que cambia su precio (i) y por la rival (j), porque los datos son diádicos y
# una misma estación aparece en muchos pares (Cameron, Gelbach y Miller, 2011).
#   Gráfico B.11: sigue_24 por separado para las alzas y las bajas del cambio líder.
#   Log: cifras agregadas que cita la robustez, juntando los cuatro combustibles: diferencia a
#     <= 2 km frente a 5-10 km por muestra (incluida la semana 1 del trisemanal) y por marca de la
#     líder con todos los días de domingo a martes de cada régimen.
#
# Entrada: data/processed/comovimiento_pares.csv.gz
# Salida:  outputs/grafico_07_comovimiento_semanal.pdf, outputs/grafico_B09_comovimiento_regimenes.pdf,
#          outputs/grafico_B11_comovimiento_direccion.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

COMB <- c(`93` = "Gasolina 93", `95` = "Gasolina 95", `97` = "Gasolina 97", DI = "Diésel")
Y <- c(sigue_24 = "Cambia en la misma dirección en 24 h", iguala = "Iguala el precio en 24 h")
ANILLOS <- c("0-1", "1-2", "2-3", "3-5")

todos <- leer_csv(ruta_proc("comovimiento_pares.csv.gz"), col_types = cols(fuel = "c"))
MUESTRAS <- c("Semanal (2022)", "Trisemanal, semanas 2 y 3 (2023)")   # las de los gráficos
pares <- filter(todos, muestra %in% MUESTRAS)

res <- map(names(Y), function(y) {
  pares |> nest(.by = c(fuel, muestra)) |> pmap(function(fuel, muestra, data) {
    m <- feols(as.formula(sprintf("%s ~ i(dist, ref = '5-10') | ev", y)), data, cluster = ~i + j)
    ct <- coeftable(m)
    tibble(fuel = fuel, muestra = muestra, y = y, dist = sub("dist::", "", rownames(ct)),
           est = ct[, 1], se = ct[, 2], cambios = n_distinct(data$ev))
  }) |> list_rbind()
}) |>
  list_rbind() |>
  mutate(dist = factor(dist, levels = ANILLOS), combustible = factor(COMB[fuel], levels = COMB),
         y = factor(Y[y], levels = Y))

message("Cambios líderes por combustible y muestra (notas de los Gráficos 7 y B.9):")
res |> distinct(combustible, muestra, cambios) |> pivot_wider(names_from = muestra, values_from = cambios) |>
  arrange(combustible) |> print()
res |> transmute(y, muestra, combustible, dist, est = round(est, 2), se = round(se, 2)) |> print(n = Inf)

grafico <- function(d, serie = NULL) {
  fig <- ggplot(d, aes(dist, est)) +
    geom_hline(yintercept = 0, colour = COLOR_REF) +
    facet_grid(y ~ combustible, scales = "free_y", labeller = label_wrap_gen(18)) +
    labs(x = "Distancia a la estación que cambia su precio (km)", y = "Respecto de 5-10 km (pp)",
         colour = NULL, shape = NULL)
  if (is.null(serie)) return(fig + geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), size = 0.2))
  fig + geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se, colour = .data[[serie]], shape = .data[[serie]]),
                        position = position_dodge(width = 0.45), size = 0.2)
}

guardar(grafico(filter(res, muestra == "Semanal (2022)")), "grafico_07_comovimiento_semanal.pdf", alto = 4)
guardar(grafico(res, "muestra"), "grafico_B09_comovimiento_regimenes.pdf", alto = 4)

# ---- Gráfico B.11: alzas y bajas ----

DIRECCION <- c(`1` = "Alzas", `-1` = "Bajas")
dir <- pares |> nest(.by = c(fuel, muestra, sgn)) |> pmap(function(fuel, muestra, sgn, data) {
  m <- feols(sigue_24 ~ i(dist, ref = '5-10') | ev, data, cluster = ~i + j)
  ct <- coeftable(m)
  tibble(fuel = fuel, muestra = muestra, direccion = DIRECCION[[as.character(sgn)]],
         dist = sub("dist::", "", rownames(ct)), est = ct[, 1], se = ct[, 2], cambios = n_distinct(data$ev),
         base = mean(data$sigue_24[data$dist == "5-10"]))
}) |>
  list_rbind() |>
  mutate(dist = factor(dist, levels = ANILLOS), combustible = factor(COMB[fuel], levels = COMB),
         direccion = factor(direccion, levels = DIRECCION))
message("Alzas y bajas: cambios líderes y probabilidad base a 5-10 km (Gráfico B.11):")
dir |> distinct(combustible, muestra, direccion, cambios, base = round(base, 2)) |>
  pivot_wider(id_cols = combustible, names_from = c(muestra, direccion), values_from = c(cambios, base)) |>
  arrange(combustible) |> print()
dir |> filter(dist %in% c("0-1", "1-2", "2-3")) |>
  transmute(combustible, muestra, direccion, dist, est = round(est, 2), se = round(se, 2)) |> print(n = Inf)
fig <- ggplot(dir, aes(dist, est, colour = direccion, shape = direccion)) +
  geom_hline(yintercept = 0, colour = COLOR_REF) +
  geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), position = position_dodge(width = 0.45), size = 0.2) +
  facet_grid(muestra ~ combustible, scales = "free_y", labeller = label_wrap_gen(18)) +
  labs(x = "Distancia a la estación que cambia su precio (km)", y = "Respecto de 5-10 km (pp)", colour = NULL, shape = NULL)
guardar(fig, "grafico_B11_comovimiento_direccion.pdf", alto = 4)

# ---- Cifras agregadas (robustez) ----

todos <- mutate(todos, d2 = if_else(dist %in% c("0-1", "1-2"), "0-2", as.character(dist)))
agregada <- function(z) {
  ct <- coeftable(feols(sigue_24 ~ i(d2, ref = '5-10') | ev, z, cluster = ~i + j))["d2::0-2", ]
  tibble(dif = ct[[1]], se = ct[[2]], p = ct[[4]], base = mean(z$sigue_24[z$dist == "5-10"]), cambios = n_distinct(z$ev))
}
redondear <- function(d) mutate(d, across(where(is.numeric), \(v) round(v, 3)))
message("Diferencia a <= 2 km frente a 5-10 km, cuatro combustibles juntos, por muestra:")
todos |> group_by(muestra) |> group_modify(\(z, k) agregada(z)) |> ungroup() |> redondear() |> print()
message("Por marca de la líder, todos los días de domingo a martes de cada régimen:")
todos |> group_by(reg, marca_i) |> group_modify(\(z, k) agregada(z)) |> ungroup() |> arrange(reg, marca_i) |> redondear() |> print()
