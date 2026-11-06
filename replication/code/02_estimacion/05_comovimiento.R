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

todos <- fread(ruta_proc("comovimiento_pares.csv.gz"), colClasses = list(character = "fuel"))
MUESTRAS <- c("Semanal (2022)", "Trisemanal, semanas 2 y 3 (2023)")   # las de los gráficos
pares <- todos[muestra %in% MUESTRAS]

res <- rbindlist(lapply(names(Y), function(y) {
  rbindlist(lapply(split(pares, by = c("fuel", "muestra")), function(z) {
    m <- feols(as.formula(sprintf("%s ~ i(dist, ref = '5-10') | ev", y)), z, cluster = ~i + j)
    ct <- coeftable(m)
    data.table(fuel = z$fuel[1], muestra = z$muestra[1], y = y, dist = sub("dist::", "", rownames(ct)),
               est = ct[, 1], se = ct[, 2], cambios = uniqueN(z$ev))
  }))
}))
res[, `:=`(dist = factor(dist, levels = ANILLOS), combustible = factor(COMB[fuel], levels = COMB),
           y = factor(Y[y], levels = Y))]

message("Cambios líderes por combustible y muestra (notas de los Gráficos 7 y B.9):")
print(dcast(unique(res[, .(combustible, muestra, cambios)]), combustible ~ muestra, value.var = "cambios"))
print(res[, .(y, muestra, combustible, dist, est = round(est, 2), se = round(se, 2))])

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

guardar(grafico(res[muestra == "Semanal (2022)"]), "grafico_07_comovimiento_semanal.pdf", alto = 4)
guardar(grafico(res, "muestra"), "grafico_B09_comovimiento_regimenes.pdf", alto = 4)

# ---- Gráfico B.11: alzas y bajas ----

DIRECCION <- c(`1` = "Alzas", `-1` = "Bajas")
dir <- rbindlist(lapply(split(pares, by = c("fuel", "muestra", "sgn")), function(z) {
  m <- feols(sigue_24 ~ i(dist, ref = '5-10') | ev, z, cluster = ~i + j)
  ct <- coeftable(m)
  data.table(fuel = z$fuel[1], muestra = z$muestra[1], direccion = DIRECCION[[as.character(z$sgn[1])]],
             dist = sub("dist::", "", rownames(ct)), est = ct[, 1], se = ct[, 2], cambios = uniqueN(z$ev),
             base = z[dist == "5-10", mean(sigue_24)])
}))
dir[, `:=`(dist = factor(dist, levels = ANILLOS), combustible = factor(COMB[fuel], levels = COMB),
           direccion = factor(direccion, levels = DIRECCION))]
message("Alzas y bajas: cambios líderes y probabilidad base a 5-10 km (Gráfico B.11):")
print(dcast(unique(dir[, .(combustible, muestra, direccion, cambios, base = round(base, 2))]),
            combustible ~ muestra + direccion, value.var = c("cambios", "base")))
print(dir[dist %in% c("0-1", "1-2", "2-3"), .(combustible, muestra, direccion, dist, est = round(est, 2), se = round(se, 2))])
fig <- ggplot(dir, aes(dist, est, colour = direccion, shape = direccion)) +
  geom_hline(yintercept = 0, colour = COLOR_REF) +
  geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), position = position_dodge(width = 0.45), size = 0.2) +
  facet_grid(muestra ~ combustible, scales = "free_y", labeller = label_wrap_gen(18)) +
  labs(x = "Distancia a la estación que cambia su precio (km)", y = "Respecto de 5-10 km (pp)", colour = NULL, shape = NULL)
guardar(fig, "grafico_B11_comovimiento_direccion.pdf", alto = 4)

# ---- Cifras agregadas (robustez) ----

todos[, d2 := fifelse(dist %in% c("0-1", "1-2"), "0-2", as.character(dist))]
agregada <- function(z) {
  ct <- coeftable(feols(sigue_24 ~ i(d2, ref = '5-10') | ev, z, cluster = ~i + j))["d2::0-2", ]
  data.table(dif = ct[[1]], se = ct[[2]], p = ct[[4]], base = z[dist == "5-10", mean(sigue_24)], cambios = uniqueN(z$ev))
}
message("Diferencia a <= 2 km frente a 5-10 km, cuatro combustibles juntos, por muestra:")
print(todos[, agregada(.SD), by = muestra][, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
message("Por marca de la líder, todos los días de domingo a martes de cada régimen:")
print(todos[, agregada(.SD), by = .(reg, marca_i)][order(reg, marca_i)][, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
