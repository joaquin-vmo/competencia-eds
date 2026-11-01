# 05_comovimiento.R
#
# Respuesta de las rivales a un cambio de precio por distancia (ecuación comov): para cada
# combustible y muestra, MCO de sigue_24 e iguala sobre indicadores de anillo de distancia,
# con 5-10 km como referencia, efecto fijo de cambio líder y errores agrupados por la
# estación que cambia su precio.
#
# Entrada: data/processed/comovimiento_pares.csv.gz
# Salida:  outputs/grafico_07_comovimiento_semanal.pdf, outputs/grafico_B09_comovimiento_regimenes.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

COMB <- c(`93` = "Gasolina 93", `95` = "Gasolina 95", `97` = "Gasolina 97", DI = "Diésel")
Y <- c(sigue_24 = "Cambia en la misma dirección en 24 h", iguala = "Iguala el precio en 24 h")
ANILLOS <- c("0-1", "1-2", "2-3", "3-5")

pares <- fread(ruta_proc("comovimiento_pares.csv.gz"), colClasses = list(character = "fuel"))

res <- rbindlist(lapply(names(Y), function(y) {
  rbindlist(lapply(split(pares, by = c("fuel", "muestra")), function(z) {
    m <- feols(as.formula(sprintf("%s ~ i(dist, ref = '5-10') | ev", y)), z, cluster = ~i)
    ct <- coeftable(m)
    data.table(fuel = z$fuel[1], muestra = z$muestra[1], y = y, dist = sub("dist::", "", rownames(ct)),
               est = ct[, 1], se = ct[, 2], cambios = uniqueN(z$ev))
  }))
}))
res[, `:=`(dist = factor(dist, levels = ANILLOS), combustible = factor(COMB[fuel], levels = COMB),
           y = factor(Y[y], levels = Y))]

message("Cambios líderes por combustible y muestra (notas de los Gráficos 7 y B.9):")
print(dcast(unique(res[, .(combustible, muestra, cambios)]), combustible ~ muestra, value.var = "cambios"))

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
