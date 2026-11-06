# 12_errores_conley.R
#
# Gráfico B.7: especificación principal con errores agrupados por comuna y de Conley (1999)
# con kernel uniforme y corte de 2 y 5 km.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/grafico_B07_conley.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

VCOV <- list(Comuna = ~comuna,
             `Conley 2 km` = conley(R_TRATADA, distance = "spherical"),
             `Conley 5 km` = conley(R_CTRL_EST, distance = "spherical"))

d <- muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal)
res <- rbindlist(lapply(names(PRECIOS), function(fv) {
  m <- estimar_es(d[!is.na(get(fv))], fv)
  rbindlist(lapply(names(VCOV), function(v) tidy_es(summary(m, vcov = VCOV[[v]]))[, errores := v]))[
    , combustible := PRECIOS[[fv]]]
}))
res[, `:=`(combustible = factor(combustible, levels = PRECIOS), errores = factor(errores, levels = names(VCOV)))]
print(res[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)], nrows = Inf)   # cifras que cita el texto
guardar(grafico_es(res, "errores", dodge = 0.5), "grafico_B07_conley.pdf", alto = 3.2)
