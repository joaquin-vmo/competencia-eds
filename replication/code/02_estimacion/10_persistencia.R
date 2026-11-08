# 10_persistencia.R
#
# Gráfico B.3: persistencia del tratamiento. Estudio de eventos de Sun y Abraham sobre el número de estaciones
# con registro a <= 2 km de la incumbente, incluyendo y excluyendo a la entrante, con y sin
# el corte de la muestra en la segunda entrada.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/grafico_B03_persistencia.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

CONTEOS  <- c(n_vecinas = "Incluye a la entrante", n_vecinas_previas = "Excluye a la entrante")
MUESTRAS <- c("Con corte en la segunda entrada", "Sin corte")

# ---- Estudio de eventos sobre el número de estaciones cercanas ----

panel <- leer_panel("2012_2026")
res <- rbindlist(lapply(MUESTRAS, function(mu) {
  x <- if (mu == MUESTRAS[1]) panel else copy(panel)[, g2_entry := NA]   # muestra_estacion() corta en g2_entry
  d <- cohortes_sunab(muestra_estacion(x, CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal))
  rbindlist(lapply(names(CONTEOS), function(v)
    tidy_es(feols(as.formula(sprintf("%s ~ sunab(cohorte, per) | %s", v, FE_PRINCIPAL)),
                  data = d, cluster = ~comuna), "per")[, `:=`(serie = CONTEOS[[v]], muestra = mu)]))
}))
res[, `:=`(serie = factor(serie, levels = CONTEOS), muestra = factor(muestra, levels = MUESTRAS))]

# ---- Gráfico B.3 ----

fig <- grafico_es(res, "muestra", facetas = ~serie, escalas = "fixed",
                  y = sprintf("Cambio en el número de estaciones a %d km", R_TRATADA)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = COLOR_REF)
print(res[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)], nrows = Inf)   # cifras que cita el texto
guardar(fig, "grafico_B03_persistencia.pdf", alto = 2.4)
