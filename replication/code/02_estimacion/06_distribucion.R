# 06_distribucion.R
#
# Gráfico 8: regresión de distribución (Chernozhukov, Fernández-Val y Melly, 2013) del precio
# menos su media nacional del mes. Para cada umbral c de una grilla de NC cuantiles,
# kappa(c) = efecto de la entrada sobre 1[y <= c] con la especificación principal; la
# distribución sin entrada es la observada de las tratadas después de la entrada menos kappa(c).
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/grafico_08_distribucion_acumulada.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

NC <- 41L   # umbrales, entre los cuantiles 2 y 98

d <- muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal)
d[, post := as.integer(treated == 1L & ym >= g_entry)]

cdf <- rbindlist(lapply(names(PRECIOS), function(fv) {
  y <- paste0(fv, "_dev")
  z <- d[!is.na(get(y))]
  cs <- quantile(z[[y]], seq(0.02, 0.98, length.out = NC), names = FALSE)
  ind <- sprintf("ind%02d", seq_along(cs))
  z[, (ind) := lapply(cs, function(cc) as.numeric(get(y) <= cc))]
  m <- feols(as.formula(sprintf("c(%s) ~ post | %s", paste(ind, collapse = ", "), FE_PRINCIPAL)),
             data = z, cluster = ~comuna)
  kappa <- vapply(seq_along(ind), function(j) coef(m[[j]])[["post"]], numeric(1))
  trat_post <- z[treated == 1L & post == 1L][[y]]
  observada <- vapply(cs, function(cc) mean(trat_post <= cc), numeric(1))
  data.table(combustible = PRECIOS[[fv]], c = cs,
             serie = rep(c("Con entrada (observada)", "Sin entrada (contrafactual)"), each = NC),
             F = c(observada, observada - kappa))
}))
cdf[, combustible := factor(combustible, levels = PRECIOS)]

fig <- ggplot(cdf, aes(c, F, colour = serie, linetype = serie)) +
  geom_line() +
  facet_wrap(~combustible, scales = "free_x") +
  labs(x = "Precio menos su media nacional del mes ($/L)", y = "Distribución acumulada", colour = NULL, linetype = NULL)
guardar(fig, "grafico_08_distribucion_acumulada.pdf", alto = 3.2)
