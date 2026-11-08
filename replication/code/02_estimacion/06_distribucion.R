# 06_distribucion.R
#
# Gráfico 8: regresión de distribución (Chernozhukov, Fernández-Val y Melly, 2013) del precio
# menos su media nacional del mes. Para cada umbral c de una grilla de NC cuantiles,
# kappa(c) = ATT de Sun y Abraham (cohortes anuales) sobre 1[y <= c] con los efectos fijos principales; la
# distribución sin entrada es la observada de las tratadas después de la entrada menos kappa(c).
# Los umbrales son cuantiles de la muestra conjunta (tratadas y controles). El log reporta
# kappa(c) con su error agrupado por comuna (puntual, sin bandas uniformes) en los umbrales
# más cercanos a los cuantiles 10, 25, 50, 75 y 90.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/grafico_08_distribucion_acumulada.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

NC <- 41L   # umbrales, entre los cuantiles 2 y 98

# ---- Muestra ----

d <- cohortes_sunab(muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal)) |>
  mutate(post = as.integer(treated == 1L & ym >= g_entry))

# ---- Regresión de distribución por umbral ----

cdf <- map(names(PRECIOS), function(fv) {
  y <- paste0(fv, "_dev")
  z <- filter(d, !is.na(.data[[y]]))
  cs <- quantile(z[[y]], seq(0.02, 0.98, length.out = NC), names = FALSE)
  ind <- sprintf("ind%02d", seq_along(cs))
  z <- bind_cols(z, as_tibble(map(set_names(cs, ind), \(cc) as.numeric(z[[y]] <= cc))))
  m <- feols(as.formula(sprintf("c(%s) ~ sunab(cohorte, per) | %s", paste(ind, collapse = ", "), FE_PRINCIPAL)),
             data = z, cluster = ~comuna)
  a <- map(seq_along(ind), \(j) att_sa(m[[j]], z)) |> list_rbind()
  kappa <- a$est; se <- a$se
  q <- seq(0.02, 0.98, length.out = NC)
  sel <- map_int(c(0.10, 0.25, 0.50, 0.75, 0.90), \(v) which.min(abs(q - v)))
  message(PRECIOS[[fv]], ": kappa (pp) en los cuantiles ", paste(sprintf("%.3f", q[sel]), collapse = "/"), " = ",
          paste(sprintf("%.1f (%.1f)", 100 * kappa[sel], 100 * se[sel]), collapse = "; "),
          sprintf("; máximo %.1f en el cuantil %.3f", 100 * max(kappa), q[which.max(kappa)]))
  trat_post <- z[[y]][z$treated == 1L & z$post == 1L]
  observada <- map_dbl(cs, \(cc) mean(trat_post <= cc))
  tibble(combustible = PRECIOS[[fv]], c = rep(cs, 2),
         serie = rep(c("Con entrada (observada)", "Sin entrada (contrafactual)"), each = NC),
         F = c(observada, observada - kappa))
}) |>
  list_rbind() |>
  mutate(combustible = factor(combustible, levels = PRECIOS))

# ---- Gráfico 8: distribución acumulada observada y contrafactual ----

fig <- ggplot(cdf, aes(c, F, colour = serie, linetype = serie)) +
  geom_line() +
  facet_wrap(~combustible, scales = "free_x") +
  labs(x = "Precio menos su media nacional del mes ($/L)", y = "Distribución acumulada", colour = NULL, linetype = NULL)
guardar(fig, "grafico_08_distribucion_acumulada.pdf", alto = 3.2)
