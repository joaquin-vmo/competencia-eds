# 04_atenuacion.R
#
# Gráfico 6: efecto estático por anillo de distancia (0-1, ..., 4-5 km) a la primera entrada
# competidora a <= 5 km. Las tratadas aportan los BIN_M meses antes y después de la entrada;
# los controles (sin entradas a <= 5 km) aportan todos sus meses. Especificación principal.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/grafico_06_atenuacion_distancia.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

ANILLOS <- c("0-1 km", "1-2 km", "2-3 km", "3-4 km", "4-5 km")

d <- muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal, radio5 = TRUE)
d <- d[treated == 0L | (miym - mi(g_evento)) %between% c(-BIN_M, BIN_M - 1L)]
d[, tpr := factor(fifelse(treated == 1L & ym >= g_evento, ring_entry, "none"), levels = c("none", ANILLOS))]

res <- rbindlist(lapply(names(PRECIOS), function(fv) {
  x <- d[!is.na(get(fv))]
  m <- estimar_es(x, fv, "i(tpr, ref = 'none')")
  u <- x[obs(m)]
  message(sprintf("Gráfico 6, %s: tratadas por anillo %s; %d controles", PRECIOS[[fv]],
                  paste(u[treated == 1L, uniqueN(station_key), keyby = ring_entry][ANILLOS, V1], collapse = "/"),
                  n_estaciones(m, x)[2]))
  ct <- coeftable(m)
  data.table(anillo = factor(sub("^tpr::", "", rownames(ct)), levels = ANILLOS), estimate = ct[, 1], se = ct[, 2],
             combustible = factor(PRECIOS[[fv]], levels = PRECIOS))
}))

fig <- ggplot(res, aes(anillo, estimate)) +
  geom_hline(yintercept = 0, colour = COLOR_REF) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), size = 0.3) +
  facet_wrap(~combustible, scales = "free_y") +
  labs(x = "Distancia a la entrada", y = "Efecto sobre el precio (%)")
guardar(fig, "grafico_06_atenuacion_distancia.pdf", alto = 3.2)
