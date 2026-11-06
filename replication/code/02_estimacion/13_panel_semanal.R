# 13_panel_semanal.R
#
# Gráfico B.8: estudio de eventos en panel mensual (especificación principal) y semanal, con el
# diseño de Fischer et al. (2025): semana de la entrada (primera semana con precio de la
# entrante), bins de 26 semanas, efecto fijo de semana y cohortes trimestrales en Sun y Abraham.
#
# Entrada: data/processed/panel_2012_2026/{panel_mensual.csv,panel_semanal.csv.gz}
# Salida:  outputs/grafico_B08_panel_semanal.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

SEM_BIN <- 26L                                                # semanas por bin
FUEL    <- c(p93 = "93", p95 = "95", p97 = "97", pdi = "di")  # códigos del panel semanal
FE_SEM  <- "station_key + wi + region^year + distribuidor^year"
SERIES  <- c("Mensual, TWFE", "Semanal, TWFE", "Semanal, Sun y Abraham")
focal   <- VENTANAS[["2012_2026"]]$focal

mensual <- muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, focal)

# solo las columnas que usa: el panel semanal tiene 4,6 millones de filas
semanal <- leer_panel("2012_2026", "panel_semanal.csv.gz",
                      select = c("station_key", "fuel", "wi", "ym", "price", "comuna", "region", "distribuidor",
                                 "role_entry", "g_entry", "g2_entry", "base", "entrante_semana"))[, miym := mi(ym)]
d <- muestra_estacion(semanal, CTRL_ESTRICTO, focal)   # misma muestra que el mensual, con filas semanales
rm(semanal)
stopifnot("tratada sin semana de entrada" = !anyNA(d[treated == 1L, entrante_semana]))
d[, rel := fifelse(treated == 1L, pmax(-NBIN, pmin(NBIN, (wi - entrante_semana) %/% SEM_BIN)), -1L)]
d[, semana_e := SEMANA_0 + entrante_semana * 7L]
d[, cohorte := fifelse(treated == 1L, year(semana_e) * 4L + (month(semana_e) - 1L) %/% 3L, 1000L)]   # trimestre
d[, rel_sa := fifelse(treated == 1L, rel, -1000L)]

# shortcut: saca de Sun y Abraham las cohortes con algún coeficiente de error estándar > 100
# veces la mediana (no identificadas por los efectos fijos); en la 95 es una cohorte de dos estaciones
sunab_identificado <- function(x, fml) {
  m <- feols(fml("sunab(cohorte, rel_sa, no_agg = TRUE)"), data = x, cluster = ~comuna)
  se <- coeftable(m)[, 2]
  malas <- unique(as.integer(sub(".*cohort::", "", names(se)[se > 100 * median(se)])))
  if (length(malas)) message("  sin identificar, fuera de Sun y Abraham: cohorte ", paste(malas, collapse = ", "))
  feols(fml("sunab(cohorte, rel_sa)"), data = x[!cohorte %in% malas], cluster = ~comuna)
}

res <- rbindlist(lapply(names(FUEL), function(fv) {
  message(PRECIOS[[fv]])
  x <- d[fuel == FUEL[[fv]] & !is.na(price)]
  fml <- function(rhs) as.formula(sprintf("log(price) * 100 ~ %s | %s", rhs, FE_SEM))
  rbind(tidy_es(estimar_es(mensual[!is.na(get(fv))], fv))[, serie := SERIES[1]],
        tidy_es(feols(fml("i(rel, treated, ref = -1)"), data = x, cluster = ~comuna))[, serie := SERIES[2]],
        tidy_es(sunab_identificado(x, fml), "rel_sa")[, serie := SERIES[3]])[, combustible := PRECIOS[[fv]]]
}))
res[, `:=`(combustible = factor(combustible, levels = PRECIOS), serie = factor(serie, levels = SERIES))]
print(res[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)], nrows = Inf)   # cifras que cita el texto
guardar(grafico_es(res, "serie", dodge = 0.6), "grafico_B08_panel_semanal.pdf", alto = 3.2)
