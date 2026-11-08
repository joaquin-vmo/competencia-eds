# 13_panel_semanal.R
#
# Gráfico B.8: estudio de eventos en panel mensual (especificación principal) y semanal, con una
# resolución más cercana a la de Fischer et al. (2025), que usan precios diarios: semana de la entrada (primera semana con precio de la
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

# ---- Muestras mensual y semanal ----

mensual <- muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, focal)

# solo las columnas que usa: el panel semanal tiene 4,6 millones de filas
semanal <- leer_panel("2012_2026", "panel_semanal.csv.gz",
                      select = c("station_key", "fuel", "wi", "ym", "price", "comuna", "region", "distribuidor",
                                 "role_entry", "g_entry", "g2_entry", "base", "entrante_semana")) |>
  mutate(miym = mi(ym))
d <- muestra_estacion(semanal, CTRL_ESTRICTO, focal)   # misma muestra que el mensual, con filas semanales
rm(semanal)
stopifnot("tratada sin semana de entrada" = !anyNA(d$entrante_semana[d$treated == 1L]))
d <- d |>
  mutate(rel = if_else(treated == 1L, pmax(-NBIN, pmin(NBIN, (wi - entrante_semana) %/% SEM_BIN)), -1L),
         semana_e = SEMANA_0 + entrante_semana * 7L) |>
  mutate(cohorte = if_else(treated == 1L, year(semana_e) * 4L + (month(semana_e) - 1L) %/% 3L, 1000L),   # trimestre
         rel_sa = if_else(treated == 1L, rel, -1000L))

# ---- Estimación: mensual TWFE, semanal TWFE y semanal Sun y Abraham ----

# shortcut: saca de Sun y Abraham las cohortes con algún coeficiente de error estándar > 100
# veces la mediana (no identificadas por los efectos fijos); en la 95 y el diésel, la cohorte 8097 (segundo trimestre de 2024)
sunab_identificado <- function(x, fml) {
  m <- feols(fml("sunab(cohorte, rel_sa, no_agg = TRUE)"), data = x, cluster = ~comuna)
  se <- coeftable(m)[, 2]
  malas <- unique(as.integer(sub(".*cohort::", "", names(se)[se > 100 * median(se)])))
  if (length(malas)) message("  sin identificar, fuera de Sun y Abraham: cohorte ", paste(malas, collapse = ", "))
  feols(fml("sunab(cohorte, rel_sa)"), data = filter(x, !cohorte %in% malas), cluster = ~comuna)
}

res <- map(names(FUEL), function(fv) {
  message(PRECIOS[[fv]])
  x <- filter(d, fuel == FUEL[[fv]], !is.na(price))
  fml <- function(rhs) as.formula(sprintf("log(price) * 100 ~ %s | %s", rhs, FE_SEM))
  bind_rows(tidy_es(estimar_es(filter(mensual, !is.na(.data[[fv]])), fv)) |> mutate(serie = SERIES[1]),
            tidy_es(feols(fml("i(rel, treated, ref = -1)"), data = x, cluster = ~comuna)) |> mutate(serie = SERIES[2]),
            tidy_es(sunab_identificado(x, fml), "rel_sa") |> mutate(serie = SERIES[3])) |>
    mutate(combustible = PRECIOS[[fv]])
}) |>
  list_rbind() |>
  mutate(combustible = factor(combustible, levels = PRECIOS), serie = factor(serie, levels = SERIES))
res |> mutate(across(where(is.numeric), \(v) round(v, 3))) |> print(n = Inf)   # cifras que cita el texto
# ---- Gráfico B.8 ----

guardar(grafico_es(res, "serie", dodge = 0.6), "grafico_B08_panel_semanal.pdf", alto = 3.2)
