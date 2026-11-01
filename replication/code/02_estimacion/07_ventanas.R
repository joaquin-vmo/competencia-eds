# 07_ventanas.R
#
# Estudio de eventos principal en las ventanas antes (2014_2019) y después (2021_2026) de la
# pandemia, TWFE y Sun y Abraham (2021) con la misma muestra, efectos fijos y bins.
#
# Entrada: data/processed/panel_<ventana>/panel_mensual.csv
# Salida:  outputs/grafico_09_ventana_2014_2019.pdf, outputs/grafico_10_ventana_2021_2026.pdf,
#          outputs/tabla_C09_ventana_2014_2019.tex, outputs/tabla_C10_ventana_2021_2026.tex

source(here::here("code", "00_configuracion.R"))
library(fixest)

SALIDAS <- list("2014_2019" = c(grafico = "grafico_09_ventana_2014_2019.pdf", tabla = "tabla_C09_ventana_2014_2019.tex"),
                "2021_2026" = c(grafico = "grafico_10_ventana_2021_2026.pdf", tabla = "tabla_C10_ventana_2021_2026.tex"))
ESTIMADORES <- c("TWFE", "Sun y Abraham")

for (ventana in names(SALIDAS)) {
  d <- cohortes_sunab(muestra_estacion(leer_panel(ventana), CTRL_ESTRICTO, VENTANAS[[ventana]]$focal))

  res <- lapply(names(PRECIOS), function(fv) {
    x <- d[!is.na(get(fv))]
    tw <- estimar_es(x, fv)
    list(es = rbind(tidy_es(tw)[, estimador := ESTIMADORES[1]],
                    tidy_es(estimar_es(x, fv, "sunab(cohorte, per)"), "per")[, estimador := ESTIMADORES[2]])[
                      , combustible := PRECIOS[[fv]]],
         n = c(n_estaciones(tw, x), nobs(tw)))
  })
  es <- rbindlist(lapply(res, `[[`, "es"))
  n <- sapply(res, `[[`, "n")

  # tabla: filas = bin, columnas = combustible x estimador
  cuerpo <- unlist(lapply(setdiff(-NBIN:NBIN, -1L), function(k) {
    r <- es[event_time == k][order(match(combustible, PRECIOS), match(estimador, ESTIMADORES))]
    filas_coef(DICT[[sprintf("rel::%d:treated", k)]], r$estimate, r$se, r$p)
  }))
  dup <- function(v) rep(v, each = 2)
  escribir_tabla(sprintf("l%s", strrep("c", 2 * length(PRECIOS))),
                 c(fila("", sprintf("\\multicolumn{2}{c}{%s}", PRECIOS)),
                   paste(sprintf("\\cmidrule(lr){%d-%d}", seq(2, 8, 2), seq(3, 9, 2)), collapse = " "),
                   fila("", rep(c("TWFE", "SA"), length(PRECIOS))), "\\midrule",
                   cuerpo, "\\midrule",
                   fila("Tratadas", dup(n[1, ])), fila("Controles", dup(n[2, ])),
                   fila("Observaciones", dup(format(n[3, ], big.mark = ",")))),
                 SALIDAS[[ventana]][["tabla"]])

  es[, `:=`(combustible = factor(combustible, levels = PRECIOS), estimador = factor(estimador, levels = ESTIMADORES))]
  guardar(grafico_es(es, "estimador"), SALIDAS[[ventana]][["grafico"]], alto = 3.2)
  message(sprintf("Ventana %s: tratadas %s, controles %s", ventana, paste(n[1, ], collapse = "/"), paste(n[2, ], collapse = "/")))
}
