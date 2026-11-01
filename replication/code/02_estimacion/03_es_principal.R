# 03_es_principal.R
#
# Estudio de eventos principal, ventana 2012_2026.
#   Tablas 4, C.3 y C.4: control estricto, especificaciones de efectos fijos de menos a más
#     exigente (93, 97 y diésel; la tesis no reporta la tabla de la 95).
#   Tabla C.5: especificación principal con control amplio y estricto, 93, 97 y diésel.
#   Gráfico 5: TWFE y Sun y Abraham (2021) con la misma muestra, efectos fijos y bins.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/tabla_04_es_efectos_fijos_p93.tex, outputs/tabla_C03_es_efectos_fijos_p97.tex,
#          outputs/tabla_C04_es_efectos_fijos_pdi.tex, outputs/tabla_C05_es_controles.tex,
#          outputs/grafico_05_es_sunab.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

V <- VENTANAS[["2012_2026"]]
panel <- leer_panel("2012_2026")
estricto <- muestra_estacion(panel, CTRL_ESTRICTO, V$focal)
amplio   <- muestra_estacion(panel, CTRL_AMPLIO, V$focal)

TABLA_EF        <- c(p93 = "tabla_04_es_efectos_fijos_p93.tex", p97 = "tabla_C03_es_efectos_fijos_p97.tex",
                     pdi = "tabla_C04_es_efectos_fijos_pdi.tex")

# ---- Tablas 4, C.3 y C.4: escalera de efectos fijos; sin efecto fijo de estación va el indicador de tratada ----

ESCALERA <- list(
  list(rhs = " + treated", fe = ""),
  list(rhs = " + treated", fe = " | ym"),
  list(rhs = "",           fe = " | ym + station_key"),
  list(rhs = "",           fe = " | ym + station_key + region^year"),
  list(rhs = "",           fe = paste(" |", FE_PRINCIPAL))
)

for (fv in names(TABLA_EF)) {
  x <- estricto[!is.na(get(fv))]
  modelos <- lapply(ESCALERA, function(e) feols(
    as.formula(sprintf("log(%s) * 100 ~ i(rel, treated, ref = -1)%s%s", fv, e$rhs, e$fe)),
    data = x, cluster = ~comuna))
  n <- sapply(modelos, n_estaciones, d = x)
  etable(modelos, tex = TRUE, drop = "Constant", replace = TRUE,
         extralines = list(Tratadas = n[1, ], Controles = n[2, ]), file = ruta_out(TABLA_EF[[fv]]))
}

# ---- Tabla C.5: control amplio (> 3 km) y estricto (> 5 km), dos columnas por combustible ----

COMB_CONTROLES <- c(p93 = "Gasolina 93", p97 = "Gasolina 97", pdi = "Diésel")
datos <- unlist(lapply(names(COMB_CONTROLES), function(fv) list(amplio[!is.na(get(fv))], estricto[!is.na(get(fv))])),
                recursive = FALSE)
modelos <- mapply(estimar_es, datos, rep(names(COMB_CONTROLES), each = 2), SIMPLIFY = FALSE)
n <- mapply(n_estaciones, modelos, datos)
etable(modelos, tex = TRUE, replace = TRUE,
       headers = list(as.list(setNames(rep(2, length(COMB_CONTROLES)), COMB_CONTROLES)),
                      rep(c("Amplio", "Estricto"), length(COMB_CONTROLES))),
       extralines = list(Tratadas = n[1, ], Controles = n[2, ]), file = ruta_out("tabla_C05_es_controles.tex"))

# ---- Gráfico 5: TWFE y Sun y Abraham ----

ESTIMADORES <- c("TWFE", "Sun y Abraham")
x <- cohortes_sunab(estricto)
res <- rbindlist(lapply(names(PRECIOS), function(fv) {
  z <- x[!is.na(get(fv))]
  tw <- estimar_es(z, fv)
  message(sprintf("Gráfico 5, %s: %d tratadas y %d controles", PRECIOS[[fv]], n_estaciones(tw, z)[1], n_estaciones(tw, z)[2]))
  rbind(tidy_es(tw)[, estimador := ESTIMADORES[1]],
        tidy_es(estimar_es(z, fv, "sunab(cohorte, per)"), "per")[, estimador := ESTIMADORES[2]])[
    , combustible := PRECIOS[[fv]]]
}))
res[, `:=`(combustible = factor(combustible, levels = PRECIOS), estimador = factor(estimador, levels = ESTIMADORES))]
guardar(grafico_es(res, "estimador"), "grafico_05_es_sunab.pdf", alto = 3.2)
