# 03_es_principal.R
#
# Estudio de eventos principal, ventana 2012_2026.
#   Tablas 4, C.3 y C.4: control estricto, especificaciones de efectos fijos de menos a más
#     exigente (93, 97 y diésel; la tesis no reporta la tabla de la 95).
#   Tabla C.5: especificación principal con control amplio y estricto, 93, 97 y diésel.
#   Gráfico 5: TWFE y Sun y Abraham (2021) con la misma muestra, efectos fijos y bins.
#   Tabla 5: número principal. ATT estático (Post = desde el mes de la entrada) por combustible,
#     TWFE y Sun y Abraham, en % del precio, en $/L y en % del margen, con dos denominadores:
#     - margen bruto de comercialización de la CNE (estructura de precios, Región Metropolitana),
#       promedio 2014-2021, los años de la muestra con estructura publicada; es el del Gráfico 1.
#     - margen sobre el costo MEPCO (precio - costo mayorista con MEPCO) de las tratadas en los
#       12 meses previos a la entrada, la definición de Alemania, México y California.
#     $/L = ATT (%) x precio medio de las tratadas en los 12 meses previos a la entrada.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv, data/processed/estructura_precios.csv
# Salida:  outputs/tabla_04_es_efectos_fijos_p93.tex, outputs/tabla_C03_es_efectos_fijos_p97.tex,
#          outputs/tabla_C04_es_efectos_fijos_pdi.tex, outputs/tabla_C05_es_controles.tex,
#          outputs/grafico_05_es_sunab.pdf, outputs/tabla_05_att_principal.tex

source(here::here("code", "00_configuracion.R"))
library(fixest)

V <- VENTANAS[["2012_2026"]]
panel <- leer_panel("2012_2026")
estricto <- muestra_estacion(panel, CTRL_ESTRICTO, V$focal)
amplio   <- muestra_estacion(panel, CTRL_AMPLIO, V$focal)

TABLA_EF        <- c(p93 = "tabla_04_es_efectos_fijos_p93.tex", p97 = "tabla_C03_es_efectos_fijos_p97.tex",
                     pdi = "tabla_C04_es_efectos_fijos_pdi.tex")

# ---- Tablas 4, C.3 y C.4: escalera de efectos fijos; sin efecto fijo de estación va el indicador de tratada ----
# (sin la columna sin efectos fijos: el texto describe ese sesgo en una frase)

ESCALERA <- list(
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
  etable_tex(modelos, drop = "Constant", replace = TRUE,
             extralines = list(Tratadas = n[1, ], Controles = n[2, ], Comunas = sapply(modelos, n_comunas, d = x),
                               `Valor $p$ bins previos` = sapply(modelos, p_previos)),
             archivo = TABLA_EF[[fv]])
}

# ---- Tabla C.5: control amplio (> 3 km) y estricto (> 5 km), dos columnas por combustible ----

COMB_CONTROLES <- c(p93 = "Gasolina 93", p97 = "Gasolina 97", pdi = "Diésel")
datos <- unlist(lapply(names(COMB_CONTROLES), function(fv) list(amplio[!is.na(get(fv))], estricto[!is.na(get(fv))])),
                recursive = FALSE)
modelos <- mapply(estimar_es, datos, rep(names(COMB_CONTROLES), each = 2), SIMPLIFY = FALSE)
n <- mapply(n_estaciones, modelos, datos)
etable_tex(modelos, replace = TRUE,
           headers = list(as.list(setNames(rep(2, length(COMB_CONTROLES)), COMB_CONTROLES)),
                          rep(c("Amplio", "Estricto"), length(COMB_CONTROLES))),
           extralines = list(Tratadas = n[1, ], Controles = n[2, ], Comunas = mapply(n_comunas, modelos, datos),
                             `Valor $p$ bins previos` = sapply(modelos, p_previos)),
           archivo = "tabla_C05_es_controles.tex")

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
print(res[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)], nrows = Inf)   # cifras que cita el texto
guardar(grafico_es(res, "estimador"), "grafico_05_es_sunab.pdf", alto = 3.2)

# ---- Tabla 5: ATT estático, número principal ----

ANIOS_CNE <- 2014:2021   # años de la muestra con estructura de precios de la CNE
cne <- fread(ruta_proc("estructura_precios.csv"))[year(fecha) %in% ANIOS_CNE,
                                                   .(margen_cne = 100 * mean(margen)), by = combustible]
MARGEN_CNE <- c(p93 = cne[combustible == "Gasolina 93", margen_cne], pdi = cne[combustible == "Diésel", margen_cne])
MARGEN_MEPCO <- c(p93 = "m93", p97 = "m97", pdi = "mdi")   # la 95 no tiene costo MEPCO
message(sprintf("Tabla 5: margen CNE %d-%d, %% del precio: %s", min(ANIOS_CNE), max(ANIOS_CNE),
                paste(sprintf("%s %.2f", names(MARGEN_CNE), MARGEN_CNE), collapse = ", ")))

x[, post := as.integer(treated == 1L & ym >= g_evento)]
pre12 <- x[treated == 1L & miym - mi(g_evento) %between% c(-12L, -1L)]   # 12 meses previos a la entrada

t5 <- rbindlist(lapply(names(PRECIOS), function(fv) {
  z <- x[!is.na(get(fv))]
  tw <- coeftable(estimar_es(z, fv, "post"))["post", ]
  sa <- coeftable(summary(estimar_es(z, fv, "sunab(cohorte, per)"), agg = "ATT"))["ATT", , drop = FALSE]
  precio <- pre12[, mean(get(fv), na.rm = TRUE)]
  m_mepco <- if (fv %in% names(MARGEN_MEPCO)) pre12[, mean(get(MARGEN_MEPCO[[fv]]), na.rm = TRUE)] else NA_real_
  data.table(fv, est = c("TWFE", "Sun y Abraham"), att = c(tw[[1]], sa[1, 1]), se = c(tw[[2]], sa[1, 2]),
             p = c(tw[[4]], sa[1, 4]), precio, m_mepco,
             m_cne = if (fv %in% names(MARGEN_CNE)) MARGEN_CNE[[fv]] else NA_real_)
}))
t5[, `:=`(pesos = att / 100 * precio, pct_mepco = 100 * (att / 100 * precio) / m_mepco, pct_cne = 100 * att / m_cne)]
print(t5)

num <- function(v, k) fifelse(is.na(v), "--", sprintf("%.*f", k, v))
cuerpo <- unlist(lapply(c("TWFE", "Sun y Abraham"), function(e) {
  r <- t5[est == e][match(names(PRECIOS), fv)]
  c(sprintf("\\multicolumn{5}{l}{\\emph{%s}} \\\\", e),
    filas_coef("\\quad En \\% del precio", r$att, r$se, r$p),
    fila("\\quad En \\$/litro", num(r$pesos, 1)),
    fila("\\quad En \\% del margen CNE", num(r$pct_cne, 1)),
    fila("\\quad En \\% del margen MEPCO", num(r$pct_mepco, 1)),
    "\\addlinespace")
}))
r <- t5[est == "TWFE"][match(names(PRECIOS), fv)]
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule", cuerpo, "\\midrule",
                          fila("Precio previo de las tratadas (\\$/litro)", num(r$precio, 0)),
                          fila(sprintf("Margen CNE %d--%d (\\%% del precio)", min(ANIOS_CNE), max(ANIOS_CNE)), num(r$m_cne, 1)),
                          fila("Margen MEPCO previo (\\$/litro)", num(r$m_mepco, 1))),
               "tabla_05_att_principal.tex")
