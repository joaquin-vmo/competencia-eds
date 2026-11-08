# 03_es_principal.R
#
# Estudio de eventos principal, ventana 2012_2026. El estimador del cuerpo es Sun y Abraham (2021),
# con cohortes anuales (cohortes_sunab()); TWFE va en el anexo como comparación.
#   Tabla de efectos fijos: Sun y Abraham, control estricto, especificaciones de efectos fijos de menos a
#     más exigente, un bloque de cuatro columnas por combustible (93, 97 y diésel; la tesis no reporta la 95).
#   Tabla C.5: Sun y Abraham con control amplio y estricto, 93, 97 y diésel.
#   Gráfico 5: Sun y Abraham con la tratada definida por una entrada a <= 1 km y a <= 2 km.
#   Gráfico B.20: TWFE y Sun y Abraham con la misma muestra, efectos fijos y bins.
#   Tabla 5 (Sun y Abraham) y Tabla C.15 (TWFE): número principal. ATT estático por combustible,
#     en % del precio, en $/L y en % del margen, con dos denominadores:
#     - margen bruto de comercialización de la CNE (estructura de precios, Región Metropolitana),
#       promedio 2014-2021, los años de la muestra con estructura publicada; es el del Gráfico 1.
#     - margen sobre el costo MEPCO (precio - costo mayorista con MEPCO) de las tratadas en los
#       12 meses previos a la entrada, la definición de Alemania, México y California.
#     $/L = ATT (%) x precio medio de las tratadas en los 12 meses previos a la entrada.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv, data/processed/estructura_precios.csv
# Salida:  outputs/tabla_04_es_efectos_fijos.tex, outputs/tabla_C05_es_controles.tex,
#          outputs/grafico_05_es_radios.pdf, outputs/grafico_B20_es_twfe_sunab.pdf,
#          outputs/tabla_05_att_principal.tex, outputs/tabla_C15_att_twfe.tex

source(here::here("code", "00_configuracion.R"))
library(fixest)

V <- VENTANAS[["2012_2026"]]
panel <- leer_panel("2012_2026")
estricto <- cohortes_sunab(muestra_estacion(panel, CTRL_ESTRICTO, V$focal))
amplio   <- cohortes_sunab(muestra_estacion(panel, CTRL_AMPLIO, V$focal))

COMB_EF <- c(p93 = "Gasolina 93", p97 = "Gasolina 97", pdi = "Diésel")   # paneles de la tabla de efectos fijos

# ---- Tabla de efectos fijos: escalera con Sun y Abraham, un bloque de columnas por combustible; sin efecto fijo de estación va el indicador de tratada ----
# (sin la columna sin efectos fijos: el texto describe ese sesgo en una frase)

ESCALERA <- list(
  list(rhs = " + treated", fe = " | ym"),
  list(rhs = "",           fe = " | ym + station_key"),
  list(rhs = "",           fe = " | ym + station_key + region^year"),
  list(rhs = "",           fe = paste(" |", FE_PRINCIPAL))
)

partes <- map(names(COMB_EF), function(fv) {
  x <- filter(estricto, !is.na(.data[[fv]]))
  modelos <- map(ESCALERA, function(e) feols(
    as.formula(sprintf("log(%s) * 100 ~ sunab(cohorte, per)%s%s", fv, e$rhs, e$fe)),
    data = x, cluster = ~comuna))
  n <- sapply(modelos, n_estaciones, d = x)
  tx <- do.call(fixest::etable, list(modelos, drop = "Constant", replace = TRUE, tex = TRUE,
                                      extralines = list(Tratadas = n[1, ], Controles = n[2, ], Comunas = sapply(modelos, n_comunas, d = x),
                                                        `Valor $p$ bins previos` = sapply(modelos, p_previos, patron = "per", d = x))))
  strsplit(paste(a_coma(tx), collapse = "\n"), "\n")[[1]]
})
# un bloque de cuatro columnas por combustible, uno al lado del otro: las filas de las tres tablas coinciden
stopifnot("las tablas por combustible no tienen las mismas filas" = length(unique(lengths(partes))) == 1L)
celdas <- function(x) sub("\\\\\\\\\\s*$", "", strsplit(x, "&", fixed = TRUE)[[1]])
ini <- grep("\\midrule", partes[[1]], fixed = TRUE)[1]; fin <- grep("\\bottomrule", partes[[1]], fixed = TRUE)[1]
filas <- vapply((ini + 1):(fin - 1), function(i) {
  if (!grepl("&", partes[[1]][i], fixed = TRUE)) return(partes[[1]][i])
  paste0(paste(c(celdas(partes[[1]][i]), unlist(lapply(partes[-1], function(l) celdas(l[i])[-1]))), collapse = " & "), "\\\\")
}, "")
k <- length(COMB_EF)
encabezado <- c(sprintf("\\begin{tabular}{l%s}", strrep("c", 4L * k)), "   \\toprule",
                paste0(" & ", paste(sprintf("\\multicolumn{4}{c}{%s}", COMB_EF), collapse = " & "), "\\\\"),
                paste(sprintf("\\cmidrule(lr){%d-%d}", 2 + 4 * (0:(k - 1)), 5 + 4 * (0:(k - 1))), collapse = " "),
                paste0(" & ", paste(rep(sprintf("(%d)", 1:4), k), collapse = " & "), "\\\\"), "   \\midrule")
writeLines(c("\\begingroup", "\\centering", encabezado, filas, "   \\bottomrule", "\\end{tabular}", "\\par\\endgroup"),
           ruta_out("tabla_04_es_efectos_fijos.tex"))

# ---- Tabla C.5: control amplio (> 3 km) y estricto (> 5 km), dos columnas por combustible ----

COMB_CONTROLES <- c(p93 = "Gasolina 93", p97 = "Gasolina 97", pdi = "Diésel")
datos <- map(names(COMB_CONTROLES), function(fv) list(filter(amplio, !is.na(.data[[fv]])), filter(estricto, !is.na(.data[[fv]])))) |>
  list_flatten()
modelos <- map2(datos, rep(names(COMB_CONTROLES), each = 2), estimar_sa)
n <- mapply(n_estaciones, modelos, datos)
etable_tex(modelos, replace = TRUE,
           headers = list(as.list(setNames(rep(2, length(COMB_CONTROLES)), COMB_CONTROLES)),
                          rep(c("Amplio", "Estricto"), length(COMB_CONTROLES))),
           extralines = list(Tratadas = n[1, ], Controles = n[2, ], Comunas = mapply(n_comunas, modelos, datos),
                             `Valor $p$ bins previos` = mapply(p_previos, modelos, d = datos, MoreArgs = list(patron = "per"))),
           archivo = "tabla_C05_es_controles.tex")

# ---- Gráfico 5: Sun y Abraham con tratadas a <= 1 km y a <= 2 km ----

RADIOS_G5 <- c("Entrada a 1 km o menos", "Entrada a 2 km o menos")
uno <- cohortes_sunab(muestra_estacion(panel, CTRL_ESTRICTO, V$focal, radio = 1))
res <- map(names(PRECIOS), function(fv) bind_rows(
  tidy_es(estimar_sa(filter(uno, !is.na(.data[[fv]])), fv), "per") |> mutate(radio = RADIOS_G5[1]),
  tidy_es(estimar_sa(filter(estricto, !is.na(.data[[fv]])), fv), "per") |> mutate(radio = RADIOS_G5[2])) |>
  mutate(combustible = PRECIOS[[fv]])) |>
  list_rbind() |>
  mutate(combustible = factor(combustible, levels = PRECIOS), radio = factor(radio, levels = RADIOS_G5))
res |>   # cifras que cita el texto
  pivot_wider(id_cols = c(combustible, event_time), names_from = radio, values_from = c(estimate, se)) |>
  arrange(combustible, event_time) |>
  mutate(across(where(is.numeric), \(v) round(v, 3))) |>
  print(n = Inf)
message(sprintf("Gráfico 5: tratadas a 1 km %s", paste(map_int(names(PRECIOS), function(fv) {
  z <- filter(uno, !is.na(.data[[fv]])); n_estaciones(estimar_sa(z, fv), z)[1] }), collapse = "/")))
guardar(grafico_es(res, "radio"), "grafico_05_es_radios.pdf", alto = 3.2)

# ---- Gráfico B.20: TWFE y Sun y Abraham ----

ESTIMADORES <- c("TWFE", "Sun y Abraham")
x <- estricto
res <- map(names(PRECIOS), function(fv) {
  z <- filter(x, !is.na(.data[[fv]]))
  tw <- estimar_es(z, fv)
  message(sprintf("Gráfico 5, %s: %d tratadas y %d controles", PRECIOS[[fv]], n_estaciones(tw, z)[1], n_estaciones(tw, z)[2]))
  bind_rows(tidy_es(tw) |> mutate(estimador = ESTIMADORES[1]),
            tidy_es(estimar_es(z, fv, "sunab(cohorte, per)"), "per") |> mutate(estimador = ESTIMADORES[2])) |>
    mutate(combustible = PRECIOS[[fv]])
}) |>
  list_rbind() |>
  mutate(combustible = factor(combustible, levels = PRECIOS), estimador = factor(estimador, levels = ESTIMADORES))
res |> mutate(across(where(is.numeric), \(v) round(v, 3))) |> print(n = Inf)   # cifras que cita el texto
guardar(grafico_es(res, "estimador"), "grafico_B20_es_twfe_sunab.pdf", alto = 3.2)

# ---- Tablas 5 (Sun y Abraham) y C.15 (TWFE): ATT estático, número principal ----

ANIOS_CNE <- 2014:2021   # años de la muestra con estructura de precios de la CNE
cne <- leer_csv(ruta_proc("estructura_precios.csv")) |>
  filter(year(fecha) %in% ANIOS_CNE) |>
  summarise(margen_cne = 100 * mean(margen), .by = combustible)
MARGEN_CNE <- c(p93 = cne$margen_cne[cne$combustible == "Gasolina 93"], pdi = cne$margen_cne[cne$combustible == "Diésel"])
MARGEN_MEPCO <- c(p93 = "m93", p97 = "m97", pdi = "mdi")   # la 95 no tiene costo MEPCO
message(sprintf("Tabla 5: margen CNE %d-%d, %% del precio: %s", min(ANIOS_CNE), max(ANIOS_CNE),
                paste(sprintf("%s %.2f", names(MARGEN_CNE), MARGEN_CNE), collapse = ", ")))

x <- mutate(x, post = as.integer(treated == 1L & ym >= g_evento))
# precedencia de R: `a - b %between% c` es a - (b %between% c); se conserva tal cual, por eso incluye todas las tratadas
pre12 <- filter(x, treated == 1L & miym - between(mi(g_evento), -12L, -1L))   # 12 meses previos a la entrada

t5 <- map(names(PRECIOS), function(fv) {
  z <- filter(x, !is.na(.data[[fv]]))
  tw <- coeftable(estimar_es(z, fv, "post"))["post", ]
  sa <- coeftable(summary(estimar_es(z, fv, "sunab(cohorte, per)"), agg = "ATT"))["ATT", , drop = FALSE]
  precio <- mean(pre12[[fv]], na.rm = TRUE)
  m_mepco <- if (fv %in% names(MARGEN_MEPCO)) mean(pre12[[MARGEN_MEPCO[[fv]]]], na.rm = TRUE) else NA_real_
  m_cne <- if (fv %in% names(MARGEN_CNE)) MARGEN_CNE[[fv]] else NA_real_
  tibble(fv = fv, est = c("TWFE", "Sun y Abraham"), att = c(tw[[1]], sa[1, 1]), se = c(tw[[2]], sa[1, 2]),
         p = c(tw[[4]], sa[1, 4]), precio, m_mepco, m_cne)
}) |>
  list_rbind() |>
  mutate(pesos = att / 100 * precio, pct_mepco = 100 * (att / 100 * precio) / m_mepco, pct_cne = 100 * att / m_cne)
print(t5)

num <- function(v, k) if_else(is.na(v), "--", sprintf("%.*f", k, v))
TABLA_ATT <- c("Sun y Abraham" = "tabla_05_att_principal.tex", TWFE = "tabla_C15_att_twfe.tex")
for (e in names(TABLA_ATT)) {
  r <- t5 |> filter(est == e) |> slice(match(names(PRECIOS), fv))
  escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule",
                            filas_coef("En \\% del precio", r$att, r$se, r$p),
                            fila("En \\$/litro", num(r$pesos, 1)),
                            fila("En \\% del margen CNE", num(r$pct_cne, 1)),
                            fila("En \\% del margen MEPCO", num(r$pct_mepco, 1)), "\\midrule",
                            fila("Precio previo de las tratadas (\\$/litro)", num(r$precio, 0)),
                            fila(sprintf("Margen CNE %d--%d (\\%% del precio)", min(ANIOS_CNE), max(ANIOS_CNE)), num(r$m_cne, 1)),
                            fila("Margen MEPCO previo (\\$/litro)", num(r$m_mepco, 1))),
                 TABLA_ATT[[e]])
}
