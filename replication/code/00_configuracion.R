# 00_configuracion.R
#
# Parámetros, rutas, formato y funciones comunes. Todos los scripts lo cargan con
# source(here::here("code", "00_configuracion.R")). Todo número que define la muestra o
# la especificación está aquí; ningún script lo redefine.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(here)
})
stopifnot("falta R.utils: fread() lo usa para leer los .csv.gz" = requireNamespace("R.utils", quietly = TRUE))

# ==============================================================================
# Rutas
# ==============================================================================

DIR_RAW  <- here("data", "raw")
DIR_PROC <- here("data", "processed")
DIR_OUT  <- here("outputs")

ruta_raw  <- function(...) file.path(DIR_RAW, ...)
ruta_proc <- function(...) file.path(DIR_PROC, ...)
ruta_out  <- function(archivo) file.path(DIR_OUT, archivo)

# ==============================================================================
# Construcción de la base
# ==============================================================================

R_TIERRA <- 6371                     # km
SEMANA_0 <- as.IDate("2012-01-05")   # jueves ancla de la grilla semanal (calendario MEPCO)
MAX_ARRASTRE_SEM <- 13L              # semanas máximas que se arrastra un precio sin reporte
RADIO_MISMO_LOCAL_M <- 50            # un código nuevo a menos de 50 m de uno apagado es el mismo local

R_TRATADA  <- 2   # km: tratada si hubo una entrada competidora a <= 2 km
R_CTRL_AMP <- 3   # km: control amplio, nunca una entrada a <= 3 km
R_CTRL_EST <- 5   # km: control estricto, nunca una entrada a <= 5 km
RADIOS_EXTRA <- c(1, 3)   # km: radios adicionales de tratamiento (el de 2 y el de 5 ya existen)
MARCAS_MAYORES <- c("copec", "shell", "enex", "aramco_petrobras")   # entrante independiente: ninguna de estas
AUTOPISTA_M <- 100   # m: estación de autopista si está a <= 100 m de la red vial clase 1

REFORMA_MEPCO <- as.IDate("2023-01-26")   # primera actualización trisemanal del costo (ley 21.537)

# Ventanas: incumbentes = estaciones presentes en `desde`; cohortes focales desde `focal`
VENTANAS <- list(
  "2012_2026" = list(desde = 2012L, hasta = as.IDate("2026-12-31"), focal = 2014L),
  "2014_2019" = list(desde = 2014L, hasta = as.IDate("2019-12-31"), focal = 2015L),
  "2021_2026" = list(desde = 2021L, hasta = as.IDate("2026-02-28"), focal = 2022L)
)
dir_ventana <- function(v) ruta_proc(paste0("panel_", v))

# ==============================================================================
# Especificación
# ==============================================================================

CTRL_AMPLIO   <- c("ctrl_a", "ctrl_b")   # valores de role_entry
CTRL_ESTRICTO <- "ctrl_b"

BIN_M <- 6L   # meses por bin de tiempo desde la entrada
NBIN  <- 4L   # bins a cada lado; los extremos se agrupan

FE_PRINCIPAL <- "station_key + ym + region^year + distribuidor^year"

PRECIOS <- c(p93 = "Gasolina 93", p95 = "Gasolina 95", p97 = "Gasolina 97", pdi = "Diésel")

# ==============================================================================
# Formato de tablas
# ==============================================================================

DICT <- c(setNames(c(sprintf("$\\leq -%d$", NBIN), sprintf("$%d$", -(NBIN - 1):(NBIN - 1)),
                     sprintf("$\\geq %d$", NBIN)),
                   sprintf("rel::%d:treated", -NBIN:NBIN)),
          treated = "Tratada", n = "Observaciones", ym = "Mes", station_key = "Estación",
          `region^year` = "Región $\\times$ año", `distribuidor^year` = "Marca $\\times$ año")

fixest::setFixest_dict(DICT)
fixest::setFixest_etable(digits = "r3", fitstat = ~n + r2, digits.stats = 3, float = FALSE, depvar = FALSE,
                         style.tex = fixest::style.tex("aer", yesNo = c("Sí", "No"),
                                                       fixef.suffix = "", tablefoot = FALSE))
fixest::setFixest_notes(FALSE)

fila <- function(...) paste(paste(c(...), collapse = " & "), "\\\\")

# números en formato español: coma decimal y punto de miles ({,} para que en modo matemático
# no agregue espacio). Solo toca dígitos a ambos lados del separador.
a_coma <- function(x) {
  x <- gsub("(?<=\\d),(?=\\d{3}(?!\\d))", "\001", x, perl = TRUE)
  x <- gsub("(?<=\\d)\\.(?=\\d)", "{,}", x, perl = TRUE)
  gsub("\001", ".", x, fixed = TRUE)
}
stopifnot(a_coma("-0.296$^{***}$ & 124,434 & [-0.2; 1.5] & 2021") == "-0{,}296$^{***}$ & 124.434 & [-0{,}2; 1{,}5] & 2021")

# etable de fixest a un .tex, con los números en formato español
# (do.call: etable evalúa sus argumentos en su propio marco y no acepta que lleguen por `...`)
etable_tex <- function(..., archivo) writeLines(a_coma(do.call(fixest::etable, c(list(...), tex = TRUE))), ruta_out(archivo))
estrellas <- function(p) fifelse(p < 0.01, "$^{***}$", fifelse(p < 0.05, "$^{**}$", fifelse(p < 0.10, "$^{*}$", "")))

# coeficiente con estrellas y, en la fila siguiente, su error estándar entre paréntesis
filas_coef <- function(nombre, est, se, p, dig = 3) {
  c(fila(nombre, sprintf("%.*f%s", dig, est, estrellas(p))), fila("", sprintf("(%.*f)", dig, se)))
}

# tabular booktabs; `cuerpo`: líneas entre \toprule y \bottomrule
escribir_tabla <- function(columnas, cuerpo, archivo) {
  writeLines(a_coma(c(sprintf("\\begin{tabular}{%s}", columnas), "\\toprule", cuerpo,
                      "\\bottomrule", "\\end{tabular}")), ruta_out(archivo))
}

# ==============================================================================
# Formato de figuras
# ==============================================================================

# Las series se distinguen por color, forma y tipo de línea a la vez. Los colores tienen
# luminosidad escalonada (L* = 19, 44, 62 y 75), así se separan también en
# una impresión en blanco y negro.
COLORES <- c("#0B2E59", "#B5442B", "#4FA3A5", "#E3B23C")
FORMAS  <- c(16, 17, 15, 4)                                   # círculo, triángulo, cuadrado, cruz
LINEAS  <- c("solid", "dashed", "dotted", "dotdash")
COLOR_REF <- "grey40"                                         # líneas de referencia (entrada, cero)

theme_set(theme_bw(base_size = 10) +
            theme(legend.position = "bottom",
                  strip.background = element_rect(fill = "grey95"),
                  palette.colour.discrete = COLORES, palette.fill.discrete = COLORES,
                  palette.shape.discrete = scales::pal_manual(FORMAS),
                  palette.linetype.discrete = scales::pal_manual(LINEAS)))
for (g in c("pointrange", "point", "line", "errorbar")) update_geom_defaults(g, list(colour = COLORES[1]))
for (g in c("col", "bar", "ribbon")) update_geom_defaults(g, list(fill = COLORES[1]))

ANCHO_FIG <- 5   # pulgadas; la tesis incluye las figuras a tamaño natural, así la letra mide igual en todas
# PDF con las fuentes incrustadas y el signo menos dibujado (el dispositivo pdf() no incrusta
# Helvetica ni Symbol). En macOS, quartz; cairo_pdf ahí requiere XQuartz. En el resto, cairo_pdf.
DISPOSITIVO_PDF <- if (capabilities("aqua")) {
  function(filename, ...) grDevices::quartz(type = "pdf", file = filename, ...)
} else grDevices::cairo_pdf
# OutDec pone la coma decimal en los ejes
guardar <- function(fig, archivo, alto, ancho = ANCHO_FIG) {
  withr::with_options(list(OutDec = ","),
                      ggsave(ruta_out(archivo), fig, width = ancho, height = alto, device = DISPOSITIVO_PDF))
}

# estudio de eventos (event_time, estimate, se) con IC al 95 %; `serie`: columna que separa series
grafico_es <- function(res, serie = NULL, dodge = 0.4, y = "Efecto sobre el precio (%)",
                       facetas = ~combustible, escalas = "free_y") {
  fig <- ggplot(res, aes(event_time, estimate)) +
    geom_hline(yintercept = 0, colour = COLOR_REF) +
    geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                    position = position_dodge(width = dodge), size = 0.3) +
    facet_wrap(facetas, scales = escalas) +
    scale_x_continuous(breaks = -NBIN:NBIN) +
    labs(x = "Semestres desde la entrada", y = y, colour = NULL, shape = NULL)
  if (is.null(serie)) fig else fig + aes(colour = .data[[serie]], shape = .data[[serie]])
}

# ==============================================================================
# Calendario: índices enteros para la aritmética de tiempo desde la entrada
# ==============================================================================

mi <- function(d) {   # mes continuo
  d <- as.IDate(d)
  year(d) * 12L + (month(d) - 1L)
}

mi2date <- function(m) {
  out <- rep(NA_integer_, length(m))
  ok <- !is.na(m)
  out[ok] <- as.integer(as.IDate(sprintf("%d-%02d-01", m[ok] %/% 12L, m[ok] %% 12L + 1L)))
  as.IDate(out)
}

semana <- function(d) as.integer(as.IDate(d) - SEMANA_0) %/% 7L   # índice de la grilla semanal

# ==============================================================================
# Distancias
# ==============================================================================

# haversine en km, matriz completa. h se topa en 1: el error de punto flotante daría NaN
# en pares casi coincidentes
dist_km <- function(lat1, lon1, lat2 = lat1, lon2 = lon1) {
  a1 <- lat1 * pi / 180; o1 <- lon1 * pi / 180
  a2 <- lat2 * pi / 180; o2 <- lon2 * pi / 180
  h <- sin(outer(a1, a2, "-") / 2)^2 + outer(cos(a1), cos(a2)) * sin(outer(o1, o2, "-") / 2)^2
  h[h > 1] <- 1
  2 * R_TIERRA * asin(sqrt(h))
}

dist_propia <- function(lat, lon) {   # diagonal Inf: nadie compite consigo
  D <- dist_km(lat, lon)
  diag(D) <- Inf
  D
}

modal <- function(x) {   # valor más frecuente, sin NA; empate: el primero en aparecer
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

CRS_M <- 32719   # WGS84 / UTM 19S, la del shapefile de red vial

# ==============================================================================
# Lectura de los paneles y muestra de estimación
# ==============================================================================

leer_panel <- function(ventana, archivo = "panel_mensual.csv", select = NULL) {
  panel <- fread(file.path(dir_ventana(ventana), archivo), select = select)
  fechas <- intersect(c("ym", "wk", "g_entry", "g2_entry", "g5_entry", "g2_5_entry", "fecha_entrante",
                        paste0(c("g_entry_r", "g2_entry_r"), rep(RADIOS_EXTRA, each = 2))), names(panel))
  panel[, (fechas) := lapply(.SD, as.IDate), .SDcols = fechas]
  panel[, year := year(ym)][]
}

# Tratadas + controles `roles`, cortada en la segunda entrada, sin cohortes previas a
# `focal`, solo incumbentes. rel: bin de BIN_M meses (+-NBIN); los controles van en -1.
# `radio`: km de la definición de tratada. 2 es la principal; 5 usa la asignación a 5 km (primera
# entrada a <= 5 km, Gráfico 6); con RADIOS_EXTRA es tratada la que tuvo una entrada a <= radio
# y los controles siguen siendo `roles` (el resto queda fuera).
muestra_estacion <- function(panel, roles, focal, radio = R_TRATADA) {
  if (radio == R_TRATADA) {
    g <- "g_entry"; g2 <- "g2_entry"; rol <- "role_entry"
  } else if (radio == R_CTRL_EST) {
    g <- "g5_entry"; g2 <- "g2_5_entry"; rol <- "role5_entry"
  } else {
    stopifnot(radio %in% RADIOS_EXTRA)
    g <- paste0("g_entry_r", radio); g2 <- paste0("g2_entry_r", radio); rol <- "rol_radio"
    panel <- copy(panel)[, rol_radio := fifelse(!is.na(get(g)), "treated", role_entry)]
  }
  d <- panel[get(rol) %in% c("treated", roles) & (is.na(get(g2)) | ym < get(g2)) & base == TRUE]
  d <- d[!(get(rol) == "treated" & year(get(g)) < focal)]
  d[, `:=`(treated = as.integer(get(rol) == "treated"), g_evento = get(g))]
  d[, rel := fifelse(treated == 1L, pmax(-NBIN, pmin(NBIN, (miym - mi(g_evento)) %/% BIN_M)), -1L)]
  d[]
}

# Especificación principal: log(precio) x 100, FE_PRINCIPAL, errores agrupados por comuna
estimar_es <- function(data, y, rhs = "i(rel, treated, ref = -1)", ...) {
  fixest::feols(as.formula(sprintf("log(%s) * 100 ~ %s | %s", y, rhs, FE_PRINCIPAL)),
                data = data, cluster = ~comuna, ...)
}

# sunab() no acepta bins: se le pasa per = cohorte + rel, de modo que per - cohorte = bin.
# Las no tratadas llevan una cohorte fuera del rango de meses: sunab() las trata como nunca tratadas.
COHORTE_NUNCA <- 99999L
cohortes_sunab <- function(d) {
  d[, cohorte := fifelse(treated == 1L, mi(g_evento), COHORTE_NUNCA)]
  d[, per := fifelse(treated == 1L, cohorte + rel, miym)][]
}

# coeficientes `patron`::k de un modelo fixest, con la referencia -1 en cero
tidy_es <- function(m, patron = "rel") {
  ct <- fixest::coeftable(m)
  ct <- ct[grepl(sprintf("^%s::", patron), rownames(ct)), , drop = FALSE]
  d <- data.table(event_time = as.integer(sub(sprintf("^%s::(-?\\d+).*", patron), "\\1", rownames(ct))),
                  estimate = ct[, 1], se = ct[, 2], p = ct[, 4])
  setorder(rbind(d, data.table(event_time = -1L, estimate = 0, se = 0, p = NA_real_)), event_time)[]
}

# tratadas y controles que usa el modelo (sin singletons descartados)
n_estaciones <- function(m, d) {
  u <- d[fixest::obs(m)]
  c(uniqueN(u[treated == 1L, station_key]), uniqueN(u[treated == 0L, station_key]))
}

# comunas (clusters) que usa el modelo
n_comunas <- function(m, d) uniqueN(d[fixest::obs(m), comuna])

# valor p del test de Wald conjunto de los bins previos a la entrada (-NBIN a -2), con el
# vcov agrupado del modelo. En Sun y Abraham (`d` = datos de estimación con cohorte y per) el
# test es sobre los bins agregados: cada bin es el promedio de los coeficientes por cohorte
# ponderado por las observaciones tratadas de la cohorte en ese bin, los mismos pesos que usa
# fixest al agregar (se verifica que reproduce sus coeficientes), y su vcov es W V W'.
p_previos <- function(m, patron = "rel", d = NULL) {
  if (is.null(d)) {
    previos <- sprintf("^%s::-[2-%d]($|:)", patron, NBIN)
    return(sprintf("%.3f", fixest::wald(m, keep = previos, print = FALSE)$p))
  }
  b <- m$coefficients
  k <- as.integer(sub(sprintf("^%s::(-?\\d+):cohort::.*", patron), "\\1", names(b)))
  coh <- sub(".*:cohort::", "", names(b))
  u <- d[fixest::obs(m)][treated == 1L, .N, by = .(rel = per - cohorte, cohorte = as.character(cohorte))]
  n <- u[data.table(rel = k, cohorte = coh), on = .(rel, cohorte), N]
  bins <- -NBIN:-2
  W <- t(sapply(bins, function(r) ifelse(k == r, n, 0) / sum(n[k == r])))
  bb <- drop(W %*% b)
  stopifnot("la agregación no reproduce a fixest" =
              isTRUE(all.equal(bb, unname(coef(m)[sprintf("%s::%d", patron, bins)]), tolerance = 1e-8)))
  F <- drop(t(bb) %*% solve(W %*% m$cov.scaled %*% t(W)) %*% bb) / length(bins)
  sprintf("%.3f", pf(F, length(bins), n_comunas(m, d) - 1L, lower.tail = FALSE))
}
