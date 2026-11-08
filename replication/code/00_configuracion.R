# 00_configuracion.R
#
# Parámetros, rutas, formato y funciones comunes. Todos los scripts lo cargan con
# source(here::here("code", "00_configuracion.R")). Todo número que define la muestra o
# la especificación está aquí; ningún script lo redefine.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(lubridate)
  library(ggplot2)
  library(here)
})

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
SEMANA_0 <- as.Date("2012-01-05")   # jueves ancla de la grilla semanal (calendario MEPCO)
MAX_ARRASTRE_SEM <- 13L              # semanas máximas que se arrastra un precio sin reporte
RADIO_MISMO_LOCAL_M <- 50            # un código nuevo a menos de 50 m de uno apagado es el mismo local

R_TRATADA  <- 2   # km: tratada si hubo una entrada competidora a <= 2 km
R_CTRL_AMP <- 3   # km: control amplio, nunca una entrada a <= 3 km
R_CTRL_EST <- 5   # km: control estricto, nunca una entrada a <= 5 km
RADIOS_EXTRA <- c(1, 3)   # km: radios adicionales de tratamiento (el de 2 y el de 5 ya existen)
MARCAS_MAYORES <- c("copec", "shell", "enex", "aramco_petrobras")   # entrante independiente: ninguna de estas
marca_red <- function(x) if_else(x %in% "enex", "shell", x)   # Enex opera la red Shell: para "misma marca" son una
AUTOPISTA_M <- 100   # m: estación de autopista si está a <= 100 m de la red vial clase 1

REFORMA_MEPCO <- as.Date("2023-01-26")   # primera actualización trisemanal del costo (ley 21.537)

# Ventanas: incumbentes = estaciones presentes en `desde`; cohortes focales desde `focal`
VENTANAS <- list(
  "2012_2026" = list(desde = 2012L, hasta = as.Date("2026-12-31"), focal = 2014L),
  "2014_2019" = list(desde = 2014L, hasta = as.Date("2019-12-31"), focal = 2015L),
  "2021_2026" = list(desde = 2021L, hasta = as.Date("2026-02-28"), focal = 2022L)
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
          setNames(c(sprintf("$\\leq -%d$", NBIN), sprintf("$%d$", -(NBIN - 1):(NBIN - 1)), sprintf("$\\geq %d$", NBIN)),
                   sprintf("per::%d", -NBIN:NBIN)),
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
estrellas <- function(p) if_else(p < 0.01, "$^{***}$", if_else(p < 0.05, "$^{**}$", if_else(p < 0.10, "$^{*}$", "")))

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

# TeX Gyre Pagella: la letra en que se basa newpxtext, la de la tesis. Se toma de TeX Live
# (fonts/opentype/public/tex-gyre/texgyrepagella-*.otf) y debe estar instalada en el sistema;
# en macOS, copiada a ~/Library/Fonts. Si falta, el dispositivo usa otra letra sin avisar.
FUENTE <- "TeX Gyre Pagella"

theme_set(theme_bw(base_size = 10, base_family = FUENTE) +
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
  d <- as.Date(d)
  year(d) * 12L + (month(d) - 1L)
}

mi2date <- function(m) make_date(m %/% 12L, m %% 12L + 1L, 1L)

semana <- function(d) as.integer(as.Date(d) - SEMANA_0) %/% 7L   # índice de la grilla semanal

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

# Lee un .csv como lo hacía fread(): enteros si todos los valores lo son y texto vacío = "" (no NA)
leer_csv <- function(archivo, ...) {
  read_csv(archivo, guess_max = Inf, show_col_types = FALSE, progress = FALSE, ...) |>
    mutate(across(where(is.character), \(x) replace_na(x, ""))) |>
    mutate(across(where(\(x) is.double(x) && !inherits(x, c("Date", "difftime"))),
                  \(x) if (all(x == round(x) & abs(x) < .Machine$integer.max, na.rm = TRUE)) as.integer(x) else x))
}
# 15 cifras significativas, como fwrite(); write_csv() escribe 17
escribir_csv <- function(d, archivo) {
  d |> mutate(across(where(\(x) is.double(x) && !inherits(x, c("Date", "difftime"))), \(x) signif(x, 15))) |>
    write_csv(archivo, na = "")
}

leer_panel <- function(ventana, archivo = "panel_mensual.csv", select = NULL) {
  panel <- leer_csv(file.path(dir_ventana(ventana), archivo), col_select = if (is.null(select)) everything() else all_of(select))
  fechas <- intersect(c("ym", "wk", "g_entry", "g2_entry", "g5_entry", "g2_5_entry", "fecha_entrante",
                        paste0(c("g_entry_r", "g2_entry_r"), rep(RADIOS_EXTRA, each = 2))), names(panel))
  panel |> mutate(across(all_of(fechas), as.Date), year = year(ym))
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
    panel <- mutate(panel, rol_radio = if_else(!is.na(.data[[g]]), "treated", role_entry))
  }
  panel |>
    filter(.data[[rol]] %in% c("treated", roles), is.na(.data[[g2]]) | ym < .data[[g2]], base == TRUE) |>
    filter(!(.data[[rol]] == "treated" & year(.data[[g]]) < focal)) |>
    mutate(treated = as.integer(.data[[rol]] == "treated"), g_evento = .data[[g]],
           rel = if_else(treated == 1L, pmax(-NBIN, pmin(NBIN, (miym - mi(g_evento)) %/% BIN_M)), -1L))
}

# Especificación principal: log(precio) x 100, FE_PRINCIPAL, errores agrupados por comuna
estimar_es <- function(data, y, rhs = "i(rel, treated, ref = -1)", ...) {
  fixest::feols(as.formula(sprintf("log(%s) * 100 ~ %s | %s", y, rhs, FE_PRINCIPAL)),
                data = data, cluster = ~comuna, ...)
}

# sunab() no acepta bins: se le pasa per = cohorte + rel, de modo que per - cohorte = bin.
# Las no tratadas llevan una cohorte fuera del rango de per: sunab() las trata como nunca tratadas.
# Cohorte anual, no mensual: con cohortes mensuales hay unos 750 coeficientes para 259 comunas y casi la
# mitad de las celdas cohorte x bin está en una sola comuna, lo que subestima el error agrupado por comuna
# (entre 0,58 y 0,78 veces el de TWFE por bin); con cohortes anuales queda entre 0,88 y 1,07.
# `grupo`: columna entera 0, 1, ... de las tratadas; la cohorte lleva el grupo en las decenas de miles
# (cohorte = año + 10000 x grupo), así una sola regresión estima los efectos de cada grupo con los mismos
# efectos fijos y controles, y agregar_sa() los separa.
COHORTE_NUNCA <- 99999L
GRUPO_SA <- 10000L
cohortes_sunab <- function(d, grupo = NULL) {
  g <- if (is.null(grupo)) 0L else d[[grupo]]
  stopifnot("grupo fuera de rango" = is.null(grupo) || all(d[[grupo]][d$treated == 1L] %in% 0:9))
  d |>
    mutate(cohorte = if_else(treated == 1L, year(g_evento) + GRUPO_SA * g, COHORTE_NUNCA)) |>
    mutate(per = if_else(treated == 1L, cohorte + rel, miym))
}
estimar_sa <- function(data, y, ...) estimar_es(data, y, "sunab(cohorte, per)", ...)

# Agregación de Sun y Abraham: cada efecto es el promedio de los coeficientes por cohorte ponderado
# por las observaciones tratadas de la cohorte en ese bin, los pesos que usa fixest (se verifica que
# los reproduce), y su varianza es w' V w con el vcov agrupado del modelo (`m` puede venir de summary()
# con otro vcov). `d`: datos de estimación con cohorte y per.
pesos_sa <- function(m, d) {
  nm <- names(m$coefficients)
  k <- suppressWarnings(as.integer(sub("^per::(-?\\d+):cohort::.*", "\\1", nm)))   # NA: otros regresores
  coh <- as.integer(sub(".*:cohort::", "", nm))
  u <- d |> slice(fixest::obs(m)) |> filter(treated == 1L) |> count(k = per - cohorte, coh = cohorte)
  tibble(k = k, coh = coh) |>
    left_join(u, by = c("k", "coh")) |>
    transmute(k, grupo = coh %/% GRUPO_SA, n) |>
    mutate(grupo = if_else(is.na(k), -1L, grupo), n = if_else(is.na(k), 0L, n), k = if_else(is.na(k), NA_integer_, k))   # otros regresores (p. ej. treated): peso 0
}
# filas de W: combinaciones de los coeficientes por cohorte; `sel` elige los de cada fila
w_sa <- function(p, sel) { w <- ifelse(sel %in% TRUE, p$n, 0); w / sum(w) }
comb_sa <- function(m, d, W) {
  W <- rbind(W)
  b <- drop(W %*% m$coefficients)
  V <- W %*% m$cov.scaled %*% t(W)
  list(b = b, V = V, se = sqrt(diag(V)), gl = n_comunas(m, d) - 1L)
}
# bins (-NBIN a NBIN, sin -1) por grupo, con la referencia en cero
agregar_sa <- function(m, d) {
  p <- pesos_sa(m, d)
  cel <- p |> filter(grupo >= 0L) |> distinct(grupo, k) |> filter(k != -1L) |> arrange(grupo, k)
  r <- comb_sa(m, d, t(mapply(function(g, kk) w_sa(p, p$grupo == g & p$k == kk), cel$grupo, cel$k)))
  if (n_distinct(cel$grupo) == 1L)
    stopifnot("la agregación no reproduce a fixest" =
                isTRUE(all.equal(unname(r$b), unname(coef(m)[sprintf("per::%d", cel$k)]), tolerance = 1e-8)))
  cel |>
    transmute(grupo, event_time = k, estimate = r$b, se = r$se, p = 2 * pt(-abs(r$b / r$se), r$gl)) |>
    bind_rows(tibble(grupo = unique(cel$grupo), event_time = -1L, estimate = 0, se = 0, p = NA_real_)) |>
    arrange(grupo, event_time)
}
# ATT (bins >= 0) de cada grupo y, si hay dos o más, la diferencia de cada uno con el grupo 0
att_sa <- function(m, d) {
  p <- pesos_sa(m, d)
  gs <- sort(unique(p$grupo[p$grupo >= 0L]))
  W <- t(sapply(gs, function(g) w_sa(p, p$grupo == g & p$k >= 0L)))
  if (length(gs) > 1L) W <- rbind(W, t(sapply(gs[-1], function(g) W[match(g, gs), ] - W[1, ])))
  r <- comb_sa(m, d, W)
  if (length(gs) == 1L)
    stopifnot("el ATT no reproduce a fixest" =
                abs(r$b - fixest::coeftable(summary(m, agg = "ATT"))["ATT", 1]) < 1e-8)
  tibble(efecto = c(paste0("grupo", gs), if (length(gs) > 1L) paste0("dif", gs[-1])),
         est = r$b, se = r$se, p = 2 * pt(-abs(r$b / r$se), r$gl))
}
# coeficientes `patron`::k de un modelo fixest, con la referencia -1 en cero
tidy_es <- function(m, patron = "rel") {
  ct <- fixest::coeftable(m)
  ct <- ct[grepl(sprintf("^%s::", patron), rownames(ct)), , drop = FALSE]
  tibble(event_time = as.integer(sub(sprintf("^%s::(-?\\d+).*", patron), "\\1", rownames(ct))),
         estimate = ct[, 1], se = ct[, 2], p = ct[, 4]) |>
    bind_rows(tibble(event_time = -1L, estimate = 0, se = 0, p = NA_real_)) |>
    arrange(event_time)
}

# tratadas y controles que usa el modelo (sin singletons descartados)
n_estaciones <- function(m, d) {
  u <- slice(d, fixest::obs(m))
  c(n_distinct(u$station_key[u$treated == 1L]), n_distinct(u$station_key[u$treated == 0L]))
}

# comunas (clusters) que usa el modelo
n_comunas <- function(m, d) n_distinct(d$comuna[fixest::obs(m)])

# valor p del test de Wald conjunto de los bins previos a la entrada (-NBIN a -2), con el vcov
# agrupado del modelo. En Sun y Abraham (`d` = datos de estimación con cohorte y per) el test es sobre
# los bins agregados del grupo `grupo`, con vcov W V W' y gl = comunas - 1.
p_previos <- function(m, patron = "rel", d = NULL, grupo = 0L) {
  if (is.null(d)) {
    previos <- sprintf("^%s::-[2-%d]($|:)", patron, NBIN)
    return(sprintf("%.3f", fixest::wald(m, keep = previos, print = FALSE)$p))
  }
  p <- pesos_sa(m, d)
  bins <- -NBIN:-2
  r <- comb_sa(m, d, t(sapply(bins, function(kk) w_sa(p, p$grupo == grupo & p$k == kk))))
  if (n_distinct(p$grupo[p$grupo >= 0L]) == 1L)
    stopifnot("la agregación no reproduce a fixest" =
                isTRUE(all.equal(unname(r$b), unname(coef(m)[sprintf("per::%d", bins)]), tolerance = 1e-8)))
  F <- drop(t(r$b) %*% solve(r$V) %*% r$b) / length(bins)
  sprintf("%.3f", pf(F, length(bins), r$gl, lower.tail = FALSE))
}

# Anillos de distancia con radio exterior R (Gráficos 6 y 10, B.21 y B.22). Para cada incumbente, su
# primera entrada competidora a <= R km con las reglas de 03_paneles.R (sin la propia marca, solo
# entradas dentro de su vida en el panel), la más cercana del mes: fecha (mes continuo), segunda entrada
# a <= R km, distancia, si es independiente (ni Copec, ni Shell/Enex, ni Aramco/Petrobras) y el anillo
# según `cortes` (0, 1, ...). Controles: incumbentes sin entradas de ninguna marca, ni de la propia, a <= R km.
# Devuelve la muestra con treated, g_evento, rel, el corte en la segunda entrada, sin cohortes previas a `focal`.
# `R_ctrl`: radio sin entradas de los controles (por defecto, el exterior); `excluir_misma = FALSE` cuenta también
# las entradas de la propia marca (columna misma: la entrante es de la marca de la incumbente).
muestra_anillos <- function(panel, entradas, cortes, focal, R_ctrl = max(cortes), excluir_misma = TRUE) {
  R <- max(cortes)
  st <- panel |>
    filter(base == TRUE, !is.na(lat)) |>
    summarise(lat = lat[1], lon = lon[1], distribuidor = distribuidor[1], ini = min(miym), fin = max(miym),
              .by = station_key)
  gm <- mi(entradas$g)
  D <- dist_km(st$lat, st$lon, entradas$elat, entradas$elon)
  D[!(outer(st$ini, gm, "<") & outer(st$fin, gm, ">="))] <- Inf
  controles <- st$station_key[apply(D, 1, min) > R_ctrl]
  misma <- outer(marca_red(st$distribuidor), marca_red(entradas$edist), "==")
  if (excluir_misma) D[!is.na(misma) & misma] <- Inf
  a <- map(seq_len(nrow(st)), function(i) {
    w <- which(D[i, ] <= R)
    if (!length(w)) return(NULL)
    meses <- sort(unique(gm[w]))
    primera <- w[gm[w] == meses[1]]
    cercana <- primera[which.min(D[i, primera])]
    tibble(station_key = st$station_key[i], g_r = meses[1], g2_r = if (length(meses) > 1) meses[2] else NA_integer_,
           km = D[i, cercana], indep = as.integer(!(entradas$edist[cercana] %in% MARCAS_MAYORES)),
           misma = as.integer(misma[i, cercana] %in% TRUE))
  }) |>
    list_rbind() |>
    filter(g_r %/% 12L >= focal) |>
    mutate(anillo = pmax(0L, findInterval(km, cortes, left.open = TRUE) - 1L))
  panel |>
    filter(base == TRUE, station_key %in% c(controles, a$station_key)) |>
    left_join(select(a, station_key, g_r, g2_r, km, indep, misma, anillo), by = "station_key") |>
    mutate(treated = as.integer(!is.na(g_r))) |>
    filter(treated == 0L | is.na(g2_r) | miym < g2_r) |>
    mutate(g_evento = mi2date(g_r),
           rel = if_else(treated == 1L, pmax(-NBIN, pmin(NBIN, (miym - g_r) %/% BIN_M)), -1L))
}
