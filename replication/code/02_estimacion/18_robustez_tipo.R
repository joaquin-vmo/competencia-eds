# 18_robustez_tipo.R
#
# Composición y robustez de la diferencia por tipo de entrante de 17_tipo_entrante.R (independiente:
# ni Copec, ni Shell/Enex, ni Aramco/Petrobras). Misma muestra (control estricto, ventana 2012_2026) y
# estimador que la Tabla 7: Sun y Abraham con un grupo por tipo (FE_PRINCIPAL, errores por comuna). Las
# filas que interactúan Post con covariables y la permutación usan TWFE (post + post:independiente),
# porque sunab() no admite esas interacciones.
#   Tabla 8: tratadas por tipo de entrante: entrantes, cohortes, regiones, marca de la incumbente,
#     precio de la entrante en sus seis primeros meses y de la incumbente en 2013.
#   Tabla 9: la diferencia independiente - de marca (gasolina 93 y diésel) con otra inferencia
#     (agrupando también por entrante, por región, permutando el tipo entre entrantes), comparando
#     dentro de región, de año de cohorte y de características de la tratada (post interactuado con
#     ellas), en submuestras y con la entrada adelantada 24 meses (placebo, solo el período previo real).
#     El estimador de imputación, con bootstrap, está en 19_imputacion_tipo.R, que run_all.R no corre.
#   Tabla C.14: ATT de Sun y Abraham por marca de la entrante (Copec, Shell, Aramco, marca pequeña, sin bandera).
#   Gráfico B.19: la diferencia sacando cada entrante independiente y cada región.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/tabla_08_composicion_tipo.tex, outputs/tabla_09_robustez_tipo.tex,
#          outputs/tabla_C14_subtipo_entrante.tex, outputs/grafico_B19_dejar_fuera_tipo.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)
set.seed(20261107)

TIPOS <- c("De marca", "Independiente")
FV <- c(p93 = "Gasolina 93", pdi = "Diésel")
NPERM <- 2000L   # permutaciones del tipo entre entrantes
MESES_VIDA <- 24L

panel <- leer_panel("2012_2026")
prep <- function(dd) {
  dd[, post := as.integer(treated == 1L & ym >= g_evento)]
  dd[, tipo := fifelse(treated == 1L, TIPOS[1L + !(entrante_marca %in% MARCAS_MAYORES)], NA_character_)]
  dd[, independiente := as.integer(tipo %in% TIPOS[2])]
  dd[, ent_cl := fifelse(treated == 1L, entrante_key, station_key)]   # cluster: la entrante; cada control el suyo
  dd[, gtipo := fifelse(treated == 1L, independiente, NA_integer_)]
  cohortes_sunab(dd, "gtipo")   # Sun y Abraham con un grupo por tipo: 0 de marca, 1 independiente
}
d <- prep(muestra_estacion(panel, CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal))

# características de la tratada antes de toda cohorte (2013) y de su entrante
b13 <- d[year == 2013L, .(n13 = mean(n_vecinas), b_p93 = mean(p93_dev, na.rm = TRUE)), by = station_key]
d <- b13[d, on = "station_key"]
vida <- panel[, .(ini = min(miym), fin = max(miym)), by = station_key]
FIN <- max(vida$fin)
d[vida, on = c(entrante_key = "station_key"), `:=`(ent_ini = i.ini, ent_fin = i.fin)]
pe <- panel[station_key %in% unique(d$entrante_key)]
pe <- pe[pe[, .I[miym <= min(miym) + 5L], by = station_key]$V1][
  , .(ent_p93 = mean(p93_dev, na.rm = TRUE), ent_pdi = mean(pdi_dev, na.rm = TRUE)), by = station_key]
d[pe, on = c(entrante_key = "station_key"), `:=`(ent_p93 = i.ent_p93, ent_pdi = i.ent_pdi)]
mediana_tr <- function(v) median(unique(d[treated == 1L, c("station_key", v), with = FALSE])[[v]], na.rm = TRUE)
d[, `:=`(densa = as.integer(treated == 1L & n13 > mediana_tr("n13")),
         barata = as.integer(treated == 1L & b_p93 <= mediana_tr("b_p93")),
         cerca = as.integer(treated == 1L & dist_entry <= 1),
         inc_mayor = as.integer(treated == 1L & distribuidor %in% MARCAS_MAYORES),
         inc_autop = as.integer(treated == 1L & d_autopista <= AUTOPISTA_M),
         ent_autop = as.integer(treated == 1L & entrante_autopista %in% TRUE),
         rm = as.integer(region == "metropolitana"),
         ent_barata = as.integer(treated == 1L & ent_p93 < 0),   # NA: entrante sin precio de la 93 en sus 6 primeros meses
         cohorte_anio = fifelse(treated == 1L, year(g_evento), 0L))]
tr <- unique(d[treated == 1L], by = "station_key")

# ---- Tabla 8: composición de las tratadas por tipo de entrante ----

pct <- function(x) sprintf("%.0f", 100 * mean(x, na.rm = TRUE))
col_tipo <- function(t) {
  x <- tr[tipo == t]
  e <- unique(x, by = "entrante_key")
  c(entrantes = nrow(e), tratadas = nrow(x), por_entrante = sprintf("%.1f", nrow(x) / nrow(e)),
    max_entrante = max(x[, .N, by = entrante_key]$N),
    c1416 = pct(year(x$g_evento) <= 2016L), c1719 = pct(year(x$g_evento) %between% c(2017L, 2019L)),
    c2026 = pct(year(x$g_evento) >= 2020L),
    rm = pct(x$region == "metropolitana"), valpo = pct(x$region == "valparaiso"),
    maule_nuble = pct(x$region %in% c("maule", "nuble")), tarapaca = pct(x$region == "tarapaca"),
    otras_reg = pct(!x$region %in% c("metropolitana", "valparaiso", "maule", "nuble", "tarapaca")),
    copec = pct(x$distribuidor == "copec"), shell = pct(x$distribuidor == "shell"),
    aramco = pct(x$distribuidor == "aramco_petrobras"), otra = pct(!x$distribuidor %in% MARCAS_MAYORES),
    ent_p93 = sprintf("%.1f", mean(e$ent_p93, na.rm = TRUE)), ent_pdi = sprintf("%.1f", mean(e$ent_pdi, na.rm = TRUE)),
    inc_p93 = sprintf("%.1f", mean(x$b_p93, na.rm = TRUE)), dist = sprintf("%.2f", mean(x$dist_entry)),
    vecinas = sprintf("%.1f", mean(x$n13, na.rm = TRUE)),
    sale = pct(e$ent_fin < FIN & e$ent_fin - e$ent_ini < MESES_VIDA))
}
comp <- sapply(TIPOS, col_tipo)
print(comp)
ETIQ <- c(entrantes = "Entrantes", tratadas = "Tratadas", por_entrante = "Tratadas por entrante (media)",
          max_entrante = "Tratadas por entrante (máximo)",
          c1416 = "\\addlinespace\\emph{Cohorte (\\% de las tratadas)} & & \\\\ \\quad 2014--2016",
          c1719 = "\\quad 2017--2019", c2026 = "\\quad 2020--2026",
          rm = "\\addlinespace\\emph{Región (\\% de las tratadas)} & & \\\\ \\quad Metropolitana",
          valpo = "\\quad Valparaíso", maule_nuble = "\\quad Maule y Ñuble", tarapaca = "\\quad Tarapacá",
          otras_reg = "\\quad Otras",
          copec = "\\addlinespace\\emph{Marca de la incumbente (\\%)} & & \\\\ \\quad Copec", shell = "\\quad Shell",
          aramco = "\\quad Aramco (ex Petrobras)", otra = "\\quad Otra o sin bandera",
          ent_p93 = "\\addlinespace\\emph{Precio menos la media nacional (\\$/litro)} & & \\\\ \\quad Entrante, gasolina 93, primeros 6 meses",
          ent_pdi = "\\quad Entrante, diésel, primeros 6 meses", inc_p93 = "\\quad Incumbente, gasolina 93, 2013",
          dist = "\\addlinespace Distancia a la entrante (km)", vecinas = "Estaciones a 2 km o menos en 2013",
          sale = sprintf("Entrantes que salen del panel antes de %d meses (\\%%)", MESES_VIDA))
escribir_tabla("lcc", c(fila("", "Entrante de marca", "Entrante independiente"), "\\midrule",
                        vapply(names(ETIQ), function(k) fila(ETIQ[[k]], comp[k, 1], comp[k, 2]), "")),
               "tabla_08_composicion_tipo.tex")

# ---- Tabla 9: robustez de la diferencia independiente - de marca ----

# Sun y Abraham con un grupo por tipo; devuelve la diferencia independiente - de marca. Con otro
# agrupamiento (`cl`) los grados de libertad son los grupos de ese agrupamiento menos uno.
dif_sa <- function(x, fv, cl = NULL) {
  z <- x[!is.na(get(fv))]
  m <- estimar_sa(z, fv)
  if (!is.null(cl)) m <- summary(m, cluster = cl)
  a <- att_sa(m, z)[efecto == "dif1"]
  gl <- if (is.null(cl)) n_comunas(m, z) - 1L else min(sapply(all.vars(cl), function(v) uniqueN(z[obs(m)][[v]]))) - 1L
  data.table(est = a$est, se = a$se, p = 2 * pt(-abs(a$est / a$se), gl))
}

# TWFE: post + post:independiente (+ extra); devuelve la diferencia, el efecto de marca y el de la independiente
dif <- function(x, fv, extra = "", cl = ~comuna) {
  m <- feols(as.formula(sprintf("log(%s) * 100 ~ post + post:independiente%s | %s", fv, extra, FE_PRINCIPAL)),
             data = x[!is.na(get(fv))], cluster = cl)
  ct <- coeftable(m)
  data.table(est = ct["post:independiente", 1], se = ct["post:independiente", 2], p = ct["post:independiente", 4],
             marca = ct["post", 1], ind = sum(coef(m)[c("post", "post:independiente")]))
}

# permutación del tipo entre las entrantes (libre o dentro de la región de la tratada). Cada permutación
# es una regresión con los mismos efectos fijos: se centran una vez todas las columnas (FWL).
ent <- unique(tr[, .(entrante_key, independiente, region)], by = "entrante_key")
perm_p <- function(fv, estrato) {
  z <- d[!is.na(get(fv))]
  idx <- match(z$entrante_key, ent$entrante_key)
  fe <- z[, .(station_key, ym, ry = paste(region, year), dy = paste(distribuidor, year))]
  X <- demean(cbind(y = log(z[[fv]]) * 100, post = z$post, obs = z$post * z$independiente), fe)
  res_post <- function(v) resid(lm.fit(X[, "post", drop = FALSE], v))
  ry <- res_post(X[, "y"])
  coef_dif <- function(v) { rp <- as.matrix(res_post(v)); colSums(rp * ry) / colSums(rp^2) }
  b0 <- coef_dif(X[, "obs", drop = FALSE])
  stopifnot("FWL no reproduce la diferencia" = abs(b0 - dif(d, fv)$est) < 1e-6)
  # en bloques de 200 permutaciones: la matriz completa no cabe en 8 GB
  b <- unlist(lapply(split(seq_len(NPERM), ceiling(seq_len(NPERM) / 200)), function(bloque) {
    P <- vapply(bloque, function(k) {
      lab <- if (estrato) copy(ent)[, ind := sample(independiente), by = region]$ind else sample(ent$independiente)
      lab[idx]
    }, numeric(nrow(z)))
    P[is.na(P)] <- 0
    coef_dif(demean(z$post * P, fe))
  }))
  mean(abs(b) >= abs(b0))
}

reg_ambos <- tr[, uniqueN(tipo), by = region][V1 == 2L, region]
top5 <- tr[independiente == 1L, .N, by = entrante_key][order(-N)][1:5, entrante_key]
EXTRA_CAR <- " + post:densa + post:barata + post:cerca + post:inc_mayor + post:inc_autop + post:ent_autop + post:rm"
amplio <- prep(muestra_estacion(panel, CTRL_AMPLIO, VENTANAS[["2012_2026"]]$focal))
# a 1 km: tratadas cuya primera entrada a <= 1 km es la de 2 km (mismo mes y entrante a <= 1 km)
r1 <- prep(muestra_estacion(panel, CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal, radio = 1))
r1 <- r1[treated == 0L | (g_evento == g_entry & dist_entry <= 1)]
plac <- copy(d)[treated == 0L | ym < g_evento][, g_evento := mi2date(mi(g_evento) - 24L)]
plac[, post := as.integer(treated == 1L & ym >= g_evento)]
plac[, rel := fifelse(treated == 1L, pmax(-NBIN, pmin(NBIN, (miym - mi(g_evento)) %/% BIN_M)), -1L)]
plac <- cohortes_sunab(plac, "gtipo")

PRUEBAS <- list(
  list("\\emph{Inferencia (Sun y Abraham)}", NULL),
  list("Diferencia principal, errores agrupados por comuna", function(fv) dif_sa(d, fv)),
  list("Errores agrupados por comuna y por entrante", function(fv) dif_sa(d, fv, cl = ~comuna + ent_cl)),
  list("Errores agrupados por región (15)", function(fv) dif_sa(d, fv, cl = ~region)),
  list("\\addlinespace\\emph{Comparación dentro de grupos (TWFE, Post interactuado con)}", NULL),
  list("Ninguno (estimación TWFE principal)", function(fv) dif(d, fv)),
  list("Región", function(fv) dif(d, fv, " + i(region, post)")),
  list("Año de la cohorte", function(fv) dif(d, fv, " + i(cohorte_anio, post, ref = 0)")),
  list("Características de la tratada$^a$", function(fv) dif(d, fv, EXTRA_CAR)),
  list("Entrante que abre bajo la media nacional$^b$", function(fv) dif(d, fv, " + post:ent_barata")),
  list("Región, año de la cohorte y características", function(fv) dif(d, fv, paste0(" + i(region, post) + i(cohorte_anio, post, ref = 0)", EXTRA_CAR))),
  list("\\addlinespace\\emph{Muestra (Sun y Abraham)}", NULL),
  list("Solo incumbentes de Copec, Shell o Aramco", function(fv) dif_sa(d[treated == 0L | inc_mayor == 1L], fv)),
  list("Cohortes 2014--2019", function(fv) dif_sa(d[treated == 0L | year(g_evento) <= 2019L], fv)),
  list("Cohortes 2015--2017", function(fv) dif_sa(d[treated == 0L | year(g_evento) %between% c(2015L, 2017L)], fv)),
  list(sprintf("Solo regiones con tratadas de ambos tipos (%d)", length(reg_ambos)), function(fv) dif_sa(d[region %in% reg_ambos], fv)),
  list("Sin las cinco entrantes independientes que más tratan", function(fv) dif_sa(d[!entrante_key %in% top5], fv)),
  list(sprintf("Solo entrantes que siguen en el panel %d meses o más", MESES_VIDA),
       function(fv) dif_sa(d[treated == 0L | ent_fin == FIN | ent_fin - ent_ini >= MESES_VIDA], fv)),
  list("Control amplio (sin entradas a 3 km o menos)", function(fv) dif_sa(amplio, fv)),
  list("Tratadas a 1 km o menos de la entrante", function(fv) dif_sa(r1, fv)),
  list("\\addlinespace\\emph{Placebo (Sun y Abraham)}", NULL),
  list("Entrada adelantada 24 meses (placebo)", function(fv) dif_sa(plac, fv))
)
cuerpo <- unlist(lapply(PRUEBAS, function(pr) {
  if (is.null(pr[[2]])) return(sprintf("%s\\multicolumn{3}{l}{%s} \\\\", if (startsWith(pr[[1]], "\\addlinespace")) "\\addlinespace " else "",
                                       sub("^\\\\addlinespace", "", pr[[1]])))
  r <- rbindlist(lapply(names(FV), pr[[2]]), fill = TRUE)
  message(sprintf("Tabla 9, %s: %s", pr[[1]], paste(sprintf("%.3f (%.3f)", r$est, r$se), collapse = " | ")))
  fila(paste0("\\quad ", pr[[1]]), sprintf("%.3f%s (%.3f)", r$est, estrellas(r$p), r$se))   # una fila por prueba: la tabla cabe en una página
}))
p_perm <- function(p) fifelse(p == 0, sprintf("$<$%.4f", 1 / NPERM), sprintf("%.4f", p))   # ninguna permutación la alcanza
pp <- sapply(names(FV), function(fv) c(perm_p(fv, FALSE), perm_p(fv, TRUE)))
cuerpo <- append(cuerpo, c(fila("\\quad Valor $p$ por permutación del tipo entre entrantes (TWFE)", p_perm(pp[1, ])),
                           fila("\\quad Valor $p$ por permutación dentro de región (TWFE)", p_perm(pp[2, ]))),
                 after = which(grepl("multicolumn.*Comparación", cuerpo)) - 1L)
escribir_tabla("lcc", c(fila("", unname(FV)), "\\midrule", cuerpo), "tabla_09_robustez_tipo.tex")

# ---- Tabla C.14: efecto por marca de la entrante ----

SUB <- c(copec = "Copec", shell = "Shell", aramco_petrobras = "Aramco", pequena = "Marca pequeña", sin_bandera = "Sin bandera")
d[, sub := fifelse(treated == 0L, "control",
                   fifelse(entrante_marca == "enex", "shell",
                           fifelse(entrante_marca %in% MARCAS_MAYORES, entrante_marca,
                                   fifelse(entrante_marca == "sin bandera", "sin_bandera", "pequena"))))]
d[, gsub := fifelse(treated == 1L, match(sub, names(SUB)) - 1L, NA_integer_)]
xs <- cohortes_sunab(copy(d), "gsub")
ms <- lapply(names(PRECIOS), function(fv) estimar_sa(xs[!is.na(get(fv))], fv))
as <- mapply(function(m, fv) att_sa(m, xs[!is.na(get(fv))]), ms, names(PRECIOS), SIMPLIFY = FALSE)
cuerpo <- unlist(lapply(seq_along(SUB), function(j) {
  s <- names(SUB)[j]
  ct <- rbindlist(lapply(as, function(a) a[efecto == paste0("grupo", j - 1L)]))
  filas_coef(sprintf("%s (%d entrantes, %d tratadas)", SUB[[s]], d[treated == 1L & sub == s, uniqueN(entrante_key)],
                     d[treated == 1L & sub == s, uniqueN(station_key)]), ct$est, ct$se, ct$p)
}))
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule", cuerpo, "\\midrule",
                          fila("Controles", mapply(function(m, fv) n_estaciones(m, xs[!is.na(get(fv))])[2], ms, names(PRECIOS)))),
               "tabla_C14_subtipo_entrante.tex")

# ---- Gráfico B.19: la diferencia sacando cada entrante independiente y cada región ----

fuera <- rbindlist(lapply(names(FV), function(fv) rbind(
  rbindlist(lapply(unique(tr[independiente == 1L, entrante_key]), function(k) dif_sa(d[!entrante_key %in% k], fv)[, quita := "Una entrante independiente"])),
  rbindlist(lapply(sort(unique(tr$region)), function(r) dif_sa(d[region != r], fv)[, quita := "Una región"])))[, combustible := FV[[fv]]]))
print(fuera[, .(min = round(min(est), 3), max = round(max(est), 3), t_max = round(max(est / se), 2)), by = .(combustible, quita)])
base <- rbindlist(lapply(names(FV), function(fv) dif_sa(d, fv)[, combustible := FV[[fv]]]))
fuera[, orden := frank(est, ties.method = "first"), by = .(combustible, quita)]
fig <- ggplot(fuera, aes(orden, est)) +
  geom_hline(yintercept = 0, colour = COLOR_REF) +
  geom_hline(data = base, aes(yintercept = est), linetype = "dashed", colour = COLOR_REF) +
  geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), size = 0.1) +
  facet_grid(combustible ~ quita, scales = "free_x") +
  labs(x = "Estimaciones ordenadas de menor a mayor", y = "Independiente menos de marca (%)")
guardar(fig, "grafico_B19_dejar_fuera_tipo.pdf", alto = 3.6)
