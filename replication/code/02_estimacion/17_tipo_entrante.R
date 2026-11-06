# 17_tipo_entrante.R
#
# Heterogeneidad del efecto de la entrada según el tipo de entrante (Castillo, 2018): independiente
# (ni Copec, ni Shell/Enex, ni Aramco/Petrobras: sin bandera y marcas pequeñas) o de marca. El tipo es
# el de la entrante más cercana del mes de la primera entrada a <= 2 km (entrante_marca, 03_paneles.R).
# Especificación principal, control estricto, Post separado por tipo de entrante.
#   Tabla 7: ATT estático por tipo (TWFE: efecto de marca, diferencia y efecto independiente; SA por
#     separado con cada grupo de tratadas y todos los controles).
#   Gráfico 9: regresión de distribución de 06_distribucion.R (mismos umbrales) con kappa_g(c) por tipo:
#     acumulada observada de las tratadas de cada tipo después de la entrada y contrafactual (observada - kappa_g).
#   Gráfico B.13: kappa_g(c) por tipo con intervalos al 95 % (errores agrupados por comuna, puntuales).
#   Gráfico B.12: estudio de eventos con el tiempo desde la entrada interactuado con el tipo.
#   Gráfico B.14: kappa_g(c) de la gasolina 93 y el diésel por marca de la incumbente, estaciones a
#     <= 2 km en 2013 y desvío del precio de la media nacional en 2013 (mediana de las tratadas).
#     Los umbrales son comunes: un grupo con precios más bajos tiene su kappa_g(c) en umbrales más bajos
#     aunque su desplazamiento sea igual, por lo que entre grupos se compara el tamaño de la curva.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/tabla_07_tipo_entrante.tex, outputs/grafico_09_distribucion_entrante.pdf,
#          outputs/grafico_B12_es_tipo_entrante.pdf, outputs/grafico_B13_kappa_tipo_entrante.pdf,
#          outputs/grafico_B14_distribucion_otras.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

NC <- 41L                                  # umbrales, como 06_distribucion.R
Q  <- seq(0.02, 0.98, length.out = NC)
TIPOS <- c("De marca", "Independiente")

d <- muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal)
stopifnot("tratada sin marca de entrante" = !anyNA(d[treated == 1L, entrante_marca]))
d[, post := as.integer(treated == 1L & ym >= g_entry)]
d[, tipo := fifelse(treated == 1L, TIPOS[1L + !(entrante_marca %in% MARCAS_MAYORES)], NA_character_)]
message("Tratadas por tipo de entrante: ", paste(d[treated == 1L, uniqueN(station_key), by = tipo][, paste(tipo, V1)], collapse = ", "))
message("Marcas de las entrantes independientes: ")
print(unique(d[tipo == TIPOS[2], .(station_key, entrante_marca)])[, .N, by = entrante_marca][order(-N)])

# grupos del anexo, fijados en 2013 (antes de toda cohorte focal); mediana entre tratadas
b13 <- d[year == 2013L, .(n13 = mean(n_vecinas), b_p93 = mean(p93_dev, na.rm = TRUE), b_pdi = mean(pdi_dev, na.rm = TRUE)), by = station_key]
d <- b13[d, on = "station_key"]
d[treated == 1L, incumbente := fifelse(distribuidor == "copec", "Copec", "Otra marca")]
med <- function(v) median(unique(d[treated == 1L & is.finite(get(v)), c("station_key", v), with = FALSE])[[v]])
d[treated == 1L, densidad := fifelse(n13 > med("n13"), "Más densa", "Menos densa")]
for (fv in c("p93", "pdi")) {
  b <- paste0("b_", fv)
  d[treated == 1L & is.finite(get(b)), (paste0("previo_", fv)) := fifelse(get(b) <= med(b), "Barata en 2013", "Cara en 2013")]
}

# Post separado por los niveles de `grupo`; las tratadas sin grupo quedan fuera, los controles no
post_por_grupo <- function(z, grupo) {
  x <- z[treated == 0L | !is.na(get(grupo))]
  niv <- sort(unique(na.omit(x[[grupo]])))
  vars <- sprintf("post_g%d", seq_along(niv))
  for (k in seq_along(niv)) set(x, j = vars[k], value = x$post * (x[[grupo]] %in% niv[k]))
  list(x = x, niv = niv, vars = vars)
}

# regresión de distribución con kappa por grupo (umbrales: cuantiles de la muestra conjunta completa)
kappa_grupo <- function(fv, grupo) {
  y <- paste0(fv, "_dev")
  z <- d[!is.na(get(y))]
  cs <- quantile(z[[y]], Q, names = FALSE)
  ind <- sprintf("ind%02d", seq_along(cs))
  z[, (ind) := lapply(cs, function(cc) as.numeric(get(y) <= cc))]
  g <- post_por_grupo(z, grupo)
  m <- feols(as.formula(sprintf("c(%s) ~ %s | %s", paste(ind, collapse = ", "), paste(g$vars, collapse = " + "), FE_PRINCIPAL)),
             data = g$x, cluster = ~comuna)
  obs_post <- lapply(g$niv, function(k) g$x[post == 1L & get(grupo) %in% k][[y]])
  rbindlist(lapply(seq_along(ind), function(j) {
    V <- vcov(m[[j]])[g$vars, g$vars, drop = FALSE]
    data.table(combustible = PRECIOS[[fv]], q = Q[j], c = cs[j], grupo = g$niv, kappa = coef(m[[j]])[g$vars], se = sqrt(diag(V)),
               observada = vapply(obs_post, function(v) mean(v <= cs[j]), numeric(1)),
               se_dif = sqrt(pmax(diag(V) + V[1, 1] - 2 * V[, 1], 0)), dif = coef(m[[j]])[g$vars] - coef(m[[j]])[g$vars[1]])
  }))
}

# ---- Tabla 7: ATT estático por tipo de entrante ----

x0 <- cohortes_sunab(copy(d))
x0[, independiente := as.integer(tipo %in% TIPOS[2])]
res <- rbindlist(lapply(names(PRECIOS), function(fv) {
  x <- x0[!is.na(get(fv))]
  m_dif <- estimar_es(x, fv, "post + post:independiente")
  ct <- coeftable(m_dif)
  b <- coef(m_dif); V <- vcov(m_dif)
  e_ind <- sum(b); se_ind <- sqrt(sum(V))
  sa <- sapply(TIPOS, function(tp) {
    z <- x[treated == 0L | tipo %in% tp]
    m_sa <- estimar_es(z, fv, "sunab(cohorte, per)")
    s <- coeftable(summary(m_sa, agg = "ATT"))["ATT", ]
    c(s[[1]], s[[2]], s[[4]], as.numeric(p_previos(m_sa, "per", z)))
  })
  u <- x[obs(m_dif)]
  data.table(fv, marca = ct["post", 1], se_marca = ct["post", 2], p_marca = ct["post", 4],
             dif = ct["post:independiente", 1], se_dif = ct["post:independiente", 2], p_dif = ct["post:independiente", 4],
             ind = e_ind, se_ind = se_ind, p_ind = 2 * pt(-abs(e_ind / se_ind), n_comunas(m_dif, x) - 1L),
             sa_marca = sa[1, 1], se_sa_marca = sa[2, 1], p_sa_marca = sa[3, 1], pre_sa_marca = sa[4, 1],
             sa_ind = sa[1, 2], se_sa_ind = sa[2, 2], p_sa_ind = sa[3, 2], pre_sa_ind = sa[4, 2],
             n_marca = uniqueN(u[tipo %in% TIPOS[1], station_key]), n_ind = uniqueN(u[tipo %in% TIPOS[2], station_key]),
             controles = uniqueN(u[treated == 0L, station_key]), comunas = uniqueN(u$comuna))
}))
print(res[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

coefs <- function(nombre, est, se, p) filas_coef(nombre, est, se, p)
cuerpo <- c("\\multicolumn{5}{l}{\\emph{TWFE}} \\\\",
            coefs("\\quad Entrante de marca", res$marca, res$se_marca, res$p_marca),
            coefs("\\quad Entrante independiente $-$ de marca", res$dif, res$se_dif, res$p_dif),
            coefs("\\quad Entrante independiente", res$ind, res$se_ind, res$p_ind), "\\addlinespace",
            "\\multicolumn{5}{l}{\\emph{Sun y Abraham}} \\\\",
            coefs("\\quad Entrante de marca", res$sa_marca, res$se_sa_marca, res$p_sa_marca),
            fila("\\quad\\quad Valor $p$ bins previos", sprintf("%.3f", res$pre_sa_marca)),
            coefs("\\quad Entrante independiente", res$sa_ind, res$se_sa_ind, res$p_sa_ind),
            fila("\\quad\\quad Valor $p$ bins previos", sprintf("%.3f", res$pre_sa_ind)))
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule", cuerpo, "\\midrule",
                          fila("Tratadas con entrante de marca", res$n_marca), fila("Tratadas con entrante independiente", res$n_ind),
                          fila("Controles", res$controles), fila("Comunas", res$comunas)),
               "tabla_07_tipo_entrante.tex")

# ---- Gráfico 9: distribución por tipo de entrante ----

k_tipo <- rbindlist(lapply(names(PRECIOS), kappa_grupo, grupo = "tipo"))
k_tipo[, combustible := factor(combustible, levels = PRECIOS)]
sel <- vapply(c(0.10, 0.25, 0.50, 0.75, 0.90), function(v) Q[which.min(abs(Q - v))], numeric(1))
message("Gráfico 9: kappa (pp) en los cuantiles 10/25/50/75/90, y umbrales con diferencia significativa al 5% (de 41)")
print(dcast(k_tipo[q %in% sel], combustible + grupo ~ round(q, 2), value.var = "kappa", fun.aggregate = function(v) round(100 * v, 1)))
print(k_tipo[grupo == TIPOS[2], .(sig5 = sum(abs(dif / se_dif) > 1.96), positivas = sum(dif / se_dif > 1.96)), by = combustible])
# integral de kappa sobre c: caída del precio medio ($/L) dentro de los umbrales
print(k_tipo[order(q), .(integral = round(sum(diff(c) * (head(kappa, -1) + tail(kappa, -1)) / 2), 2)), by = .(combustible, grupo)])

grafico_kappa <- function(k, facetas = ~combustible) {
  ggplot(k, aes(q, 100 * kappa, colour = grupo, shape = grupo)) +
    geom_hline(yintercept = 0, colour = COLOR_REF) +
    geom_pointrange(aes(ymin = 100 * (kappa - 1.96 * se), ymax = 100 * (kappa + 1.96 * se)), size = 0.12,
                    position = position_dodge(width = 0.012)) +
    facet_wrap(facetas) +
    labs(x = "Cuantil del umbral en la muestra conjunta", y = "Efecto sobre la distribución acumulada (pp)", colour = NULL, shape = NULL)
}
SERIES <- c("Con entrada (observada)", "Sin entrada (contrafactual)")
cdf <- rbind(k_tipo[, .(combustible, grupo, c, F = observada, serie = SERIES[1])],
             k_tipo[, .(combustible, grupo, c, F = observada - kappa, serie = SERIES[2])])
cdf[, `:=`(serie = factor(serie, levels = SERIES), grupo = factor(sprintf("Entrante %s", tolower(grupo)), levels = sprintf("Entrante %s", tolower(TIPOS))))]
fig <- ggplot(cdf, aes(c, F, colour = serie, linetype = serie)) +
  geom_line() +
  facet_grid(grupo ~ combustible, scales = "free_x") +
  labs(x = "Precio menos su media nacional del mes ($/L)", y = "Distribución acumulada", colour = NULL, linetype = NULL)
guardar(fig, "grafico_09_distribucion_entrante.pdf", alto = 4.2)
guardar(grafico_kappa(k_tipo), "grafico_B13_kappa_tipo_entrante.pdf", alto = 3.6)

# ---- Gráfico B.12: estudio de eventos por tipo de entrante ----

d[, `:=`(t_marca = treated * (tipo %in% TIPOS[1]), t_ind = treated * (tipo %in% TIPOS[2]))]
es <- rbindlist(lapply(names(PRECIOS), function(fv) {
  m <- estimar_es(d[!is.na(get(fv))], fv, "i(rel, t_marca, ref = -1) + i(rel, t_ind, ref = -1)")
  message(sprintf("Gráfico B.12, %s: valor p bins previos de marca %.3f, independiente %.3f", PRECIOS[[fv]],
                  wald(m, keep = "^rel::-[234]:t_marca$", print = FALSE)$p, wald(m, keep = "^rel::-[234]:t_ind$", print = FALSE)$p))
  ct <- coeftable(m)
  r <- data.table(event_time = as.integer(sub("^rel::(-?\\d+):.*", "\\1", rownames(ct))),
                  grupo = TIPOS[grepl("t_ind$", rownames(ct)) + 1L], estimate = ct[, 1], se = ct[, 2])
  rbind(r, data.table(event_time = -1L, grupo = TIPOS, estimate = 0, se = 0))[, combustible := PRECIOS[[fv]]]
}))
es[, `:=`(combustible = factor(combustible, levels = PRECIOS), grupo = factor(grupo, levels = TIPOS))]
print(dcast(es, combustible + event_time ~ grupo, value.var = "estimate", fun.aggregate = function(v) round(v, 3)))
guardar(grafico_es(es, "grupo"), "grafico_B12_es_tipo_entrante.pdf", alto = 3.2)

# ---- Gráfico B.14: otras dimensiones, gasolina 93 y diésel ----

# filas: (1) frente a (2), en el orden alfabético de los grupos que usa kappa_grupo
DIMS <- c(incumbente = "Marca: Copec (1), otra (2)", densidad = "Estaciones a 2 km en 2013: más (1), menos (2)",
          previo = "Precio en 2013: barata (1), cara (2)")
otras <- rbindlist(lapply(c("p93", "pdi"), function(fv) rbindlist(lapply(names(DIMS), function(dm) {
  grupo <- if (dm == "previo") paste0("previo_", fv) else dm
  kappa_grupo(fv, grupo)[, dimension := DIMS[[dm]]]
}))))
otras[, `:=`(combustible = factor(combustible, levels = PRECIOS), dimension = factor(dimension, levels = DIMS))]
otras[, orden := match(grupo, sort(unique(grupo))), by = dimension]
stopifnot("el orden de los grupos no calza con las etiquetas de las filas" =
            otras[orden == 1L, all(grupo %in% c("Copec", "Más densa", "Barata en 2013"))])
print(otras[q %in% sel, .(kappa = round(100 * kappa, 1)), by = .(combustible, dimension, grupo, q = round(q, 2))][
  , dcast(.SD, combustible + dimension + grupo ~ q, value.var = "kappa")])
print(otras[order(q), .(integral = round(sum(diff(c) * (head(kappa, -1) + tail(kappa, -1)) / 2), 2),
                        sig5 = sum(se_dif > 0 & abs(dif / se_dif) > 1.96)), by = .(combustible, dimension, grupo)])
fig <- grafico_kappa(copy(otras)[, grupo := factor(sprintf("Grupo (%d)", orden))]) +
  facet_grid(dimension ~ combustible, labeller = label_wrap_gen(24))
guardar(fig, "grafico_B14_distribucion_otras.pdf", alto = 6.5, ancho = 6.2)

# ---- Log: composición (las entradas de marca son más recientes) y robustez del ATT por tipo ----

print(unique(d[treated == 1L, .(station_key, tipo, anio = year(g_entry), rm = region == "metropolitana", dist_entry, n13, b_p93)])[
  , .(tratadas = .N, anio_medio = round(mean(anio), 1), desde_2021 = round(mean(anio >= 2021), 2), rm = round(mean(rm), 2),
      distancia = round(mean(dist_entry), 2), vecinas_2013 = round(mean(n13), 1), desvio_93_2013 = round(mean(b_p93, na.rm = TRUE), 2)), by = tipo])
for (fv in c("p93", "pdi")) for (mu in c("cohortes hasta 2019", "sin Región Metropolitana")) {
  x <- x0[!is.na(get(fv))]
  x <- if (mu == "sin Región Metropolitana") x[region != "metropolitana"] else x[treated == 0L | year(g_evento) <= 2019L]
  ct <- coeftable(estimar_es(x, fv, "post + post:independiente"))
  message(sprintf("%s, %s: de marca %.3f (%.3f), diferencia independiente %.3f (%.3f)", PRECIOS[[fv]], mu,
                  ct["post", 1], ct["post", 2], ct["post:independiente", 1], ct["post:independiente", 2]))
}
