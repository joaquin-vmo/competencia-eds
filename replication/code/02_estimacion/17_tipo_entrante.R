# 17_tipo_entrante.R
#
# Heterogeneidad del efecto de la entrada según el tipo de entrante (Castillo, 2018): independiente
# (ni Copec, ni Shell/Enex, ni Aramco/Petrobras: sin bandera y marcas pequeñas) o de marca. El tipo es
# el de la entrante más cercana del mes de la primera entrada a <= 2 km (entrante_marca, 03_paneles.R).
# Control estricto. Estimador del cuerpo: Sun y Abraham (cohortes anuales) en una sola regresión con un
# grupo por tipo (cohortes_sunab(d, grupo)), de la que salen el ATT de cada tipo y su diferencia.
#   Tabla 7: ATT de Sun y Abraham por tipo, diferencia y valor p de los bins previos de cada tipo.
#   Tabla C.16: lo mismo con TWFE (Post y Post x independiente).
#   Gráfico 9: regresión de distribución de 06_distribucion.R (mismos umbrales) con kappa_g(c) por tipo:
#     acumulada observada de las tratadas de cada tipo después de la entrada y contrafactual (observada - kappa_g).
#   Gráfico B.13: kappa_g(c) por tipo con intervalos al 95 % (errores agrupados por comuna, puntuales).
#   Gráfico B.12: estudio de eventos TWFE con el tiempo desde la entrada interactuado con el tipo.
#   Gráfico B.14: kappa_g(c) de la gasolina 93 y el diésel por marca de la incumbente, estaciones a
#     <= 2 km en 2013 y desvío del precio de la media nacional en 2013 (mediana de las tratadas).
#     Los umbrales son comunes: un grupo con precios más bajos tiene su kappa_g(c) en umbrales más bajos
#     aunque su desplazamiento sea igual, por lo que entre grupos se compara el tamaño de la curva.
#   Gráficos B.15 a B.17: estudio de eventos de Sun y Abraham por tipo de entrante en las ventanas
#     2012_2026, 2014_2019 y 2021_2026, una regresión por ventana con un grupo por tipo.
#   Log (nota al pie): la Tabla 7 contando también las entradas de la propia marca de la incumbente,
#     con dos grupos (de marca, independiente) y con tres (más las entradas de la propia marca).
#
# Entrada: data/processed/panel_{2012_2026,2014_2019,2021_2026}/panel_mensual.csv
# Salida:  outputs/tabla_07_tipo_entrante.tex, outputs/tabla_C16_tipo_entrante_twfe.tex,
#          outputs/grafico_09_distribucion_entrante.pdf, outputs/grafico_B12_es_tipo_entrante.pdf,
#          outputs/grafico_B13_kappa_tipo_entrante.pdf, outputs/grafico_B14_distribucion_otras.pdf,
#          outputs/grafico_B15_sa_tipo_2012_2026.pdf, outputs/grafico_B16_sa_tipo_2014_2019.pdf,
#          outputs/grafico_B17_sa_tipo_2021_2026.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

NC <- 41L                                  # umbrales, como 06_distribucion.R
Q  <- seq(0.02, 0.98, length.out = NC)
TIPOS <- c("De marca", "Independiente")

# tipo de entrante; gtipo: 0 de marca, 1 independiente (grupo de cohortes_sunab())
con_tipo <- function(dd) {
  stopifnot("tratada sin marca de entrante" = !anyNA(dd[treated == 1L, entrante_marca]))
  dd[, tipo := fifelse(treated == 1L, TIPOS[1L + !(entrante_marca %in% MARCAS_MAYORES)], NA_character_)]
  dd[, gtipo := match(tipo, TIPOS) - 1L]
  cohortes_sunab(dd, "gtipo")
}
d <- con_tipo(muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal))
d[, post := as.integer(treated == 1L & ym >= g_entry)]
d[, independiente := as.integer(tipo %in% TIPOS[2])]
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

# Sun y Abraham con un grupo por nivel de `grupo` (orden alfabético); las tratadas sin grupo quedan
# fuera, los controles no. Devuelve los datos con cohorte y per y los niveles.
sa_por_grupo <- function(z, grupo) {
  x <- copy(z)[treated == 0L | !is.na(get(grupo))]
  niv <- sort(unique(na.omit(x[[grupo]])))
  x[, gcod := match(get(grupo), niv) - 1L]
  list(x = cohortes_sunab(x, "gcod"), niv = niv)
}

# regresión de distribución con kappa por grupo (umbrales: cuantiles de la muestra conjunta completa)
kappa_grupo <- function(fv, grupo) {
  y <- paste0(fv, "_dev")
  z <- d[!is.na(get(y))]
  cs <- quantile(z[[y]], Q, names = FALSE)
  ind <- sprintf("ind%02d", seq_along(cs))
  z[, (ind) := lapply(cs, function(cc) as.numeric(get(y) <= cc))]
  g <- sa_por_grupo(z, grupo)
  m <- feols(as.formula(sprintf("c(%s) ~ sunab(cohorte, per) | %s", paste(ind, collapse = ", "), FE_PRINCIPAL)),
             data = g$x, cluster = ~comuna)
  obs_post <- lapply(g$niv, function(k) g$x[post == 1L & get(grupo) %in% k][[y]])
  rbindlist(lapply(seq_along(ind), function(j) {
    a <- att_sa(m[[j]], g$x)
    dif <- a[startsWith(efecto, "dif")]
    data.table(combustible = PRECIOS[[fv]], q = Q[j], c = cs[j], grupo = g$niv,
               kappa = a[startsWith(efecto, "grupo"), est], se = a[startsWith(efecto, "grupo"), se],
               observada = vapply(obs_post, function(v) mean(v <= cs[j]), numeric(1)),
               se_dif = c(0, dif$se), dif = c(0, dif$est))
  }))
}

# ---- Tabla 7 (Sun y Abraham) y Tabla C.16 (TWFE): ATT estático por tipo de entrante ----

res <- rbindlist(lapply(names(PRECIOS), function(fv) {
  x <- d[!is.na(get(fv))]
  m <- estimar_sa(x, fv)
  a <- att_sa(m, x)
  for (g in 0:1) {   # cifras que cita el texto
    pre <- agregar_sa(m, x)[grupo == g & event_time < -1L]
    message(sprintf("Tabla 7, %s, %s: bins previos %s", PRECIOS[[fv]], TIPOS[g + 1L],
                    paste(sprintf("%d: %.3f (p %.3f)", pre$event_time, pre$estimate, pre$p), collapse = "; ")))
  }
  mt <- estimar_es(x, fv, "post + post:independiente")
  tw <- coeftable(mt); b <- coef(mt); V <- vcov(mt)
  u <- x[obs(m)]
  data.table(fv,
             sa_marca = a[efecto == "grupo0", est], se_sa_marca = a[efecto == "grupo0", se], p_sa_marca = a[efecto == "grupo0", p],
             sa_ind = a[efecto == "grupo1", est], se_sa_ind = a[efecto == "grupo1", se], p_sa_ind = a[efecto == "grupo1", p],
             sa_dif = a[efecto == "dif1", est], se_sa_dif = a[efecto == "dif1", se], p_sa_dif = a[efecto == "dif1", p],
             pre_marca = as.numeric(p_previos(m, "per", x, 0L)), pre_ind = as.numeric(p_previos(m, "per", x, 1L)),
             marca = tw["post", 1], se_marca = tw["post", 2], p_marca = tw["post", 4],
             dif = tw["post:independiente", 1], se_dif = tw["post:independiente", 2], p_dif = tw["post:independiente", 4],
             ind = sum(b), se_ind = sqrt(sum(V)), p_ind = 2 * pt(-abs(sum(b) / sqrt(sum(V))), n_comunas(m, x) - 1L),
             n_marca = uniqueN(u[tipo %in% TIPOS[1], station_key]), n_ind = uniqueN(u[tipo %in% TIPOS[2], station_key]),
             controles = uniqueN(u[treated == 0L, station_key]), comunas = uniqueN(u$comuna))
}))
print(res[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

pie <- c("\\midrule", fila("Tratadas con entrante de marca", res$n_marca), fila("Tratadas con entrante independiente", res$n_ind),
         fila("Controles", res$controles), fila("Comunas", res$comunas))
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule",
                          filas_coef("Entrante de marca", res$sa_marca, res$se_sa_marca, res$p_sa_marca),
                          filas_coef("Entrante independiente", res$sa_ind, res$se_sa_ind, res$p_sa_ind),
                          filas_coef("Independiente $-$ de marca", res$sa_dif, res$se_sa_dif, res$p_sa_dif), "\\addlinespace",
                          fila("Valor $p$ bins previos, entrante de marca", sprintf("%.3f", res$pre_marca)),
                          fila("Valor $p$ bins previos, entrante independiente", sprintf("%.3f", res$pre_ind)), pie),
               "tabla_07_tipo_entrante.tex")
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule",
                          filas_coef("Entrante de marca", res$marca, res$se_marca, res$p_marca),
                          filas_coef("Entrante independiente $-$ de marca", res$dif, res$se_dif, res$p_dif),
                          filas_coef("Entrante independiente", res$ind, res$se_ind, res$p_ind), pie),
               "tabla_C16_tipo_entrante_twfe.tex")

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

# ---- Gráfico B.12: estudio de eventos TWFE por tipo de entrante (comparación) ----

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

# ---- Log: composición (las entradas de marca son más recientes) y robustez de la diferencia ----

print(unique(d[treated == 1L, .(station_key, tipo, anio = year(g_entry), rm = region == "metropolitana", dist_entry, n13, b_p93)])[
  , .(tratadas = .N, anio_medio = round(mean(anio), 1), desde_2021 = round(mean(anio >= 2021), 2), rm = round(mean(rm), 2),
      distancia = round(mean(dist_entry), 2), vecinas_2013 = round(mean(n13), 1), desvio_93_2013 = round(mean(b_p93, na.rm = TRUE), 2)), by = tipo])
for (fv in c("p93", "pdi")) for (mu in c("cohortes hasta 2019", "sin Región Metropolitana")) {
  x <- d[!is.na(get(fv))]
  x <- if (mu == "sin Región Metropolitana") x[region != "metropolitana"] else x[treated == 0L | year(g_evento) <= 2019L]
  a <- att_sa(estimar_sa(x, fv), x)
  message(sprintf("%s, %s: de marca %.3f (%.3f), diferencia independiente %.3f (%.3f)", PRECIOS[[fv]], mu,
                  a[efecto == "grupo0", est], a[efecto == "grupo0", se], a[efecto == "dif1", est], a[efecto == "dif1", se]))
}

# ---- Gráficos B.15 a B.17: Sun y Abraham por tipo de entrante en cada ventana ----

SA_VENTANAS <- c("2012_2026" = "grafico_B15_sa_tipo_2012_2026.pdf", "2014_2019" = "grafico_B16_sa_tipo_2014_2019.pdf",
                 "2021_2026" = "grafico_B17_sa_tipo_2021_2026.pdf")
for (v in names(SA_VENTANAS)) {
  dv <- con_tipo(muestra_estacion(leer_panel(v), CTRL_ESTRICTO, VENTANAS[[v]]$focal))
  es <- rbindlist(lapply(names(PRECIOS), function(fv) {
    z <- dv[!is.na(get(fv))]
    m <- estimar_sa(z, fv)
    a <- att_sa(m, z)
    u <- z[obs(m)]
    for (g in 0:1)
      message(sprintf("Ventana %s, %s, %s: %d tratadas, ATT %.3f (%.3f), valor p bins previos %s", v, PRECIOS[[fv]], TIPOS[g + 1L],
                      uniqueN(u[gtipo %in% g, station_key]), a[efecto == paste0("grupo", g), est], a[efecto == paste0("grupo", g), se],
                      p_previos(m, "per", z, g)))
    message(sprintf("Ventana %s, %s: diferencia %.3f (%.3f)", v, PRECIOS[[fv]], a[efecto == "dif1", est], a[efecto == "dif1", se]))
    agregar_sa(m, z)[, `:=`(grupo = TIPOS[grupo + 1L], combustible = PRECIOS[[fv]])]
  }))
  es[, `:=`(combustible = factor(combustible, levels = PRECIOS), grupo = factor(grupo, levels = TIPOS))]
  print(dcast(es, combustible + event_time ~ grupo, value.var = "estimate", fun.aggregate = function(x) round(x, 3)))
  guardar(grafico_es(es, "grupo"), SA_VENTANAS[[v]], alto = 3.2)
}

# ---- Log: Tabla 7 sin excluir las entradas de la propia marca (nota al pie) ----

entradas <- fread(file.path(dir_ventana("2012_2026"), "entradas.csv"))[, g := as.IDate(g)]
dm <- muestra_anillos(leer_panel("2012_2026"), entradas, c(0, R_TRATADA), VENTANAS[["2012_2026"]]$focal,
                      R_ctrl = R_CTRL_EST, excluir_misma = FALSE)
message(sprintf("Con entradas de la propia marca: %d tratadas, %d por una entrada de su propia marca; %d controles",
                uniqueN(dm[treated == 1L, station_key]), uniqueN(dm[treated == 1L & misma == 1L, station_key]),
                uniqueN(dm[treated == 0L, station_key])))
GRUPOS_MM <- list("dos grupos: de marca, independiente" = function(x) x$indep,
                  "tres grupos: de marca, independiente, propia marca" = function(x) fifelse(x$misma == 1L, 2L, x$indep))
for (nm in names(GRUPOS_MM)) for (fv in names(PRECIOS)) {
  x <- copy(dm)[, grp := fifelse(treated == 1L, GRUPOS_MM[[nm]](.SD), NA_integer_)]
  z <- cohortes_sunab(x, "grp")[!is.na(get(fv))]
  m <- estimar_sa(z, fv)
  a <- att_sa(m, z)
  message(sprintf("Propia marca incluida, %s, %s: %s; tratadas %s; valor p bins previos %s", nm, PRECIOS[[fv]],
                  paste(sprintf("%s %.3f (%.3f)", a$efecto, a$est, a$se), collapse = "; "),
                  paste(z[obs(m)][treated == 1L, uniqueN(station_key), keyby = grp]$V1, collapse = "/"),
                  paste(sapply(sort(unique(z[treated == 1L, grp])), function(g) p_previos(m, "per", z, g)), collapse = "/")))
}
