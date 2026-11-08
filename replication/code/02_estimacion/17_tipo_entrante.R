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
  stopifnot("tratada sin marca de entrante" = !anyNA(dd$entrante_marca[dd$treated == 1L]))
  dd |>
    mutate(tipo = if_else(treated == 1L, TIPOS[1L + !(entrante_marca %in% MARCAS_MAYORES)], NA_character_)) |>
    mutate(gtipo = match(tipo, TIPOS) - 1L) |>
    cohortes_sunab("gtipo")
}
d <- con_tipo(muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal)) |>
  mutate(post = as.integer(treated == 1L & ym >= g_entry), independiente = as.integer(tipo %in% TIPOS[2]))
message("Tratadas por tipo de entrante: ",
        paste(d |> filter(treated == 1L) |> summarise(n = n_distinct(station_key), .by = tipo) |> mutate(txt = paste(tipo, n)) |> pull(txt),
              collapse = ", "))
message("Marcas de las entrantes independientes: ")
d |> filter(tipo == TIPOS[2]) |> distinct(station_key, entrante_marca) |> summarise(N = n(), .by = entrante_marca) |>
  arrange(desc(N)) |> print(n = Inf)

# grupos del anexo, fijados en 2013 (antes de toda cohorte focal); mediana entre tratadas
b13 <- d |>
  filter(year == 2013L) |>
  summarise(n13 = mean(n_vecinas), b_p93 = mean(p93_dev, na.rm = TRUE), b_pdi = mean(pdi_dev, na.rm = TRUE), .by = station_key)
d <- left_join(d, b13, by = "station_key")
med <- function(v) d |> filter(treated == 1L, is.finite(.data[[v]])) |> select(station_key, all_of(v)) |> distinct() |> pull(all_of(v)) |> median()
d <- d |>
  mutate(incumbente = if_else(treated == 1L, if_else(distribuidor == "copec", "Copec", "Otra marca"), NA_character_),
         densidad = if_else(treated == 1L, if_else(n13 > med("n13"), "Más densa", "Menos densa"), NA_character_))
for (fv in c("p93", "pdi")) {
  b <- paste0("b_", fv)
  d[[paste0("previo_", fv)]] <- if_else(d$treated == 1L & is.finite(d[[b]]),
                                        if_else(d[[b]] <= med(b), "Barata en 2013", "Cara en 2013"), NA_character_)
}

# Sun y Abraham con un grupo por nivel de `grupo` (orden alfabético); las tratadas sin grupo quedan
# fuera, los controles no. Devuelve los datos con cohorte y per y los niveles.
sa_por_grupo <- function(z, grupo) {
  x <- filter(z, treated == 0L | !is.na(.data[[grupo]]))
  niv <- sort(unique(na.omit(x[[grupo]])))
  x <- mutate(x, gcod = match(.data[[grupo]], niv) - 1L)
  list(x = cohortes_sunab(x, "gcod"), niv = niv)
}

# regresión de distribución con kappa por grupo (umbrales: cuantiles de la muestra conjunta completa)
kappa_grupo <- function(fv, grupo) {
  y <- paste0(fv, "_dev")
  z <- filter(d, !is.na(.data[[y]]))
  cs <- quantile(z[[y]], Q, names = FALSE)
  ind <- sprintf("ind%02d", seq_along(cs))
  z <- bind_cols(z, as_tibble(map(set_names(cs, ind), \(cc) as.numeric(z[[y]] <= cc))))
  g <- sa_por_grupo(z, grupo)
  m <- feols(as.formula(sprintf("c(%s) ~ sunab(cohorte, per) | %s", paste(ind, collapse = ", "), FE_PRINCIPAL)),
             data = g$x, cluster = ~comuna)
  obs_post <- map(g$niv, \(k) g$x[[y]][g$x$post == 1L & g$x[[grupo]] %in% k])
  map(seq_along(ind), function(j) {
    a <- att_sa(m[[j]], g$x)
    dif <- filter(a, startsWith(efecto, "dif"))
    grp <- filter(a, startsWith(efecto, "grupo"))
    se_dif <- c(0, dif$se); dif_est <- c(0, dif$est)
    tibble(combustible = PRECIOS[[fv]], q = Q[j], c = cs[j], grupo = g$niv,
           kappa = grp$est, se = grp$se,
           observada = map_dbl(obs_post, \(v) mean(v <= cs[j])),
           se_dif = se_dif, dif = dif_est)
  }) |>
    list_rbind()
}

# ---- Tabla 7 (Sun y Abraham) y Tabla C.16 (TWFE): ATT estático por tipo de entrante ----

res <- map(names(PRECIOS), function(fv) {
  x <- filter(d, !is.na(.data[[fv]]))
  m <- estimar_sa(x, fv)
  a <- att_sa(m, x)
  for (g in 0:1) {   # cifras que cita el texto
    pre <- agregar_sa(m, x) |> filter(grupo == g, event_time < -1L)
    message(sprintf("Tabla 7, %s, %s: bins previos %s", PRECIOS[[fv]], TIPOS[g + 1L],
                    paste(sprintf("%d: %.3f (p %.3f)", pre$event_time, pre$estimate, pre$p), collapse = "; ")))
  }
  mt <- estimar_es(x, fv, "post + post:independiente")
  tw <- coeftable(mt); b <- coef(mt); V <- vcov(mt)
  u <- slice(x, obs(m))
  ef <- function(e, col) a[[col]][a$efecto == e]
  tibble(fv = fv,
         sa_marca = ef("grupo0", "est"), se_sa_marca = ef("grupo0", "se"), p_sa_marca = ef("grupo0", "p"),
         sa_ind = ef("grupo1", "est"), se_sa_ind = ef("grupo1", "se"), p_sa_ind = ef("grupo1", "p"),
         sa_dif = ef("dif1", "est"), se_sa_dif = ef("dif1", "se"), p_sa_dif = ef("dif1", "p"),
         pre_marca = as.numeric(p_previos(m, "per", x, 0L)), pre_ind = as.numeric(p_previos(m, "per", x, 1L)),
         marca = tw["post", 1], se_marca = tw["post", 2], p_marca = tw["post", 4],
         dif = tw["post:independiente", 1], se_dif = tw["post:independiente", 2], p_dif = tw["post:independiente", 4],
         ind = sum(b), se_ind = sqrt(sum(V)), p_ind = 2 * pt(-abs(sum(b) / sqrt(sum(V))), n_comunas(m, x) - 1L),
         n_marca = n_distinct(u$station_key[u$tipo %in% TIPOS[1]]), n_ind = n_distinct(u$station_key[u$tipo %in% TIPOS[2]]),
         controles = n_distinct(u$station_key[u$treated == 0L]), comunas = n_distinct(u$comuna))
}) |>
  list_rbind()
res |> mutate(across(where(is.numeric), \(v) round(v, 3))) |> print()

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

k_tipo <- map(names(PRECIOS), kappa_grupo, grupo = "tipo") |>
  list_rbind() |>
  mutate(combustible = factor(combustible, levels = PRECIOS))
sel <- map_dbl(c(0.10, 0.25, 0.50, 0.75, 0.90), \(v) Q[which.min(abs(Q - v))])
message("Gráfico 9: kappa (pp) en los cuantiles 10/25/50/75/90, y umbrales con diferencia significativa al 5% (de 41)")
k_tipo |> filter(q %in% sel) |> mutate(q = round(q, 2), kappa = round(100 * kappa, 1)) |>
  pivot_wider(id_cols = c(combustible, grupo), names_from = q, values_from = kappa) |> arrange(combustible, grupo) |> print()
k_tipo |> filter(grupo == TIPOS[2]) |>
  summarise(sig5 = sum(abs(dif / se_dif) > 1.96), positivas = sum(dif / se_dif > 1.96), .by = combustible) |> print()
# integral de kappa sobre c: caída del precio medio ($/L) dentro de los umbrales
k_tipo |> arrange(q) |>
  summarise(integral = round(sum(diff(c) * (head(kappa, -1) + tail(kappa, -1)) / 2), 2), .by = c(combustible, grupo)) |> print()

grafico_kappa <- function(k, facetas = ~combustible) {
  ggplot(k, aes(q, 100 * kappa, colour = grupo, shape = grupo)) +
    geom_hline(yintercept = 0, colour = COLOR_REF) +
    geom_pointrange(aes(ymin = 100 * (kappa - 1.96 * se), ymax = 100 * (kappa + 1.96 * se)), size = 0.12,
                    position = position_dodge(width = 0.012)) +
    facet_wrap(facetas) +
    labs(x = "Cuantil del umbral en la muestra conjunta", y = "Efecto sobre la distribución acumulada (pp)", colour = NULL, shape = NULL)
}
SERIES <- c("Con entrada (observada)", "Sin entrada (contrafactual)")
cdf <- bind_rows(transmute(k_tipo, combustible, grupo, c, F = observada, serie = SERIES[1]),
                 transmute(k_tipo, combustible, grupo, c, F = observada - kappa, serie = SERIES[2])) |>
  mutate(serie = factor(serie, levels = SERIES),
         grupo = factor(sprintf("Entrante %s", tolower(grupo)), levels = sprintf("Entrante %s", tolower(TIPOS))))
fig <- ggplot(cdf, aes(c, F, colour = serie, linetype = serie)) +
  geom_line() +
  facet_grid(grupo ~ combustible, scales = "free_x") +
  labs(x = "Precio menos su media nacional del mes ($/L)", y = "Distribución acumulada", colour = NULL, linetype = NULL)
guardar(fig, "grafico_09_distribucion_entrante.pdf", alto = 4.2)
guardar(grafico_kappa(k_tipo), "grafico_B13_kappa_tipo_entrante.pdf", alto = 3.6)

# ---- Gráfico B.12: estudio de eventos TWFE por tipo de entrante (comparación) ----

d <- mutate(d, t_marca = treated * (tipo %in% TIPOS[1]), t_ind = treated * (tipo %in% TIPOS[2]))
es <- map(names(PRECIOS), function(fv) {
  m <- estimar_es(filter(d, !is.na(.data[[fv]])), fv, "i(rel, t_marca, ref = -1) + i(rel, t_ind, ref = -1)")
  message(sprintf("Gráfico B.12, %s: valor p bins previos de marca %.3f, independiente %.3f", PRECIOS[[fv]],
                  wald(m, keep = "^rel::-[234]:t_marca$", print = FALSE)$p, wald(m, keep = "^rel::-[234]:t_ind$", print = FALSE)$p))
  ct <- coeftable(m)
  tibble(event_time = as.integer(sub("^rel::(-?\\d+):.*", "\\1", rownames(ct))),
         grupo = TIPOS[grepl("t_ind$", rownames(ct)) + 1L], estimate = ct[, 1], se = ct[, 2]) |>
    bind_rows(tibble(event_time = -1L, grupo = TIPOS, estimate = 0, se = 0)) |>
    mutate(combustible = PRECIOS[[fv]])
}) |>
  list_rbind() |>
  mutate(combustible = factor(combustible, levels = PRECIOS), grupo = factor(grupo, levels = TIPOS))
es |> pivot_wider(id_cols = c(combustible, event_time), names_from = grupo, values_from = estimate) |>
  arrange(combustible, event_time) |> mutate(across(where(is.numeric), \(v) round(v, 3))) |> print(n = Inf)
guardar(grafico_es(es, "grupo"), "grafico_B12_es_tipo_entrante.pdf", alto = 3.2)

# ---- Gráfico B.14: otras dimensiones, gasolina 93 y diésel ----

# filas: (1) frente a (2), en el orden alfabético de los grupos que usa kappa_grupo
DIMS <- c(incumbente = "Marca: Copec (1), otra (2)", densidad = "Estaciones a 2 km en 2013: más (1), menos (2)",
          previo = "Precio en 2013: barata (1), cara (2)")
otras <- map(c("p93", "pdi"), function(fv) map(names(DIMS), function(dm) {
  grupo <- if (dm == "previo") paste0("previo_", fv) else dm
  kappa_grupo(fv, grupo) |> mutate(dimension = DIMS[[dm]])
}) |> list_rbind()) |>
  list_rbind() |>
  mutate(combustible = factor(combustible, levels = PRECIOS), dimension = factor(dimension, levels = DIMS)) |>
  mutate(orden = match(grupo, sort(unique(grupo))), .by = dimension)
stopifnot("el orden de los grupos no calza con las etiquetas de las filas" =
            all(filter(otras, orden == 1L)$grupo %in% c("Copec", "Más densa", "Barata en 2013")))
otras |> filter(q %in% sel) |> mutate(q = round(q, 2), kappa = round(100 * kappa, 1)) |>
  pivot_wider(id_cols = c(combustible, dimension, grupo), names_from = q, values_from = kappa) |> print()
otras |> arrange(q) |>
  summarise(integral = round(sum(diff(c) * (head(kappa, -1) + tail(kappa, -1)) / 2), 2),
            sig5 = sum(se_dif > 0 & abs(dif / se_dif) > 1.96), .by = c(combustible, dimension, grupo)) |> print()
fig <- grafico_kappa(mutate(otras, grupo = factor(sprintf("Grupo (%d)", orden)))) +
  facet_grid(dimension ~ combustible, labeller = label_wrap_gen(24))
guardar(fig, "grafico_B14_distribucion_otras.pdf", alto = 6.5, ancho = 6.2)

# ---- Log: composición (las entradas de marca son más recientes) y robustez de la diferencia ----

d |>
  filter(treated == 1L) |>
  transmute(station_key, tipo, anio = year(g_entry), rm = region == "metropolitana", dist_entry, n13, b_p93) |>
  distinct() |>
  summarise(tratadas = n(), anio_medio = round(mean(anio), 1), desde_2021 = round(mean(anio >= 2021), 2), rm = round(mean(rm), 2),
            distancia = round(mean(dist_entry), 2), vecinas_2013 = round(mean(n13), 1), desvio_93_2013 = round(mean(b_p93, na.rm = TRUE), 2),
            .by = tipo) |> print()
for (fv in c("p93", "pdi")) for (mu in c("cohortes hasta 2019", "sin Región Metropolitana")) {
  x <- filter(d, !is.na(.data[[fv]]))
  x <- if (mu == "sin Región Metropolitana") filter(x, region != "metropolitana") else filter(x, treated == 0L | year(g_evento) <= 2019L)
  a <- att_sa(estimar_sa(x, fv), x)
  message(sprintf("%s, %s: de marca %.3f (%.3f), diferencia independiente %.3f (%.3f)", PRECIOS[[fv]], mu,
                  a$est[a$efecto == "grupo0"], a$se[a$efecto == "grupo0"], a$est[a$efecto == "dif1"], a$se[a$efecto == "dif1"]))
}

# ---- Gráficos B.15 a B.17: Sun y Abraham por tipo de entrante en cada ventana ----

SA_VENTANAS <- c("2012_2026" = "grafico_B15_sa_tipo_2012_2026.pdf", "2014_2019" = "grafico_B16_sa_tipo_2014_2019.pdf",
                 "2021_2026" = "grafico_B17_sa_tipo_2021_2026.pdf")
for (v in names(SA_VENTANAS)) {
  dv <- con_tipo(muestra_estacion(leer_panel(v), CTRL_ESTRICTO, VENTANAS[[v]]$focal))
  es <- map(names(PRECIOS), function(fv) {
    z <- filter(dv, !is.na(.data[[fv]]))
    m <- estimar_sa(z, fv)
    a <- att_sa(m, z)
    u <- slice(z, obs(m))
    for (g in 0:1)
      message(sprintf("Ventana %s, %s, %s: %d tratadas, ATT %.3f (%.3f), valor p bins previos %s", v, PRECIOS[[fv]], TIPOS[g + 1L],
                      n_distinct(u$station_key[u$gtipo %in% g]), a$est[a$efecto == paste0("grupo", g)], a$se[a$efecto == paste0("grupo", g)],
                      p_previos(m, "per", z, g)))
    message(sprintf("Ventana %s, %s: diferencia %.3f (%.3f)", v, PRECIOS[[fv]], a$est[a$efecto == "dif1"], a$se[a$efecto == "dif1"]))
    agregar_sa(m, z) |> mutate(grupo = TIPOS[grupo + 1L], combustible = PRECIOS[[fv]])
  }) |>
    list_rbind() |>
    mutate(combustible = factor(combustible, levels = PRECIOS), grupo = factor(grupo, levels = TIPOS))
  es |> pivot_wider(id_cols = c(combustible, event_time), names_from = grupo, values_from = estimate) |>
    arrange(combustible, event_time) |> mutate(across(where(is.numeric), \(x) round(x, 3))) |> print(n = Inf)
  guardar(grafico_es(es, "grupo"), SA_VENTANAS[[v]], alto = 3.2)
}

# ---- Log: Tabla 7 sin excluir las entradas de la propia marca (nota al pie) ----

entradas <- leer_csv(file.path(dir_ventana("2012_2026"), "entradas.csv")) |> mutate(g = as.Date(g))
dm <- muestra_anillos(leer_panel("2012_2026"), entradas, c(0, R_TRATADA), VENTANAS[["2012_2026"]]$focal,
                      R_ctrl = R_CTRL_EST, excluir_misma = FALSE)
message(sprintf("Con entradas de la propia marca: %d tratadas, %d por una entrada de su propia marca; %d controles",
                n_distinct(dm$station_key[dm$treated == 1L]), n_distinct(dm$station_key[dm$treated == 1L & dm$misma == 1L]),
                n_distinct(dm$station_key[dm$treated == 0L])))
GRUPOS_MM <- list("dos grupos: de marca, independiente" = function(x) x$indep,
                  "tres grupos: de marca, independiente, propia marca" = function(x) if_else(x$misma == 1L, 2L, x$indep))
for (nm in names(GRUPOS_MM)) for (fv in names(PRECIOS)) {
  x <- mutate(dm, grp = if_else(treated == 1L, GRUPOS_MM[[nm]](pick(everything())), NA_integer_))
  z <- cohortes_sunab(x, "grp") |> filter(!is.na(.data[[fv]]))
  m <- estimar_sa(z, fv)
  a <- att_sa(m, z)
  message(sprintf("Propia marca incluida, %s, %s: %s; tratadas %s; valor p bins previos %s", nm, PRECIOS[[fv]],
                  paste(sprintf("%s %.3f (%.3f)", a$efecto, a$est, a$se), collapse = "; "),
                  paste(slice(z, obs(m)) |> filter(treated == 1L) |> summarise(n = n_distinct(station_key), .by = grp) |>
                          arrange(grp) |> pull(n), collapse = "/"),
                  paste(map_chr(sort(unique(z$grp[z$treated == 1L])), \(g) p_previos(m, "per", z, g)), collapse = "/")))
}
