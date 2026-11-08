# 04_atenuacion.R
#
# Alcance espacial del efecto, Sun y Abraham (cohortes anuales).
#   Gráfico 6: efecto de los primeros BIN_M meses por anillo de distancia (0-2, 2-4 y 4-6 km) a la primera
#     entrada competidora a <= 6 km (muestra_anillos()), en una sola regresión con un grupo por anillo.
#     Las tratadas aportan los BIN_M meses antes y después de la entrada; los controles (sin entradas de
#     ninguna marca a <= 6 km) aportan todos sus meses.
#   Gráfico 10: lo mismo con un grupo por anillo y tipo de la entrante (independiente: ni Copec, ni
#     Shell/Enex, ni Aramco/Petrobras).
#   Tabla C.11: equivalencia de los anillos de 2 a 6 km. TOST con margen de equivalencia igual a la
#     mitad del ATT estático principal del combustible: el anillo es equivalente a cero si su intervalo
#     al 90 % cae dentro de +-margen.
#   Tabla 6: ATT estático de Sun y Abraham con la tratada definida por una entrada a <= 1, 2, 3 o
#     5 km; mismo control en todos los radios, por lo que solo cambia la definición de tratada.
#
# Entrada: data/processed/panel_2012_2026/{panel_mensual,entradas}.csv
# Salida:  outputs/grafico_06_atenuacion_distancia.pdf, outputs/grafico_10_atenuacion_tipo.pdf,
#          outputs/tabla_C11_equivalencia_anillos.tex, outputs/tabla_06_att_radios.tex

source(here::here("code", "00_configuracion.R"))
library(fixest)

CORTES <- c(0, 2, 4, 6)   # km
ANILLOS <- sprintf("%g-%g km", head(CORTES, -1), CORTES[-1])
TIPOS <- c("De marca", "Independiente")
RADIOS <- sort(c(RADIOS_EXTRA, R_TRATADA, R_CTRL_EST))
V <- VENTANAS[["2012_2026"]]

panel <- leer_panel("2012_2026")
entradas <- leer_csv(file.path(dir_ventana("2012_2026"), "entradas.csv")) |> mutate(g = as.Date(g))
da <- muestra_anillos(panel, entradas, CORTES, V$focal) |>
  filter(treated == 0L | between(miym - mi(g_evento), -BIN_M, BIN_M - 1L)) |>   # primeros BIN_M meses
  mutate(anillo_tipo = anillo * 2L + indep)

# ATT de cada grupo en una regresión de Sun y Abraham
att_anillos <- function(fv, grupo) {
  z <- cohortes_sunab(filter(da, !is.na(.data[[fv]])), grupo)
  m <- estimar_sa(z, fv)
  a <- att_sa(m, z) |> filter(startsWith(efecto, "grupo")) |> mutate(codigo = as.integer(sub("grupo", "", efecto)))
  u <- slice(z, obs(m))
  message(sprintf("%s, %s: tratadas por grupo %s; %d controles", grupo, PRECIOS[[fv]],
                  paste(u |> filter(treated == 1L) |> summarise(n = n_distinct(station_key), .by = all_of(grupo)) |>
                          arrange(.data[[grupo]]) |> pull(n), collapse = "/"), n_estaciones(m, z)[2]))
  a |> transmute(codigo, estimate = est, se, p, combustible = factor(PRECIOS[[fv]], levels = PRECIOS), fv = fv, gl = n_comunas(m, z) - 1L)
}
fig_anillos <- function(r, serie = NULL) {
  f <- ggplot(r, aes(anillo, estimate)) +
    geom_hline(yintercept = 0, colour = COLOR_REF) +
    geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), size = 0.3,
                    position = position_dodge(width = if (is.null(serie)) 0 else 0.4)) +
    facet_wrap(~combustible, scales = "free_y") +
    labs(x = "Distancia a la entrada", y = "Efecto sobre el precio (%)", colour = NULL, shape = NULL)
  if (is.null(serie)) f else f + aes(colour = .data[[serie]], shape = .data[[serie]])
}

# ---- Gráfico 6: atenuación con la distancia ----

redondear <- function(d) mutate(d, across(where(is.numeric), \(v) round(v, 3)))
res <- map(names(PRECIOS), att_anillos, grupo = "anillo") |>
  list_rbind() |>
  mutate(anillo = factor(ANILLOS[codigo + 1L], levels = ANILLOS))
res |>   # cifras que cita el texto
  pivot_wider(id_cols = combustible, names_from = anillo, values_from = c(estimate, se)) |>
  arrange(combustible) |> redondear() |> print()
guardar(fig_anillos(res), "grafico_06_atenuacion_distancia.pdf", alto = 3.2)

# ---- Gráfico 10: atenuación por tipo de entrante ----

rt <- map(names(PRECIOS), att_anillos, grupo = "anillo_tipo") |>
  list_rbind() |>
  mutate(anillo = factor(ANILLOS[codigo %/% 2L + 1L], levels = ANILLOS), tipo = factor(TIPOS[codigo %% 2L + 1L], levels = TIPOS))
rt |>
  pivot_wider(id_cols = c(combustible, anillo), names_from = tipo, values_from = c(estimate, se, p)) |>
  arrange(combustible, anillo) |> redondear() |> print()
guardar(fig_anillos(rt, "tipo"), "grafico_10_atenuacion_tipo.pdf", alto = 3.4)

# ---- Tabla 6: ATT estático por radio de tratamiento ----

att_radio <- map(RADIOS, function(r) {
  d <- cohortes_sunab(muestra_estacion(panel, CTRL_ESTRICTO, V$focal, radio = r))
  map(names(PRECIOS), function(fv) {
    x <- filter(d, !is.na(.data[[fv]]))
    m <- estimar_sa(x, fv)
    ct <- att_sa(m, x)
    tibble(radio = r, fv = fv, att = ct$est, se = ct$se, p = ct$p,
           tratadas = n_estaciones(m, x)[1], controles = n_estaciones(m, x)[2])
  }) |> list_rbind()
}) |> list_rbind()
print(att_radio)
message(sprintf("Tabla 6: tratadas a 3 km (cohortes focales) con una entrada de 2013 a <= 5 km, fuera a 5 km: %d",
                n_distinct(filter(panel, base == TRUE, !is.na(g_entry_r3), year(g_entry_r3) >= V$focal, year(g5_entry) < V$focal)$station_key)))

cuerpo <- unlist(lapply(RADIOS, function(r) {
  a <- att_radio |> filter(radio == r) |> slice(match(names(PRECIOS), fv))
  c(filas_coef(sprintf("Entrada a %d km o menos", r), a$att, a$se, a$p),
    fila("\\quad Tratadas", a$tratadas), "\\addlinespace")
}))
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule", cuerpo, "\\midrule",
                          fila("Controles", att_radio |> filter(radio == R_TRATADA) |> slice(match(names(PRECIOS), fv)) |> pull(controles))),
               "tabla_06_att_radios.tex")

# ---- Tabla C.11: equivalencia de los anillos lejanos ----

LEJANOS <- ANILLOS[-1]
margen <- att_radio |> filter(radio == R_TRATADA) |> transmute(fv, delta = abs(att) / 2)
eq <- res |>
  filter(anillo %in% LEJANOS) |>
  left_join(margen, by = "fv") |>
  mutate(lo = estimate - qt(0.95, gl) * se, hi = estimate + qt(0.95, gl) * se) |>
  mutate(p_tost = pmax(pt((estimate + delta) / se, gl, lower.tail = FALSE), pt((estimate - delta) / se, gl)))
print(eq)

cuerpo <- unlist(lapply(LEJANOS, function(a) {
  e <- eq |> filter(anillo == a) |> slice(match(names(PRECIOS), fv))
  c(fila(sprintf("Anillo %s", a), sprintf("%.3f", e$estimate)),
    fila("\\quad Intervalo al 90\\%", sprintf("[%.3f; %.3f]", e$lo, e$hi)),
    fila("\\quad Valor $p$ de equivalencia (TOST)", sprintf("%.3f", e$p_tost)), "\\addlinespace")
}))
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule", cuerpo, "\\midrule",
                          fila("Margen de equivalencia ($\\pm$)", sprintf("%.3f", margen |> slice(match(names(PRECIOS), fv)) |> pull(delta)))),
               "tabla_C11_equivalencia_anillos.tex")
