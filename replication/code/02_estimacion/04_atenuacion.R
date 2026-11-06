# 04_atenuacion.R
#
# Alcance espacial del efecto, especificación principal, control estricto.
#   Gráfico 6: efecto estático por anillo de distancia (0-1, ..., 4-5 km) a la primera entrada
#     competidora a <= 5 km. Las tratadas aportan los BIN_M meses antes y después de la entrada;
#     los controles (sin entradas de ninguna marca a <= 5 km) aportan todos sus meses.
#   Tabla C.11: equivalencia de los anillos de 3 a 5 km. TOST con margen
#     de equivalencia igual a la mitad del ATT estático principal del combustible: el anillo es
#     equivalente a cero si su intervalo al 90 % cae dentro de +-margen.
#   Tabla 6: ATT estático con la tratada definida por una entrada a <= 1, 2, 3 o 5 km; mismo
#     control en todos los radios, por lo que solo cambia la definición de tratada.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/grafico_06_atenuacion_distancia.pdf, outputs/tabla_C11_equivalencia_anillos.tex,
#          outputs/tabla_06_att_radios.tex

source(here::here("code", "00_configuracion.R"))
library(fixest)

ANILLOS <- c("0-1 km", "1-2 km", "2-3 km", "3-4 km", "4-5 km")
HORIZONTES <- BIN_M   # meses a cada lado de la entrada que aportan las tratadas
RADIOS <- sort(c(RADIOS_EXTRA, R_TRATADA, R_CTRL_EST))
V <- VENTANAS[["2012_2026"]]

panel <- leer_panel("2012_2026")
d5 <- muestra_estacion(panel, CTRL_ESTRICTO, V$focal, radio = R_CTRL_EST)
d5[, tpr := factor(fifelse(treated == 1L & ym >= g_evento, ring_entry, "none"), levels = c("none", ANILLOS))]

res <- rbindlist(lapply(HORIZONTES, function(h) {
  d <- d5[treated == 0L | (miym - mi(g_evento)) %between% c(-h, h - 1L)]
  rbindlist(lapply(names(PRECIOS), function(fv) {
    x <- d[!is.na(get(fv))]
    m <- estimar_es(x, fv, "i(tpr, ref = 'none')")
    u <- x[obs(m)]
    message(sprintf("Gráfico 6, %d meses, %s: tratadas por anillo %s; %d controles", h, PRECIOS[[fv]],
                    paste(u[treated == 1L, uniqueN(station_key), keyby = ring_entry][ANILLOS, V1], collapse = "/"),
                    n_estaciones(m, x)[2]))
    ct <- coeftable(m)
    data.table(anillo = factor(sub("^tpr::", "", rownames(ct)), levels = ANILLOS), estimate = ct[, 1], se = ct[, 2],
               combustible = factor(PRECIOS[[fv]], levels = PRECIOS), fv = fv,
               horizonte = sprintf("%d meses", h), gl = n_comunas(m, x) - 1L)
  }))
}))
res[, horizonte := factor(horizonte, levels = sprintf("%d meses", HORIZONTES))]
print(dcast(res, combustible + anillo ~ horizonte, value.var = c("estimate", "se"))[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

fig <- ggplot(res, aes(anillo, estimate)) +
  geom_hline(yintercept = 0, colour = COLOR_REF) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), size = 0.3) +
  facet_wrap(~combustible, scales = "free_y") +
  labs(x = "Distancia a la entrada", y = "Efecto sobre el precio (%)")
guardar(fig, "grafico_06_atenuacion_distancia.pdf", alto = 3.2)

# ---- Tabla 6: ATT estático por radio de tratamiento ----

att_radio <- rbindlist(lapply(RADIOS, function(r) {
  d <- muestra_estacion(panel, CTRL_ESTRICTO, V$focal, radio = r)
  d[, post := as.integer(treated == 1L & ym >= g_evento)]
  rbindlist(lapply(names(PRECIOS), function(fv) {
    x <- d[!is.na(get(fv))]
    m <- estimar_es(x, fv, "post")
    ct <- coeftable(m)["post", ]
    data.table(radio = r, fv = fv, att = ct[[1]], se = ct[[2]], p = ct[[4]],
               tratadas = n_estaciones(m, x)[1], controles = n_estaciones(m, x)[2])
  }))
}))
print(att_radio)

cuerpo <- unlist(lapply(RADIOS, function(r) {
  a <- att_radio[radio == r][match(names(PRECIOS), fv)]
  c(filas_coef(sprintf("Entrada a %d km o menos", r), a$att, a$se, a$p),
    fila("\\quad Tratadas", a$tratadas), "\\addlinespace")
}))
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule", cuerpo, "\\midrule",
                          fila("Controles", att_radio[radio == R_TRATADA][match(names(PRECIOS), fv), controles])),
               "tabla_06_att_radios.tex")

# ---- Tabla C.11: equivalencia de los anillos lejanos ----

LEJANOS <- c("3-4 km", "4-5 km")
margen <- att_radio[radio == R_TRATADA, .(fv, delta = abs(att) / 2)]
eq <- margen[res[horizonte == sprintf("%d meses", max(HORIZONTES)) & anillo %in% LEJANOS], on = "fv"]
eq[, `:=`(lo = estimate - qt(0.95, gl) * se, hi = estimate + qt(0.95, gl) * se)]
eq[, p_tost := pmax(pt((estimate + delta) / se, gl, lower.tail = FALSE), pt((estimate - delta) / se, gl))]
print(eq)

cuerpo <- unlist(lapply(LEJANOS, function(a) {
  e <- eq[anillo == a][match(names(PRECIOS), fv)]
  c(fila(sprintf("Anillo %s", a), sprintf("%.3f", e$estimate)),
    fila("\\quad Intervalo al 90\\%", sprintf("[%.3f; %.3f]", e$lo, e$hi)),
    fila("\\quad Valor $p$ de equivalencia (TOST)", sprintf("%.3f", e$p_tost)), "\\addlinespace")
}))
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule", cuerpo, "\\midrule",
                          fila("Margen de equivalencia ($\\pm$)", sprintf("%.3f", margen[match(names(PRECIOS), fv), delta]))),
               "tabla_C11_equivalencia_anillos.tex")
