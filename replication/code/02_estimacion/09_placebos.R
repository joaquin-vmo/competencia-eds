# 09_placebos.R
#
# Placebos y fechado de la entrada, ventana 2012_2026, especificación principal.
#   Gráfico 12: entrada fechada un período (seis meses) antes de la registrada, en meses,
#     con referencia en el mes previo a la entrada adelantada.
#   Gráfico B.5: estudio de eventos con la entrada registrada y fechada dos meses antes.
#   Tabla C.8: fechas de entrada falsas en el grupo de control. Cada réplica sortea la
#     proporción real de tratadas dentro del grupo, les asigna cohortes y desfases a la segunda
#     entrada de sus distribuciones reales, corta en la segunda entrada falsa y estima el ATT
#     estático. Sobre todo el grupo y sobre su mitad más densa (vecinas a <= 2 km).
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/grafico_12_placebo_adelanto.pdf, outputs/grafico_B05_es_adelanto_2meses.pdf,
#          outputs/tabla_C08_placebo_controles.tex

source(here::here("code", "00_configuracion.R"))
library(fixest)

panel <- leer_panel("2012_2026")
d <- muestra_estacion(panel, CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal)

# ==============================================================================
# Gráfico 12: entrada adelantada seis meses, resolución mensual
# ==============================================================================

DELTA <- BIN_M          # meses que se adelanta la entrada: un período
NB_M  <- NBIN * BIN_M   # meses a cada lado de la entrada registrada; los extremos se agrupan

# tiempo desde la entrada adelantada, para que la referencia -1 sea el mes previo a ella
d[, relp := fifelse(treated == 1L, pmax(-NB_M, pmin(NB_M, miym - mi(g_entry))) + DELTA, -1L)]
res <- rbindlist(lapply(names(PRECIOS), function(fv)
  tidy_es(estimar_es(d[!is.na(get(fv))], fv, "i(relp, treated, ref = -1)"), "relp")[, combustible := PRECIOS[[fv]]]))
res[, `:=`(mes = event_time - DELTA, combustible = factor(combustible, levels = PRECIOS))]

ENTRADAS <- data.table(x = c(-DELTA, 0), tipo = c("Entrada adelantada (placebo)", "Entrada registrada"))
fig <- ggplot(res, aes(mes, estimate)) +
  geom_hline(yintercept = 0, colour = COLOR_REF) +
  geom_vline(data = ENTRADAS, aes(xintercept = x, linetype = tipo), colour = COLOR_REF) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), size = 0.15) +
  facet_wrap(~combustible, scales = "free_y") +
  scale_x_continuous(breaks = seq(-NB_M, NB_M, BIN_M)) +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  labs(x = "Meses desde la entrada registrada", y = "Efecto sobre el precio (%)", linetype = NULL)
guardar(fig, "grafico_12_placebo_adelanto.pdf", alto = 3.4)

# ==============================================================================
# Gráfico B.5: entrada registrada y adelantada dos meses
# ==============================================================================

ADELANTO <- 2L   # meses
FECHAS <- c("Entrada registrada", "Entrada adelantada 2 meses")
d[, rel_adel := fifelse(treated == 1L, pmax(-NBIN, pmin(NBIN, (miym - (mi(g_entry) - ADELANTO)) %/% BIN_M)), -1L)]
res <- rbindlist(lapply(names(PRECIOS), function(fv) {
  x <- d[!is.na(get(fv))]
  rbind(tidy_es(estimar_es(x, fv))[, fecha := FECHAS[1]],
        tidy_es(estimar_es(x, fv, "i(rel_adel, treated, ref = -1)"), "rel_adel")[, fecha := FECHAS[2]])[
    , combustible := PRECIOS[[fv]]]
}))
res[, `:=`(combustible = factor(combustible, levels = PRECIOS), fecha = factor(fecha, levels = FECHAS))]
guardar(grafico_es(res, "fecha"), "grafico_B05_es_adelanto_2meses.pdf", alto = 3.6)

# ==============================================================================
# Tabla C.8: placebo con fechas de entrada falsas en el grupo de control
# ==============================================================================

B       <- 500L
SEMILLA <- 20260902L
POOLS   <- c(completo = "Todo el grupo de control", denso = "Mitad más densa del grupo de control")

set.seed(SEMILLA)
d[, post := as.integer(treated == 1L & ym >= g_entry)]

st <- unique(d[, .(station_key, treated, g_entry, g2_entry)])
prop_trat <- mean(st$treated)
coh_real <- mi(st[treated == 1L, g_entry])
d2_real  <- st[treated == 1L & !is.na(g2_entry), mi(g2_entry) - mi(g_entry)]

dens <- d[treated == 0L, .(n = mean(n_vecinas)), keyby = station_key]
pools <- list(completo = dens$station_key, denso = dens[n >= median(n), station_key])

FML <- as.formula(sprintf("c(%s) ~ post | %s", paste(sprintf("log(%s) * 100", names(PRECIOS)), collapse = ", "), FE_PRINCIPAL))
att <- function(datos) {
  m <- feols(FML, data = datos, cluster = ~comuna)
  rbindlist(lapply(seq_along(PRECIOS), function(j) {
    ct <- coeftable(m[[j]])["post", ]
    data.table(outcome = names(PRECIOS)[j], att = ct[[1]], se = ct[[2]], p = ct[[4]])
  }))
}

reales <- att(d)
reps <- rbindlist(lapply(names(POOLS), function(pool) {
  sk <- pools[[pool]]
  base <- d[station_key %in% sk, !c("g_entry", "g2_entry", "post")]
  k <- round(prop_trat * length(sk))
  rbindlist(lapply(seq_len(B), function(b) {
    g <- sample(coh_real, k, replace = TRUE)
    falsa <- data.table(station_key = sample(sk, k), g = g, g2 = g + sample(d2_real, k, replace = TRUE))
    x <- falsa[base, on = "station_key"][is.na(g2) | miym < g2]
    x[, post := as.integer(!is.na(g) & miym >= g)]
    att(x)[, `:=`(pool = pool, rep = b)]
  }))
}))

res <- reps[, .(media = mean(att), de = sd(att), se_medio = mean(se),
                q025 = quantile(att, 0.025), q975 = quantile(att, 0.975), rechazo = mean(p < 0.05),
                p_ri = mean(abs(att) >= abs(reales[outcome == .BY$outcome, att]))),
            by = .(pool, outcome)]

ordenar <- function(r) r[match(names(PRECIOS), outcome)]
num <- function(x, k = 3) sprintf("%.*f", k, x)
cuerpo <- unlist(lapply(names(POOLS), function(pl) {
  r <- ordenar(res[pool == pl])
  c(sprintf("\\multicolumn{5}{l}{\\emph{%s (%d estaciones)}} \\\\", POOLS[[pl]], length(pools[[pl]])),
    fila("\\quad ATT placebo medio", num(r$media)),
    fila("\\quad Desviación estándar de la distribución", num(r$de)),
    fila("\\quad Error estándar agrupado medio", num(r$se_medio)),
    fila("\\quad Cociente error estándar / desviación", num(r$se_medio / r$de, 2)),
    fila("\\quad Intervalo del 95\\% de la distribución", sprintf("[%.3f; %.3f]", r$q025, r$q975)),
    fila("\\quad Tasa de rechazo al 5\\%", num(r$rechazo)),
    fila("\\quad Valor $p$ por aleatorización del ATT real", num(r$p_ri)),
    "\\addlinespace")
}))
r <- ordenar(reales)
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule", cuerpo, "\\midrule",
                          filas_coef("ATT real", r$att, r$se, r$p)),
               "tabla_C08_placebo_controles.tex")
