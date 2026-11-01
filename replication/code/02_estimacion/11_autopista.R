# 11_autopista.R
#
# Efecto de la entrada en estaciones a <= 100 m de la red vial clase 1 (autopistas y rutas
# nacionales) frente al resto, en una sola regresión. Se excluyen las tratadas cuya entrante
# está sobre la red.
#   Tabla C.6: efecto estático; "Entrada" es el efecto en el resto y "Entrada x autopista" la
#     diferencia; el efecto en autopista va como fila aparte.
#   Gráfico B.6: estudio de eventos con el tiempo desde la entrada interactuado con cada grupo.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/tabla_C06_autopista.tex, outputs/grafico_B06_autopista.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

GRUPOS <- c(sprintf("Resto (más de %d m)", AUTOPISTA_M), sprintf("Autopista (hasta %d m)", AUTOPISTA_M))

d <- muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal)
message(sprintf("Tratadas fuera por entrante a %d m o menos de la red: %d de %d", AUTOPISTA_M,
                uniqueN(d[entrante_autopista == TRUE, station_key]), uniqueN(d[treated == 1L, station_key])))
d <- d[!(entrante_autopista %in% TRUE)]

d[, cerca := as.integer(d_autopista <= AUTOPISTA_M)]
d[, post := as.integer(treated == 1L & ym >= g_entry)]
d[, `:=`(post_lejos = post * (1L - cerca), post_cerca = post * cerca,
         trat_lejos = treated * (1L - cerca), trat_cerca = treated * cerca)]

res <- lapply(names(PRECIOS), function(fv) {
  x <- d[!is.na(get(fv))]
  m_es <- estimar_es(x, fv, "i(rel, trat_lejos, ref = -1) + i(rel, trat_cerca, ref = -1)")
  ct <- coeftable(m_es)
  es <- data.table(event_time = as.integer(sub("^rel::(-?\\d+):.*", "\\1", rownames(ct))),
                   grupo = GRUPOS[grepl("trat_cerca$", rownames(ct)) + 1L], estimate = ct[, 1], se = ct[, 2])
  es <- rbind(es, data.table(event_time = -1L, grupo = GRUPOS, estimate = 0, se = 0))[, combustible := PRECIOS[[fv]]]

  att <- coeftable(estimar_es(x, fv, "post_lejos + post_cerca"))
  u <- x[obs(m_es)]
  list(es = es, m_dif = estimar_es(x, fv, "post + post:cerca"),
       resumen = data.table(att_autopista = att["post_cerca", 1], se_autopista = att["post_cerca", 2],
                            p_autopista = att["post_cerca", 4],
                            trat_resto = uniqueN(u[trat_lejos == 1L, station_key]),
                            trat_autopista = uniqueN(u[trat_cerca == 1L, station_key]),
                            controles = uniqueN(u[treated == 0L, station_key])))
})
resumen <- rbindlist(lapply(res, `[[`, "resumen"))

etable(lapply(res, `[[`, "m_dif"), tex = TRUE, headers = unname(PRECIOS), replace = TRUE,
       dict = c(post = "Entrada", `post:cerca` = "Entrada $\\times$ autopista"),
       extralines = list(
         `Efecto en autopista` = sprintf("%.3f%s (%.3f)", resumen$att_autopista, estrellas(resumen$p_autopista),
                                         resumen$se_autopista),
         `Tratadas resto` = resumen$trat_resto,
         `Tratadas autopista` = resumen$trat_autopista,
         Controles = resumen$controles),
       file = ruta_out("tabla_C06_autopista.tex"))

es <- rbindlist(lapply(res, `[[`, "es"))
es[, `:=`(combustible = factor(combustible, levels = PRECIOS), grupo = factor(grupo, levels = GRUPOS))]
guardar(grafico_es(es, "grupo"), "grafico_B06_autopista.pdf", alto = 3.2)
