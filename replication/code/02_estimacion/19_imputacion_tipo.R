# 19_imputacion_tipo.R
#
# Estimador de imputación de Borusyak, Jaravel y Spiess (2024) por tipo de entrante, con la muestra
# y los efectos fijos de 17_tipo_entrante.R. Los efectos fijos se estiman en las observaciones no
# tratadas (controles y tratadas antes de la entrada); el efecto es observado - imputado, promediado
# por tipo y por bin desde la entrada. Errores estándar: bootstrap por comuna (NBOOT réplicas).
# run_all.R no lo corre por su tiempo (unos 10 minutos); se corre a mano desde replication/:
#   Rscript code/02_estimacion/19_imputacion_tipo.R > logs/19_imputacion_tipo.log 2>&1
#   Gráfico B.18: estudio de eventos por tipo; el log imprime el ATT por tipo y su diferencia.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/grafico_B18_imputacion_tipo.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)
set.seed(20261107)

TIPOS <- c("De marca", "Independiente")
FV <- c(p93 = "Gasolina 93", pdi = "Diésel")
NBOOT <- 199L
ATT <- 99L   # marca de la fila del ATT en la columna rel

d <- muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal)
d[, post := as.integer(treated == 1L & ym >= g_evento)]
d[, tipo := fifelse(treated == 1L, TIPOS[1L + !(entrante_marca %in% MARCAS_MAYORES)], NA_character_)]

imputa <- function(z, fv) {
  z <- copy(z)[, y := log(get(fv)) * 100]
  m <- feols(as.formula(sprintf("y ~ 1 | %s", FE_PRINCIPAL)), data = z[post == 0L], notes = FALSE, warn = FALSE)
  t <- z[post == 1L]
  t[, tau := y - predict(m, newdata = t)][!is.na(tau)]   # NA: algún efecto fijo sin observaciones no tratadas
}
resumen <- function(t) rbind(t[, .(rel = ATT, att = mean(tau)), by = tipo], t[, .(att = mean(tau)), by = .(tipo, rel)],
                             data.table(tipo = "dif", rel = ATT, att = t[tipo == TIPOS[2], mean(tau)] - t[tipo == TIPOS[1], mean(tau)]))

imp <- rbindlist(lapply(names(FV), function(fv) {
  z <- d[!is.na(get(fv))]
  t <- imputa(z, fv)
  sin <- z[post == 1L][!t, on = .(station_key, ym)]
  message(sprintf("%s: %d de %d obs. tratadas después de la entrada sin imputar (%d tratadas; %s)", FV[[fv]], nrow(sin),
                  z[post == 1L, .N], uniqueN(sin$station_key), paste(sin[, .N, by = tipo][, paste(tipo, N)], collapse = ", ")))
  coms <- unique(z$comuna)
  bs <- rbindlist(lapply(seq_len(NBOOT), function(b) {
    s <- data.table(comuna = sample(coms, replace = TRUE))[, copia := .I]
    zb <- z[s, on = "comuna", allow.cartesian = TRUE][, station_key := paste(station_key, copia)]   # cada copia, otro cluster
    resumen(imputa(zb, fv))
  }))
  resumen(t)[bs[, .(se = sd(att)), by = .(tipo, rel)], on = .(tipo, rel)][, fv := fv]
}))
print(imp[rel == ATT, .(fv, tipo, att = round(att, 3), se = round(se, 3), p = round(2 * pnorm(-abs(att / se)), 4))])   # cifras que cita el texto
print(dcast(imp[rel != ATT & tipo %in% TIPOS], fv + rel ~ tipo, value.var = "att", fun.aggregate = function(v) round(v, 3)))

g <- imp[rel != ATT & tipo %in% TIPOS, .(event_time = rel, estimate = att, se, grupo = factor(tipo, levels = TIPOS),
                                         combustible = factor(FV[fv], levels = FV))]
guardar(grafico_es(g, "grupo") + scale_x_continuous(breaks = 0:NBIN, labels = c(0:(NBIN - 1), sprintf("≥%d", NBIN))),
        "grafico_B18_imputacion_tipo.pdf", alto = 3)
