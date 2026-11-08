# 08_traspaso.R
#
# Brecha acumulada del precio de las tratadas frente a los controles estrictos de su región
# en los días de actualización del MEPCO, gasolina 93, régimen semanal. En ese régimen las
# ventanas de cada actualización (u - 1 a u + 6) se encadenan, por lo que la suma acumulada
# de dif_log_reg es la brecha del precio frente a los controles. Se normaliza en la última
# actualización previa a una fecha de referencia y se promedia por tiempo desde la entrada.
# Solo tratadas cuya ventana cae entera en el régimen semanal y con todas sus actualizaciones.
#   Gráfico B.4: ventana de un año en bloques de cuatro semanas, con la entrada registrada o
#     adelantada uno y dos meses; cada panel se normaliza antes de su propia fecha.
#
# Entrada: data/processed/traspaso_actualizaciones.csv
# Salida:  outputs/grafico_B04_traspaso_adelantos.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

td <- fread(ruta_proc("traspaso_actualizaciones.csv"))[, u := as.IDate(u)]
setorder(td, station_key, u)
ee <- function(x) sd(x) / sqrt(length(x))

# brecha acumulada de cada tratada en las filas con t en [desde, hasta), normalizada en su
# última actualización con t < ref
# `col`: columna que se acumula (la muestra se elige siempre con la diferencia total)
brecha <- function(t, desde, hasta, ref, col = "dif_log_reg") {
  x <- td[, .(station_key, u, dif = dif_log_reg, parte = get(col), t = t)][t >= desde & t < hasta]
  x <- x[, if (max(u) < REFORMA_MEPCO && !anyNA(dif)) .SD, by = station_key]
  x[, C := cumsum(parte), by = station_key]
  x[, es_ref := if (any(t < ref)) t == max(t[t < ref]) else FALSE, by = station_key]
  x <- x[, if (any(es_ref)) .SD, by = station_key]
  x[, C := C - C[es_ref], by = station_key][]
}

# etiquetas del eje con los extremos agrupados; signos en Unicode, así se dibujan en la letra
# de la tesis (plotmath los toma de la fuente Symbol)
etiquetas <- function(marcas) {
  e <- sub("^-", "\u2212", as.character(marcas))
  e[c(1, length(e))] <- c(paste("\u2264", e[1]), paste("\u2265", e[length(e)]))
  e
}

# ---- Gráfico B.4 ----

BLOQUE <- 4L                  # semanas por punto
SEMANAS <- 60L                # datos a +-60 semanas de la fecha usada
HI16 <- 48L %/% BLOQUE        # extremos agrupados en +-12 bloques (un año)
FECHAS <- c(`0` = "Entrada registrada", `4` = "Entrada adelantada 1 mes", `8` = "Entrada adelantada 2 meses")

res <- rbindlist(lapply(as.integer(names(FECHAS)), function(s) {
  x <- brecha(td$k + s, -SEMANAS, SEMANAS, 0L)    # t: semanas desde la entrada adelantada s semanas
  x[, b := pmin(pmax(t %/% BLOQUE, -HI16), HI16)]
  x[, .(est = mean(C), se = ee(C)), by = b][
    , `:=`(fecha = factor(FECHAS[[as.character(s)]], levels = FECHAS),
           registrada = s / BLOQUE - 0.5)]       # posición de la entrada registrada en el eje
}))

paso <- HI16 %/% 3L
marcas <- c(-HI16, rev(-seq(paso, HI16 - 1L, by = paso)), 0L, seq(paso, HI16 - 1L, by = paso), HI16)
fig <- ggplot(res, aes(b, est)) +
  geom_hline(yintercept = 0, colour = COLOR_REF) +
  geom_vline(xintercept = -0.5, linetype = "dashed", colour = COLOR_REF) +
  geom_vline(aes(xintercept = registrada), linetype = "dotted", colour = COLOR_REF) +
  geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), size = 0.2) +
  facet_wrap(~fecha, ncol = 1) +
  scale_x_continuous(breaks = marcas, labels = etiquetas(marcas)) +
  labs(x = "Bloques de 4 semanas desde la entrada (adelantada)", y = "Brecha acumulada (%)")
print(res[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)], nrows = Inf)   # cifras que cita el texto
guardar(fig, "grafico_B04_traspaso_adelantos.pdf", alto = 6)
