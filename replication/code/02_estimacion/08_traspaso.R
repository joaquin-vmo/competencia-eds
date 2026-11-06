# 08_traspaso.R
#
# Brecha acumulada del precio de las tratadas frente a los controles estrictos de su región
# en los días de actualización del MEPCO, gasolina 93, régimen semanal. En ese régimen las
# ventanas de cada actualización (u - 1 a u + 6) se encadenan, por lo que la suma acumulada
# de dif_log_reg es la brecha del precio frente a los controles. Se normaliza en la última
# actualización previa a una fecha de referencia y se promedia por tiempo desde la entrada.
# Solo tratadas cuya ventana cae entera en el régimen semanal y con todas sus actualizaciones.
#   Gráfico 12: ventana de 24 semanas antes y 16 después de la entrada registrada, normalizada
#     ocho semanas antes (la respuesta empieza antes de la entrada registrada).
#     La brecha total se separa en la parte acumulada el día de actualización y el siguiente
#     (días 0 y 1) y la del resto de la ventana (días 2 a 6).
#   Gráfico B.4: ventana de un año en bloques de cuatro semanas, con la entrada registrada o
#     adelantada uno y dos meses; cada panel se normaliza antes de su propia fecha.
#   Tabla C.12: traspaso frente a cambio de nivel, misma muestra del Gráfico 12. MCO de la
#     diferencia con los controles en cada actualización sobre Post (desde la entrada), Pre (las
#     ocho semanas previas), el cambio del costo en % del precio previo de la tratada y sus
#     interacciones, por separado en alzas y bajas del costo; errores agrupados por tratada.
#     b (Post) < 0 con d (Post x costo) ~ 0: cambio de nivel; d < 0 en las alzas: menor traspaso.
#   Gráfico B.10: brecha final de cada tratada (promedio de las semanas 9 a 15) y semana de su
#     mayor caída relativa a los controles.
#
# Entrada: data/processed/traspaso_actualizaciones.csv
# Salida:  outputs/grafico_12_traspaso_acumulado.pdf, outputs/grafico_B04_traspaso_adelantos.pdf,
#          outputs/tabla_C12_traspaso_nivel.tex, outputs/grafico_B10_brecha_estaciones.pdf

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

# etiquetas del eje con los extremos agrupados (plotmath, para que el pdf dibuje los signos)
etiquetas <- function(marcas) {
  e <- as.character(marcas)
  e[c(1, length(e))] <- c(sprintf("''<= %d", marcas[1]), sprintf("''>= %d", marcas[length(marcas)]))
  parse(text = e)
}

# ---- Gráfico 12 ----

ADELANTO <- 8L                                  # semanas: normalización dos meses antes de la entrada
LO <- -(9L + ADELANTO); HI <- 9L                # extremos agrupados en <= LO y >= HI
PARTES <- c(dif_log_reg = "Total", dif_01 = "Días 0 y 1", dif_26 = "Días 2 a 6")
res <- rbindlist(lapply(names(PARTES), function(col) {
  x <- brecha(td$k, -(16L + ADELANTO), 16L, -ADELANTO, col)
  x[, k := pmin(pmax(t, LO), HI)]
  x[, .(est = mean(C), se = ee(C), n = uniqueN(station_key)), by = k][, parte := factor(PARTES[[col]], levels = PARTES)]
}))
message(sprintf("Gráfico 12: %d tratadas", uniqueN(brecha(td$k, -(16L + ADELANTO), 16L, -ADELANTO)$station_key)))
print(dcast(res, k ~ parte, value.var = c("est", "se"))[, lapply(.SD, round, 3)], nrows = Inf)   # cifras que cita el texto

marcas <- c(LO, seq(LO + 2L + (-(LO + 2L)) %% 3L, HI - 2L, by = 3), HI)
fig <- ggplot(res, aes(k, est, colour = parte, shape = parte)) +
  geom_hline(yintercept = 0, colour = COLOR_REF) +
  geom_vline(xintercept = -0.5, linetype = "dashed", colour = COLOR_REF) +             # entrada registrada
  geom_vline(xintercept = -ADELANTO - 0.5, linetype = "dotted", colour = COLOR_REF) +  # normalización
  geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), size = 0.2,
                  position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = marcas, labels = etiquetas(marcas)) +
  labs(x = "Semanas desde la entrada registrada", y = "Brecha acumulada (%)", colour = NULL, shape = NULL)
guardar(fig, "grafico_12_traspaso_acumulado.pdf", alto = 3.4)

# ---- Tabla C.12: traspaso frente a cambio de nivel ----

x <- brecha(td$k, -(16L + ADELANTO), 16L, -ADELANTO)[td, on = .(station_key, u), nomatch = NULL]
x[, `:=`(post = as.integer(t >= 0L), pre = as.integer(t >= -ADELANTO & t < 0L), costo = 100 * dc / antes)]
SIGNOS <- c(alza = "Alzas del costo", baja = "Bajas del costo")
datos <- list(alza = x[dc > 0], baja = x[dc < 0])
modelos <- lapply(datos, function(z) feols(dif ~ post + pre + costo + post:costo + pre:costo, data = z, cluster = ~station_key))
etable_tex(modelos, headers = unname(SIGNOS), replace = TRUE, fitstat = ~n,
           dict = c(post = "Post", pre = "Pre (8 semanas)", costo = "Cambio del costo (\\% del precio)",
                    `post:costo` = "Post $\\times$ cambio del costo", `pre:costo` = "Pre $\\times$ cambio del costo",
                    `(Intercept)` = "Constante"),
           extralines = list(Tratadas = mapply(function(m, z) uniqueN(z[obs(m), station_key]), modelos, datos)),
           archivo = "tabla_C12_traspaso_nivel.tex")


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

# ---- Gráfico B.10: brecha por tratada ----

x <- brecha(td$k, -(16L + ADELANTO), 16L, -ADELANTO)
fin <- x[t >= HI, .(brecha = mean(C)), by = station_key]
caida <- x[t >= -ADELANTO, .SD[which.min(dif)], by = station_key][, .(station_key, semana = pmin(pmax(t, -ADELANTO), 15L))]
message(sprintf("Gráfico B.10: %d tratadas; brecha final media %.3f, mediana %.3f; %.1f%% con brecha negativa",
                nrow(fin), mean(fin$brecha), median(fin$brecha), 100 * mean(fin$brecha < 0)))
PANELES <- c("Brecha final (%)", "Semana de la mayor caída")
paneles <- rbind(fin[, .(valor = brecha, panel = PANELES[1])],
                 caida[, .(valor = semana, panel = PANELES[2])])
paneles[, panel := factor(panel, levels = PANELES)]
message(sprintf("Gráfico B.10: semana de la mayor caída, %% antes de la entrada %.1f, en las semanas 0 a 8 %.1f, desde la 9 %.1f",
                100 * mean(caida$semana < 0), 100 * mean(caida$semana %between% c(0L, 8L)), 100 * mean(caida$semana >= 9L)))
fig <- ggplot(paneles, aes(valor)) +
  geom_histogram(data = paneles[panel == PANELES[1]], binwidth = 0.5, boundary = 0, colour = "white") +
  geom_bar(data = paneles[panel == PANELES[2]], colour = "white", width = 0.9) +
  geom_vline(data = data.table(panel = factor(PANELES[2], levels = PANELES), x = -0.5), aes(xintercept = x),
             linetype = "dashed", colour = COLOR_REF) +
  facet_wrap(~panel, scales = "free") +
  labs(x = NULL, y = "Tratadas")
guardar(fig, "grafico_B10_brecha_estaciones.pdf", alto = 2.8)
