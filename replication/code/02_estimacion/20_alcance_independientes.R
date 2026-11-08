# 20_alcance_independientes.R
#
# Alcance espacial del efecto de las entrantes independientes (Gráficos B.21 y B.22). Estudio de eventos de Sun y Abraham
# (cohortes anuales) por anillo de distancia, solo con entradas independientes (ni Copec, ni Shell/Enex,
# ni Aramco/Petrobras). Cada incumbente se ubica en el anillo de su primera entrada competidora a
# <= R km (R: radio exterior del diseño; la más cercana del mes) y entra solo si esa entrante es
# independiente; las de primera entrada de marca quedan fuera. Una sola regresión con un grupo por
# anillo (cohortes_sunab(d, grupo)). Mismas reglas que 03_paneles.R: no cuentan las entradas de su
# propia marca ni las de fuera de su vida en el panel; la tratada se corta en la segunda entrada
# competidora a <= R km. Controles: sin entradas de ninguna marca, ni de la propia, a <= R km.
#   Gráfico B.21: anillos de 1 km hasta 5 km (R = 5, controles estrictos).
#   Gráfico B.22: tramos 0-2, 2-5 y 5-10 km (R = 10).
#
# Entrada: data/processed/panel_2012_2026/{panel_mensual,entradas}.csv
# Salida:  outputs/grafico_B21_es_independientes_anillos.pdf, outputs/grafico_B22_es_independientes_tramos.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)

V <- VENTANAS[["2012_2026"]]
DISENOS <- list(anillos = list(cortes = 0:5, archivo = "grafico_B21_es_independientes_anillos.pdf"),
                tramos = list(cortes = c(0, 2, 5, 10), archivo = "grafico_B22_es_independientes_tramos.pdf"))

panel <- leer_panel("2012_2026")
entradas <- fread(file.path(dir_ventana("2012_2026"), "entradas.csv"))[, g := as.IDate(g)]

for (dz in names(DISENOS)) {
  cortes <- DISENOS[[dz]]$cortes
  R <- max(cortes)
  ANILLOS <- sprintf("%g a %g km", head(cortes, -1), cortes[-1])
  d <- muestra_anillos(panel, entradas, cortes, V$focal)[treated == 0L | indep == 1L]   # solo entradas independientes
  a <- unique(d[treated == 1L, .(station_key, anillo)])
  controles <- d[treated == 0L, unique(station_key)]
  d <- cohortes_sunab(d, "anillo")
  message(sprintf("Diseño %s: tratadas por anillo %s; %d controles (sin entradas a <= %g km)", dz,
                  paste(sprintf("%s: %d", ANILLOS, a[, .N, keyby = anillo]$N), collapse = ", "), length(controles), R))
  res <- rbindlist(lapply(names(PRECIOS), function(fv) {
    z <- d[!is.na(get(fv))]
    m <- estimar_sa(z, fv)
    at <- att_sa(m, z)[startsWith(efecto, "grupo")]
    message(sprintf("  %s: ATT %s; valor p bins previos %s", PRECIOS[[fv]],
                    paste(sprintf("%s %.3f (%.3f)", ANILLOS, at$est, at$se), collapse = "; "),
                    paste(sapply(seq_along(ANILLOS) - 1L, function(g) p_previos(m, "per", z, g)), collapse = "/")))
    agregar_sa(m, z)[, `:=`(anillo = factor(ANILLOS[grupo + 1L], levels = ANILLOS), combustible = factor(PRECIOS[[fv]], levels = PRECIOS))]
  }))
  print(dcast(res, combustible + event_time ~ anillo, value.var = "estimate", fun.aggregate = function(v) round(v, 3)), nrows = Inf)
  fig <- grafico_es(res, "anillo", dodge = 0.6) +
    scale_colour_manual(values = c(COLORES, "grey45")) + scale_shape_manual(values = c(FORMAS, 18)) +
    labs(colour = "Entrada independiente a", shape = "Entrada independiente a")
  guardar(fig, DISENOS[[dz]]$archivo, alto = 3.8, ancho = 6)
}
