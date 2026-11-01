# 02_muestra.R
#
# Asignación de tratamiento y muestra de estimación de la ventana 2012_2026.
#   Gráfico 4: una entrada del Gran Santiago con círculos de 1, 2, 3 y 5 km y las estaciones
#     presentes ese mes según su distancia a la entrante. Se elige, entre las cohortes focales
#     del Gran Santiago con al menos una estación en cada anillo, la de más estaciones a <= 2 km.
#   Tabla 3: medias de tratadas y controles estrictos en 2013 (el año previo a la primera
#     cohorte), una observación por estación; la diferencia con errores agrupados por comuna.
#   Tabla C.1: estaciones por rol y muestra de estimación de la 93.
#
# Entrada: data/processed/panel_2012_2026/{panel_mensual,entradas}.csv, data/raw/mapas/comunas
# Salida:  outputs/grafico_04_caso_entrada.pdf, outputs/tabla_03_descriptiva.tex,
#          outputs/tabla_C01_composicion_muestra.tex

source(here::here("code", "00_configuracion.R"))
library(fixest)
library(sf)

V <- VENTANAS[["2012_2026"]]
panel <- leer_panel("2012_2026")
entradas <- fread(file.path(dir_ventana("2012_2026"), "entradas.csv"))[, g := as.IDate(g)]

# ==============================================================================
# Gráfico 4: caso de entrada
# ==============================================================================

RADIOS  <- c(1, R_TRATADA, R_CTRL_AMP, R_CTRL_EST)   # km
VENTANA_MAPA <- 6.5                                  # km alrededor de la entrante
GRUPOS <- c("Tratada (0 a 1 km)", "Tratada (1 a 2 km)", "Excluida (2 a 3 km)",
            "Control amplio (3 a 5 km)", "Control amplio y estricto (5 km o más)")
# tratadas en colores oscuros y triángulos, excluidas en gris con cruz, controles en círculos
COLOR_GRUPO <- c(COLORES[2], COLORES[4], "grey50", COLORES[3], COLORES[1])
FORMA_GRUPO <- c(17, 17, 4, 1, 16)

# las 32 comunas de la provincia de Santiago más Puente Alto y San Bernardo
comunas <- st_transform(st_read(ruta_raw("mapas", "comunas", "comunas.shp"), quiet = TRUE), CRS_M)
gs <- st_make_valid(comunas[comunas$codregion == 13 &
                              (comunas$Provincia == "Santiago" | comunas$Comuna %in% c("Puente Alto", "San Bernardo")), ])
stopifnot(nrow(gs) == 34)

e <- entradas[eregion == "metropolitana" & year(g) >= V$focal]
e <- e[as.vector(st_within(st_transform(st_as_sf(e, coords = c("elon", "elat"), crs = 4326), CRS_M),
                           st_union(gs), sparse = FALSE))]

anillos <- function(ev) {   # estaciones con registro en el mes de la entrada, por anillo
  s <- panel[ym == ev$g & station_key != ev$station_key & !is.na(lat)]
  s[, km := as.vector(dist_km(ev$elat, ev$elon, lat, lon))]
  s[, grupo := factor(GRUPOS[findInterval(km, c(0, RADIOS), left.open = TRUE)], levels = GRUPOS)]
  s[km <= VENTANA_MAPA]
}
cuenta <- rbindlist(lapply(seq_len(nrow(e)), function(i) {
  n <- table(anillos(e[i])$grupo)
  data.table(i = i, minimo = min(n[1:4]), tratadas = sum(n[1:2]), total = sum(n))
}))
ev <- e[cuenta[minimo >= 1][order(-tratadas, -total)]$i[1]]
s <- anillos(ev)
message(sprintf("Gráfico 4: entrada %s (%s), %s", ev$station_key, ev$edist, ev$g))

centro <- st_transform(st_as_sf(ev, coords = c("elon", "elat"), crs = 4326), CRS_M)
caja <- st_bbox(st_buffer(centro, VENTANA_MAPA * 1000))
circulos <- st_sf(radio = RADIOS, geometry = st_buffer(st_geometry(centro)[rep(1, length(RADIOS))], RADIOS * 1000))
puntos <- st_transform(st_as_sf(s, coords = c("lon", "lat"), crs = 4326), CRS_M)
nombres <- suppressWarnings(st_point_on_surface(st_crop(gs, caja)))

fig <- ggplot() +
  geom_sf(data = gs, fill = "grey97", colour = "grey70", linewidth = 0.3) +
  geom_sf_text(data = nombres, aes(label = Comuna), size = 1.9, colour = "grey55") +
  geom_sf(data = circulos, fill = NA, colour = "grey25", linetype = "dashed") +
  geom_sf(data = puntos, aes(colour = grupo, shape = grupo), size = 1.2) +
  geom_sf(data = centro, shape = 8, size = 3, stroke = 1, colour = "black") +
  scale_colour_manual(values = COLOR_GRUPO, drop = FALSE) +
  scale_shape_manual(values = FORMA_GRUPO, drop = FALSE) +
  coord_sf(xlim = caja[c("xmin", "xmax")], ylim = caja[c("ymin", "ymax")], datum = NA) +
  labs(x = NULL, y = NULL, colour = NULL, shape = NULL) +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(ncol = 2), shape = guide_legend(ncol = 2))
guardar(fig, "grafico_04_caso_entrada.pdf", alto = 4.6, ancho = 4.2)

# ==============================================================================
# Tabla 3: características de tratadas y controles estrictos en 2013
# ==============================================================================

ANIO_BASE <- 2013L
d <- muestra_estacion(panel, CTRL_ESTRICTO, V$focal)[!is.na(p93)]

est <- d[year == ANIO_BASE, .(p93 = mean(p93, na.rm = TRUE), p97 = mean(p97, na.rm = TRUE), pdi = mean(pdi, na.rm = TRUE),
                              n = mean(n_vecinas), treated = treated[1], comuna = comuna[1],
                              distribuidor = distribuidor[1], region = region[1], d_autopista = d_autopista[1]),
         by = station_key]
est[, `:=`(copec = 100 * (distribuidor == "copec"), shell = 100 * (distribuidor == "shell"),
           aramco = 100 * (distribuidor == "aramco_petrobras"),
           otra = 100 * !(distribuidor %in% c("copec", "shell", "aramco_petrobras")),
           rm = 100 * (region == "metropolitana"), autopista = 100 * (d_autopista <= AUTOPISTA_M))]
est <- est[is.finite(p93)]   # las estaciones sin precio en 2013 quedan fuera de la tabla, no del modelo

VARS <- c(p93 = "Precio gasolina 93 (\\$/litro)", p97 = "Precio gasolina 97 (\\$/litro)",
          pdi = "Precio diésel (\\$/litro)", n = "Estaciones a 2 km o menos",
          copec = "Copec (\\%)", shell = "Shell (\\%)", aramco = "Petrobras/Aramco (\\%)",
          otra = "Otra marca o sin bandera (\\%)", rm = "Región Metropolitana (\\%)",
          autopista = "A 100 m o menos de una autopista (\\%)")

cuerpo <- unlist(lapply(names(VARS), function(v) {
  x <- est[is.finite(get(v))]
  ct <- coeftable(feols(as.formula(sprintf("%s ~ treated", v)), data = x, cluster = ~comuna))["treated", ]
  g <- x[, .(media = mean(get(v)), de = sd(get(v))), keyby = treated]
  c(fila(VARS[[v]], sprintf("%.1f", g[treated == 1L, media]), sprintf("%.1f", g[treated == 0L, media]),
         sprintf("%.1f%s", ct[1], estrellas(ct[4])), sprintf("%.2f", ct[4])),
    fila("", sprintf("(%.1f)", g[treated == 1L, de]), sprintf("(%.1f)", g[treated == 0L, de]),
         sprintf("(%.1f)", ct[2]), ""))
}))
escribir_tabla("lcccc", c(fila("", "Tratadas", "Controles", "Diferencia", "Valor $p$"), "\\midrule",
                          cuerpo, "\\midrule",
                          fila("Estaciones", est[treated == 1L, .N], est[treated == 0L, .N], "", "")),
               "tabla_03_descriptiva.tex")

# ==============================================================================
# Tabla C.1: composición de la muestra
# ==============================================================================

st <- unique(panel[, .(station_key, base, role_entry)])

# mismos n que las tablas de resultados: estaciones que usa el modelo, sin singletons
n_muestra <- function(roles) {
  x <- muestra_estacion(panel, roles, V$focal)[!is.na(p93)]
  m <- estimar_es(x, "p93")
  message(sprintf("Tabla C.1: %s observaciones en la 93 con controles %s", format(nobs(m), big.mark = ","),
                  paste(roles, collapse = "+")))
  n_estaciones(m, x)
}
n_amplio <- n_muestra(CTRL_AMPLIO)
n_estricto <- n_muestra(CTRL_ESTRICTO)
stopifnot(n_amplio[1] == n_estricto[1])

filas <- list(
  c("Estaciones observadas (2012--2026)", nrow(st)),
  c("\\quad presentes en 2012 (incumbentes)", st[base == TRUE, .N]),
  c("\\quad entrantes posteriores a 2012", st[base == FALSE, .N]),
  c("Entradas registradas", nrow(entradas)),
  c(sprintf("\\quad cohortes focales (%d en adelante)", V$focal), entradas[year(g) >= V$focal, .N]),
  c(sprintf("Tratadas (entrada competidora a %d km o menos)", R_TRATADA), st[role_entry == "treated", .N]),
  c(sprintf("Excluidas (entrada entre %d y %d km)", R_TRATADA, R_CTRL_AMP), st[role_entry == "buffer", .N]),
  c(sprintf("Control amplio (sin entradas a %d km o menos)", R_CTRL_AMP), st[role_entry %in% CTRL_AMPLIO, .N]),
  c(sprintf("\\quad control estricto (sin entradas a %d km o menos)", R_CTRL_EST), st[role_entry %in% CTRL_ESTRICTO, .N]),
  c("Muestra de estimación, gasolina 93", ""),
  c("\\quad tratadas", n_amplio[1]),
  c("\\quad control amplio", n_amplio[2]),
  c("\\quad control estricto", n_estricto[2])
)
escribir_tabla("lr", c(fila("", "N"), "\\midrule", vapply(filas, function(f) fila(f[1], f[2]), "")),
               "tabla_C01_composicion_muestra.tex")
