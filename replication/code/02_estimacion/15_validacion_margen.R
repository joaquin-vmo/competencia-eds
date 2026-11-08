# 15_validacion_margen.R
#
# Tabla C.2: margen de Copec, Petrobras y Shell por anillos de 1 km en torno a la ex estación
# Sesa de Av. Tobalaba 1895 (agosto 2018 a julio 2019), junto a la Tabla N.º 4 del informe de
# archivo Rol 2615-20 de la FNE. Margen ponderado por volumen sobre precio ponderado,
# renormalizado a los combustibles que vende cada estación-mes.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/tabla_C02_validacion_margen.tex

source(here::here("code", "00_configuracion.R"))

REF_LAT <- -33.43005126196647   # coordenadas CNE de pb1312301, Av. Tobalaba 1895
REF_LON <- -70.58694630861282
DESDE   <- as.IDate("2018-08-01")
HASTA   <- as.IDate("2019-07-01")
MARCAS  <- c(copec = "Copec", aramco_petrobras = "Petrobras", shell = "Shell")
# participaciones nacionales en volumen (FNE 2025) de los combustibles con costo MEPCO
W <- c(p93 = 16.0, p97 = 3.3, pdi = 52.5)
W <- W / sum(W)
MARGEN <- c(p93 = "m93", p97 = "m97", pdi = "mdi")

# informe Rol 2615-20, Tabla N.º 4: rangos de margen porcentual y en $/L por anillo
FNE <- list(Copec     = c("5--10\\% (40--50)", "5--10\\% (50--60)", "5--10\\% (40--50)", "5--10\\% (40--50)", "5--10\\% (50--60)"),
            Petrobras = c("--", "--", "0--5\\% (20--30)", "0--5\\% (20--30)", "5--10\\% (30--40)"),
            Shell     = rep("0--5\\% (20--30)", 5))

# ---- Estaciones a menos de 5 km de la ex Sesa ----

d <- leer_panel("2012_2026")[ym >= DESDE & ym <= HASTA & distribuidor %in% names(MARCAS)]
d[, km := as.vector(dist_km(REF_LAT, REF_LON, lat, lon))]
d <- d[km > 0.01 & km < 5]   # la estación no es rival de sí misma
d[, `:=`(anillo = floor(km), marca = factor(MARCAS[distribuidor], levels = MARCAS))]

# ---- Margen ponderado por volumen ----

num <- Reduce(`+`, lapply(names(W), function(f) W[[f]] * fcoalesce(d[[MARGEN[[f]]]], 0)))
den <- Reduce(`+`, lapply(names(W), function(f) W[[f]] * fifelse(is.na(d[[MARGEN[[f]]]]), 0, d[[f]])))
wd  <- Reduce(`+`, lapply(names(W), function(f) W[[f]] * !is.na(d[[MARGEN[[f]]]])))
d[, `:=`(pct = 100 * num / den, pesos = num / wd)]

# ---- Tabla C.2: margen por anillo frente al informe de la FNE ----

agr <- d[is.finite(pct), .(pct = mean(pct), pesos = mean(pesos)), keyby = .(anillo, marca)]
celda <- function(m, a) {
  r <- agr[marca == m & anillo == a]
  if (nrow(r)) sprintf("%.1f\\%% (%.0f)", r$pct, r$pesos) else "--"
}
escribir_tabla("lcccccc",
               c(fila("", sprintf("\\multicolumn{2}{c}{%s}", MARCAS)),
                 "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
                 fila("Anillo", rep(c("Este trabajo", "FNE"), 3)), "\\midrule",
                 vapply(0:4, function(a) fila(sprintf("%d--%d km", a, a + 1),
                                              c(rbind(vapply(MARCAS, celda, "", a = a),
                                                      vapply(MARCAS, function(m) FNE[[m]][a + 1], "")))), "")),
               "tabla_C02_validacion_margen.tex")
