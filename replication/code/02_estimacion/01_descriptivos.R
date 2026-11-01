# 01_descriptivos.R
#
# Descripción del mercado: estructura y evolución del precio, margen, calendario de los
# cambios de precio y entradas por año.
#
# Entrada: data/processed/{estructura_precios,cambios_precio}.csv*, panel_2012_2026/{panel_semanal.csv.gz,entradas.csv}
# Salida:  outputs/grafico_01_estructura_precios.pdf, outputs/grafico_02_serie_precios.pdf,
#          outputs/tabla_01_cambios_por_dia.tex, outputs/grafico_B01_entradas_anio.pdf,
#          outputs/grafico_B02_serie_margen.pdf

source(here::here("code", "00_configuracion.R"))

# ---- Gráfico 1: estructura del precio a público, promedio del último año con ambos combustibles ----

COMPONENTES <- c(refineria = "Precio en refinería", especifico = "Impuesto específico", fepp = "FEPP",
                 iva = "IVA", margen = "Margen bruto de comercialización")   # orden de la barra, de abajo hacia arriba

est <- fread(ruta_proc("estructura_precios.csv"))
anio <- min(est[, max(year(fecha)), by = combustible]$V1)
prom <- est[year(fecha) == anio, lapply(.SD, function(v) 100 * mean(v)), by = combustible, .SDcols = names(COMPONENTES)]
message(sprintf("Gráfico 1: año %d", anio))
pl <- melt(prom, id.vars = "combustible", variable.name = "comp", value.name = "pesos")
pl <- pl[comp %in% pl[pesos != 0, comp]]   # fuera los componentes que no aplican
pl[, `:=`(comp = factor(COMPONENTES[as.character(comp)], levels = COMPONENTES),
          combustible = factor(combustible, levels = c("Gasolina 93", "Diésel")))]

fig <- ggplot(pl, aes(combustible, pesos, fill = comp)) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.6, colour = "grey20", linewidth = 0.2) +
  scale_fill_manual(values = c(COLORES, "#F2EBD9")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = NULL, y = "Porcentaje del precio a público", fill = NULL)
guardar(fig, "grafico_01_estructura_precios.pdf", alto = 3.3, ancho = 4)

# ---- Gráficos 2 y B.2: precio y margen semanales, promedio simple entre estaciones ----

semanal <- fread(file.path(dir_ventana("2012_2026"), "panel_semanal.csv.gz"),
                 select = c("fuel", "wk", "price", "margin"), colClasses = list(character = "fuel"))
serie <- semanal[, .(precio = mean(price), margen = mean(100 * margin / price, na.rm = TRUE)),
                 by = .(wk = as.IDate(wk), combustible = PRECIOS[paste0("p", fuel)])]
serie[, combustible := factor(combustible, levels = PRECIOS)]

grafico_serie <- function(d, y, etiqueta) {
  ggplot(d, aes(wk, .data[[y]], colour = combustible, linetype = combustible)) +
    geom_line() +
    # colores fijos por combustible: el margen no tiene 95 y no debe correr la paleta
    scale_colour_manual(values = setNames(COLORES, PRECIOS)) +
    scale_linetype_manual(values = setNames(LINEAS, PRECIOS)) +
    scale_x_date(expand = expansion(mult = c(0, .02)), date_breaks = "2 years", date_labels = "%Y") +
    labs(x = NULL, y = etiqueta, colour = NULL, linetype = NULL)
}
guardar(grafico_serie(serie, "precio", "Precio promedio ($/L)"), "grafico_02_serie_precios.pdf", alto = 2.6)
guardar(grafico_serie(serie[!is.na(margen)], "margen", "Margen de venta (%)"), "grafico_B02_serie_margen.pdf", alto = 2.6)

# ---- Tabla 1: % de los cambios de precio por tipo de día del calendario MEPCO ----

cambios <- fread(ruta_proc("cambios_precio.csv.gz"), colClasses = list(character = "fuel"))
t1 <- cambios[, .N, by = .(fuel, regimen, tipo)][, pct := 100 * N / sum(N), by = .(fuel, regimen)]
pct <- dcast(t1, tipo ~ fuel + regimen, value.var = "pct", fill = 0)
COLS <- c("93_Semanal", "93_Trisemanal", "di_Semanal", "di_Trisemanal")
setcolorder(pct, c("tipo", COLS))
n <- t1[, .(N = sum(N)), by = .(col = paste(fuel, regimen, sep = "_"))][match(COLS, col), N]

escribir_tabla("lcccc", c(
  " & \\multicolumn{2}{c}{Gasolina 93} & \\multicolumn{2}{c}{Diésel} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  fila("Tipo de día", "Semanal", "Trisemanal", "Semanal", "Trisemanal"), "\\midrule",
  vapply(seq_len(nrow(pct)), function(i) fila(pct$tipo[i], sprintf("%.1f", unlist(pct[i, ..COLS]))), ""),
  "\\midrule",
  fila("Cambios", formatC(n, format = "d", big.mark = ","))),
  "tabla_01_cambios_por_dia.tex")

# ---- Gráfico B.1: entradas por año (2013 en adelante; en 2012 se pobló la plataforma) ----

entradas <- fread(file.path(dir_ventana("2012_2026"), "entradas.csv"), select = "g")[, .N, by = .(anio = year(as.IDate(g)))]
fig <- ggplot(entradas, aes(anio, N)) +
  geom_col() +
  scale_x_continuous(breaks = 2013:2026) +
  labs(x = NULL, y = "Entradas")
guardar(fig, "grafico_B01_entradas_anio.pdf", alto = 2.4)
