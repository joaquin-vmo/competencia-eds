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

est <- leer_csv(ruta_proc("estructura_precios.csv"))
anio <- min(summarise(est, m = max(year(fecha)), .by = combustible)$m)
message(sprintf("Gráfico 1: año %d", anio))
pl <- est |>
  filter(year(fecha) == anio) |>
  summarise(across(all_of(names(COMPONENTES)), \(v) 100 * mean(v)), .by = combustible) |>
  pivot_longer(-combustible, names_to = "comp", values_to = "pesos")
pl <- pl |>
  filter(comp %in% comp[pesos != 0]) |>   # fuera los componentes que no aplican
  mutate(comp = factor(unname(COMPONENTES[comp]), levels = COMPONENTES),
         combustible = factor(combustible, levels = c("Gasolina 93", "Diésel")))

fig <- ggplot(pl, aes(combustible, pesos, fill = comp)) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.6, colour = "grey20", linewidth = 0.2) +
  scale_fill_manual(values = c(COLORES, "#F2EBD9")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = NULL, y = "Porcentaje del precio a público", fill = NULL)
guardar(fig, "grafico_01_estructura_precios.pdf", alto = 3.3, ancho = 4)

# ---- Gráficos 2 y B.2: precio y margen semanales, promedio simple entre estaciones ----

serie <- leer_csv(file.path(dir_ventana("2012_2026"), "panel_semanal.csv.gz"),
                 col_select = c(fuel, wk, price, margin), col_types = cols(fuel = "c")) |>
  mutate(combustible = unname(PRECIOS[paste0("p", fuel)])) |>
  summarise(precio = mean(price), margen = mean(100 * margin / price, na.rm = TRUE), .by = c(wk, combustible)) |>
  mutate(combustible = factor(combustible, levels = PRECIOS))

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
guardar(grafico_serie(filter(serie, !is.na(margen)), "margen", "Margen de venta (%)"), "grafico_B02_serie_margen.pdf", alto = 2.6)

# ---- Tabla 1: % de los cambios de precio por tipo de día del calendario MEPCO ----

cambios <- leer_csv(ruta_proc("cambios_precio.csv.gz"), col_types = cols(fuel = "c"))
t1 <- cambios |>
  count(fuel, regimen, tipo, name = "N") |>
  mutate(pct = 100 * N / sum(N), .by = c(fuel, regimen))
COLS <- c("93_Semanal", "93_Trisemanal", "di_Semanal", "di_Trisemanal")
pct <- t1 |>
  pivot_wider(id_cols = tipo, names_from = c(fuel, regimen), values_from = pct, values_fill = 0) |>
  arrange(tipo) |>
  select(tipo, all_of(COLS))
n <- t1 |>
  mutate(col = paste(fuel, regimen, sep = "_")) |>
  summarise(N = sum(N), .by = col) |>
  slice(match(COLS, col)) |>
  pull(N)

escribir_tabla("lcccc", c(
  " & \\multicolumn{2}{c}{Gasolina 93} & \\multicolumn{2}{c}{Diésel} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  fila("Tipo de día", "Semanal", "Trisemanal", "Semanal", "Trisemanal"), "\\midrule",
  vapply(seq_len(nrow(pct)), function(i) fila(pct$tipo[i], sprintf("%.1f", unlist(pct[i, COLS]))), ""),
  "\\midrule",
  fila("Cambios", formatC(n, format = "d", big.mark = ","))),
  "tabla_01_cambios_por_dia.tex")

# ---- Gráfico B.1: entradas por año (2013 en adelante; en 2012 se pobló la plataforma) ----

entradas <- leer_csv(file.path(dir_ventana("2012_2026"), "entradas.csv"), col_select = g) |>
  count(anio = year(g), name = "N")
fig <- ggplot(entradas, aes(anio, N)) +
  geom_col() +
  scale_x_continuous(breaks = 2013:2026) +
  labs(x = NULL, y = "Entradas")
guardar(fig, "grafico_B01_entradas_anio.pdf", alto = 2.4)
