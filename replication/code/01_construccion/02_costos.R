# 02_costos.R
#
# Dos fuentes de costos:
#   1. Costo mayorista semanal con y sin MEPCO (Ministerio de Hacienda) para 93, 97 y diésel.
#      La 95 no tiene referencia, por eso no tiene margen. La serie es de jueves
#      consecutivos y cae sobre la grilla semanal del panel (SEMANA_0).
#   2. Estructura del precio a público de la 93 y el diésel (CNE), como fracción del precio por componente.
#
# Entrada: data/raw/mepco.xlsx, data/raw/estructura_precios_combustibles.xlsx
# Salida:  data/processed/costo_mepco.csv, data/processed/estructura_precios.csv

source(here::here("code", "00_configuracion.R"))
library(readxl)

# ---- 1. MEPCO ----

# columna 8 del excel: separador vacío
mepco <- read_excel(
  ruta_raw("mepco.xlsx"), skip = 2,
  col_types = c("date", rep("text", 6), "skip", rep("text", 3)),
  col_names = c("date", "93_w/o", "93_w/", "97_w/o", "97_w/", "di_w/o", "di_w/",
                "93_variable_specific_tax_utm_m3", "97_variable_specific_tax_utm_m3",
                "di_variable_specific_tax_utm_m3")) |>
  mutate(date = as.Date(date), across(-date, as.numeric)) |>
  arrange(date)

stopifnot("la serie MEPCO no es de jueves consecutivos" =
            all(diff(mepco$date) == 7) && all(wday(mepco$date) == 5L),
          "la serie MEPCO no cae sobre la grilla semanal" = as.integer(mepco$date[1] - SEMANA_0) %% 7L == 0L)

escribir_csv(mepco, ruta_proc("costo_mepco.csv"))
message(sprintf("02_costos.R: MEPCO, %d semanas, %s a %s", nrow(mepco), min(mepco$date), max(mepco$date)))

# ---- 2. Estructura del precio (CNE) ----

HOJAS <- c(GASOLINA_93 = "Gasolina 93", DIESEL = "Diésel")
COMPONENTES <- c("refineria", "margen", "iva", "especifico", "fepp")   # orden de las columnas del excel

# fecha como serial de excel; "NO APLICA" y vacío = 0
estructura <- imap(HOJAS, function(hoja, h) {
  suppressMessages(read_excel(ruta_raw("estructura_precios_combustibles.xlsx"),
                              sheet = h, skip = 5, col_names = FALSE)) |>
    rename_with(\(n) c("serial", COMPONENTES), 1:6) |>
    mutate(fecha = as.Date(suppressWarnings(as.numeric(serial)), origin = "1899-12-30")) |>
    filter(!is.na(fecha)) |>
    mutate(across(all_of(COMPONENTES), \(v) replace_na(suppressWarnings(as.numeric(v)), 0))) |>
    select(fecha, all_of(COMPONENTES)) |>
    mutate(combustible = hoja)
}) |>
  list_rbind()

escribir_csv(estructura, ruta_proc("estructura_precios.csv"))
message(sprintf("02_costos.R: estructura del precio, %d meses-combustible", nrow(estructura)))
