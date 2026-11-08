# 04_cambios_precio.R
#
# Cambios de precio diarios de la 93 y el diésel, clasificados según el calendario del MEPCO.
# La ley obliga a informar cada cambio: un cambio es un día cuyo último precio difiere del
# último precio reportado antes. Tipo de día:
#   - día de actualización (el jueves en que cambia el costo MEPCO) o el siguiente,
#   - jueves sin cambio de costo o el siguiente (casi solo en el régimen trisemanal; en el semanal, las
#     pocas semanas sin cambio de costo: 88 cambios),
#   - otro día.
#
# Entrada: data/processed/{precios,costo_mepco}.csv
# Salida:  data/processed/cambios_precio.csv.gz (un cambio por fila, 2015 a 2025)

source(here::here("code", "00_configuracion.R"))

DESDE <- as.Date("2015-01-01")
HASTA <- as.Date("2025-12-31")

# ---- Cambios de precio diarios ----

cambios <- leer_csv(ruta_proc("precios.csv"), col_select = c(id, price, date, time, fuel),
                    col_types = cols(id = "c", fuel = "c", date = "D")) |>
  filter(fuel %in% c("93", "di")) |>
  arrange(id, fuel, date, time) |>
  filter(!duplicated(consecutive_id(id, fuel, date), fromLast = TRUE)) |>   # último precio del día
  mutate(dp = price - lag(price), .by = c(id, fuel)) |>
  filter(!is.na(dp), dp != 0, date >= DESDE, date <= HASTA) |>
  select(id, fuel, date, dp)

# ---- Clasificación según el calendario del MEPCO ----

costo <- leer_csv(ruta_proc("costo_mepco.csv")) |>
  transmute(date, c93 = `93_w/`, cdi = `di_w/`) |>
  arrange(date)
publicacion  <- costo$date                                                                   # todos los jueves
actualizacion <- filter(costo, c93 != lag(c93) | cdi != lag(cdi))$date                       # jueves con cambio de costo

cambios <- cambios |>
  mutate(regimen = if_else(date < REFORMA_MEPCO, "Semanal", "Trisemanal"),
         tipo = case_when(date %in% actualizacion | (date - 1L) %in% actualizacion ~ "Día de actualización o siguiente",
                          date %in% publicacion | (date - 1L) %in% publicacion ~ "Jueves sin cambio de costo",
                          .default = "Otro día"))

escribir_csv(cambios, ruta_proc("cambios_precio.csv.gz"))
message(sprintf("04_cambios_precio.R: %s cambios de precio", format(nrow(cambios), big.mark = ",")))
