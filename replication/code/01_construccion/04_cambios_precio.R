# 04_cambios_precio.R
#
# Cambios de precio diarios de la 93 y el diésel, clasificados según el calendario del MEPCO.
# La ley obliga a informar cada cambio: un cambio es un día cuyo último precio difiere del
# último precio reportado antes. Tipo de día:
#   - día de actualización (el jueves en que cambia el costo MEPCO) o el siguiente,
#   - jueves sin cambio de costo (solo existe en el régimen trisemanal) o el siguiente,
#   - otro día.
#
# Entrada: data/processed/{precios,costo_mepco}.csv
# Salida:  data/processed/cambios_precio.csv.gz (un cambio por fila, 2015 a 2025)

source(here::here("code", "00_configuracion.R"))

DESDE <- as.IDate("2015-01-01")
HASTA <- as.IDate("2025-12-31")

p <- fread(ruta_proc("precios.csv"), select = c("id", "price", "date", "time", "fuel"),
           colClasses = list(character = c("id", "fuel"), IDate = "date"))[fuel %in% c("93", "di")]
p[, time := as.ITime(time)]
setorder(p, id, fuel, date, time, na.last = TRUE)
p <- unique(p, by = c("id", "fuel", "date"), fromLast = TRUE)   # último precio del día
p[, dp := price - shift(price), by = .(id, fuel)]
cambios <- p[!is.na(dp) & dp != 0 & date >= DESDE & date <= HASTA, .(id, fuel, date, dp)]

costo <- fread(ruta_proc("costo_mepco.csv"))[, .(date = as.IDate(date), c93 = `93_w/`, cdi = `di_w/`)]
setorder(costo, date)
publicacion  <- costo$date                                                   # todos los jueves
actualizacion <- costo[c93 != shift(c93) | cdi != shift(cdi), date]          # jueves con cambio de costo

cambios[, regimen := fifelse(date < REFORMA_MEPCO, "Semanal", "Trisemanal")]
cambios[, tipo := fcase(date %in% actualizacion | (date - 1L) %in% actualizacion, "Día de actualización o siguiente",
                        date %in% publicacion | (date - 1L) %in% publicacion, "Jueves sin cambio de costo",
                        default = "Otro día")]

fwrite(cambios, ruta_proc("cambios_precio.csv.gz"))
message(sprintf("04_cambios_precio.R: %s cambios de precio", format(nrow(cambios), big.mark = ",")))
