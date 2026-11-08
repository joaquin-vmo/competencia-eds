# 01_precios.R
#
# Une y limpia los precios de Bencina en Línea de los dos regímenes de reporte de la CNE.
#   Antiguo (2012-2022): separador ";", coma decimal en las coordenadas, sin hora.
#   Actual (2023-2026): separador ",", precio asistido y autoservicio, con hora.
# Se usa el antiguo antes del 2023-01-01 y el actual desde esa fecha. El actual no trae la
# historia de las estaciones que dejaron de reportar antes, por eso se empalman ambos.
#
# Entrada: data/raw/bencina_en_linea/{2012..2026}.csv
# Salida:  data/processed/precios.csv (una fila por reporte: estación, fecha, hora, combustible)

source(here::here("code", "00_configuracion.R"))

ANIOS <- 2012:2026
CORTE_REGIMEN <- as.IDate("2023-01-01")   # inicio del régimen actual

SENTINELAS  <- c(0, 99999999)
PRECIO_MIN  <- 200    # $/L
PRECIO_MAX  <- 5000   # $/L
TOL_MEDIANA <- 0.35   # tolerancia al corregir precios con decimales corridos

# Solo precio asistido: "A93" etc. (autoservicio, desde 2023) no es comparable con el antiguo
COMBUSTIBLE <- c("Gasolina 93" = "93", "Gasolina 95" = "95", "Gasolina 97" = "97", "Petroleo Diesel" = "di",
                 "93" = "93", "95" = "95", "97" = "97", "DI" = "di")

# vocabulario único entre regímenes
DISTRIBUIDOR <- c("vigu ltda." = "vigu", "ruta 45" = "ruta v45", "go abastible" = "abastible",
                  "gasco autogas" = "autogasco", "hola" = "hola!", "esa" = "sesa",
                  "petrobras" = "aramco_petrobras", "aramco" = "aramco_petrobras")
COMUNA <- c("aisén" = "aysen", "santiago centro" = "santiago")
REGION <- c(
  # antiguo
  "bío bío" = "biobio", "biobío" = "biobio", "gral. bernardo o'higgins" = "ohiggins",
  "los ríos" = "los_rios", "los lagos" = "los_lagos", "araucanía" = "araucania",
  "magallanes y la antártida chilena" = "magallanes_antartida",
  "aysén gral. c. ibáñez del campo" = "aysen",
  # actual
  "metropolitana de santiago" = "metropolitana", "del biobío" = "biobio", "del maule" = "maule",
  "de la araucanía" = "araucania", "de los lagos" = "los_lagos", "de los ríos" = "los_rios",
  "magallanes y de la antártica chilena" = "magallanes_antartida",
  "aysén del gral. carlos ibáñez del campo" = "aysen",
  # ambos
  "valparaíso" = "valparaiso", "ñuble" = "nuble", "tarapacá" = "tarapaca", "arica y parinacota" = "arica")

# ---- Funciones de lectura y limpieza ----

recodificar <- function(x, dic) {
  i <- match(x, names(dic))
  fifelse(is.na(i), x, dic[i])
}

informar <- function(d, paso) {
  message(sprintf("  %-50s %s filas", paso, format(nrow(d), big.mark = ",")))
  d
}

# todas las columnas como texto; `columnas`: nombres en orden del archivo, NA = no se lee
leer_regimen <- function(anios, sep, columnas) {
  rbindlist(lapply(anios, function(a) {
    f <- ruta_raw("bencina_en_linea", paste0(a, ".csv"))
    if (ncol(fread(f, sep = sep, nrows = 0L)) != length(columnas))
      stop(basename(f), ": se esperaban ", length(columnas), " columnas")
    fread(f, sep = sep, header = TRUE, colClasses = "character", encoding = "UTF-8",
          na.strings = c("", "NA"), select = which(!is.na(columnas)),
          col.names = columnas[!is.na(columnas)], showProgress = FALSE)
  }))
}

# aplica f una vez por valor único (mucho más rápido que por fila)
por_unico <- function(x, f) {
  u <- unique(x)
  f(u)[match(x, u)]
}

a_numero <- function(x) suppressWarnings(as.numeric(sub(",", ".", gsub('"', "", x, fixed = TRUE), fixed = TRUE)))

normalizar_lugar <- function(x) {
  x <- gsub("['’]", "", chartr("áéíóúüñ", "aeiouun", x), perl = TRUE)
  gsub("(*UCP)\\s+", "_", x, perl = TRUE)
}

limpiar_campos <- function(d) {
  d <- d[!id %in% c("prueba", "CNE 01")]   # registros de prueba
  d[, `:=`(
    id = por_unico(id, function(x) tolower(trimws(fifelse(x == "co730401 co730401", "co730401", x, na = NA_character_)))),
    distributor = por_unico(distributor, function(x) recodificar(tolower(trimws(x)), DISTRIBUIDOR)),
    municipality = por_unico(municipality, function(x) normalizar_lugar(recodificar(tolower(trimws(x)), COMUNA))),
    region = por_unico(region, function(x) {
      x <- tolower(trimws(gsub("\\p{Cf}", "", x, perl = TRUE)))   # carácter invisible
      recodificar(fifelse(grepl("libertador", x, fixed = TRUE), "ohiggins", x), REGION)
    }),
    date = por_unico(date, as.IDate),
    latitud  = por_unico(latitud, a_numero),
    longitud = por_unico(longitud, a_numero)
  )]
  d[latitud %between% c(-90, 0) & longitud %between% c(-180, 0)]   # en Chile ambas son negativas
}

# precio sobre el doble de la mediana del combustible-año: se divide por 10 o 100 si así
# queda a menos de TOL_MEDIANA de la mediana (decimales corridos); lo demás fuera de rango sale
limpiar_precios <- function(d) {
  d <- d[fuel %in% names(COMBUSTIBLE)]
  d[, `:=`(fuel = unname(COMBUSTIBLE[fuel]), price = por_unico(price, function(x) suppressWarnings(as.numeric(x))))]
  d[price %in% SENTINELAS, price := NA_real_]
  d[, med := median(price[price %between% c(PRECIO_MIN, PRECIO_MAX)], na.rm = TRUE), by = .(fuel, anio = year(date))]
  d[, price := fcase(is.na(med) | price <= 2 * med, price,
                     abs(price / 10 - med) / med <= TOL_MEDIANA, price / 10,
                     abs(price / 100 - med) / med <= TOL_MEDIANA, price / 100)]
  d[, med := NULL]
  d[price %between% c(PRECIO_MIN, PRECIO_MAX)]
}

# ---- Régimen antiguo (2012-2022) ----

message("Régimen antiguo")
antiguo <- leer_regimen(ANIOS[ANIOS < year(CORTE_REGIMEN)], sep = ";", columnas = c(
  "id", NA, "distributor", NA, NA, "municipality", "region", "price", "date", "fuel", "latitud", "longitud")) |>
  informar("leídas") |>
  limpiar_campos() |>
  informar("sin registros de prueba y con coordenadas válidas") |>
  limpiar_precios() |>
  informar("combustibles del análisis con precio válido")

# ---- Régimen actual (2023-2026) ----

message("Régimen actual")
actual <- leer_regimen(ANIOS[ANIOS >= year(CORTE_REGIMEN)], sep = ",", columnas = c(
  "id", NA, "distributor", NA, "latitud", "longitud", "municipality", "region", "fuel", "price", NA, NA,
  "date", "time", "ev_station", "gas_station")) |>
  informar("leídas") |>
  limpiar_campos() |>
  informar("sin registros de prueba y con coordenadas válidas")
actual[, time := por_unico(time, as.ITime)]
# una coordenada por estación: la más frecuente
actual[, n := .N, by = .(id, latitud, longitud)]
actual[, c("latitud", "longitud") := { i <- which.max(n); .(latitud[i], longitud[i]) }, by = id]
actual[, n := NULL]
actual <- limpiar_precios(actual) |>
  informar("combustibles asistidos con precio válido")

# ---- Empalme de los dos regímenes ----

# Las coordenadas del régimen actual son más exactas: cada estación toma su ubicación más
# frecuente del actual y esta reemplaza a la del antiguo
ubicacion <- actual[, .N, by = .(id, latitud, longitud, municipality, region)]
setorder(ubicacion, id, latitud, longitud, municipality, region, na.last = TRUE)
ubicacion <- ubicacion[ubicacion[, .I[which.max(N)], by = id]$V1][, N := NULL]
antiguo[ubicacion, on = "id", `:=`(latitud = i.latitud, longitud = i.longitud,
                                   municipality = i.municipality, region = i.region)]

precios <- rbindlist(list(antiguo[date < CORTE_REGIMEN], actual[date >= CORTE_REGIMEN]), use.names = TRUE, fill = TRUE) |>
  informar("empalme") |>
  unique() |>
  informar("sin filas duplicadas")
precios[, c("ev_station", "gas_station") := NULL]   # no se usan
setorder(precios, id, date, fuel, time, na.last = TRUE)

fwrite(precios, ruta_proc("precios.csv"))
message(sprintf("01_precios.R: %s códigos de estación, %s a %s", uniqueN(precios$id), min(precios$date), max(precios$date)))
