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
CORTE_REGIMEN <- as.Date("2023-01-01")   # inicio del régimen actual

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
  if_else(is.na(i), x, unname(dic[i]))
}

informar <- function(d, paso) {
  message(sprintf("  %-50s %s filas", paso, format(nrow(d), big.mark = ",")))
  d
}

# todas las columnas como texto; `columnas`: nombres en orden del archivo, NA = no se lee
leer_regimen <- function(anios, sep, columnas) {
  map(anios, function(a) {
    f <- ruta_raw("bencina_en_linea", paste0(a, ".csv"))
    if (ncol(read_delim(f, delim = sep, n_max = 0L, col_types = cols(.default = "c"), show_col_types = FALSE)) != length(columnas))
      stop(basename(f), ": se esperaban ", length(columnas), " columnas")
    read_delim(f, delim = sep, col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8"),
               na = c("", "NA"), col_select = which(!is.na(columnas)), progress = FALSE) |>
      set_names(columnas[!is.na(columnas)])
  }) |>
    list_rbind()
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
  d |>
    filter(!id %in% c("prueba", "CNE 01")) |>   # registros de prueba
    mutate(
      id = por_unico(id, function(x) tolower(trimws(if_else(x == "co730401 co730401", "co730401", x, missing = NA_character_)))),
      distributor = por_unico(distributor, function(x) recodificar(tolower(trimws(x)), DISTRIBUIDOR)),
      municipality = por_unico(municipality, function(x) normalizar_lugar(recodificar(tolower(trimws(x)), COMUNA))),
      region = por_unico(region, function(x) {
        x <- tolower(trimws(gsub("\\p{Cf}", "", x, perl = TRUE)))   # carácter invisible
        recodificar(if_else(grepl("libertador", x, fixed = TRUE), "ohiggins", x), REGION)
      }),
      date = por_unico(date, as.Date),
      latitud  = por_unico(latitud, a_numero),
      longitud = por_unico(longitud, a_numero)
    ) |>
    filter(between(latitud, -90, 0), between(longitud, -180, 0))   # en Chile ambas son negativas
}

# precio sobre el doble de la mediana del combustible-año: se divide por 10 o 100 si así
# queda a menos de TOL_MEDIANA de la mediana (decimales corridos); lo demás fuera de rango sale
limpiar_precios <- function(d) {
  d |>
    filter(fuel %in% names(COMBUSTIBLE)) |>
    mutate(fuel = unname(COMBUSTIBLE[fuel]), price = por_unico(price, function(x) suppressWarnings(as.numeric(x)))) |>
    mutate(price = if_else(price %in% SENTINELAS, NA_real_, price)) |>
    mutate(anio = year(date)) |>
    mutate(med = median(price[between(price, PRECIO_MIN, PRECIO_MAX)], na.rm = TRUE), .by = c(fuel, anio)) |>
    mutate(price = case_when(is.na(med) | price <= 2 * med ~ price,
                             abs(price / 10 - med) / med <= TOL_MEDIANA ~ price / 10,
                             abs(price / 100 - med) / med <= TOL_MEDIANA ~ price / 100)) |>
    select(-med, -anio) |>
    filter(between(price, PRECIO_MIN, PRECIO_MAX))
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
actual <- actual |>
  mutate(time = por_unico(time, hms::as_hms)) |>
  # una coordenada por estación: la más frecuente
  add_count(id, latitud, longitud) |>
  mutate(latitud = latitud[which.max(n)], longitud = longitud[which.max(n)], .by = id) |>
  select(-n) |>
  limpiar_precios() |>
  informar("combustibles asistidos con precio válido")

# ---- Empalme de los dos regímenes ----

# Las coordenadas del régimen actual son más exactas: cada estación toma su ubicación más
# frecuente del actual y esta reemplaza a la del antiguo
ubicacion <- actual |>
  count(id, latitud, longitud, municipality, region) |>
  arrange(id, latitud, longitud, municipality, region) |>
  slice(which.max(n), .by = id) |>
  select(-n)
antiguo <- rows_update(antiguo, ubicacion, by = "id", unmatched = "ignore")

precios <- bind_rows(filter(antiguo, date < CORTE_REGIMEN), filter(actual, date >= CORTE_REGIMEN)) |>
  informar("empalme") |>
  distinct() |>
  informar("sin filas duplicadas") |>
  select(-ev_station, -gas_station) |>   # no se usan
  arrange(id, date, fuel, time)

escribir_csv(precios, ruta_proc("precios.csv"))
message(sprintf("01_precios.R: %s códigos de estación, %s a %s", n_distinct(precios$id), min(precios$date), max(precios$date)))
