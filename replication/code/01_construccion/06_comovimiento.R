# 06_comovimiento.R
#
# Base de la respuesta de las rivales a un cambio de precio (Gráficos 7 y B.9), con el registro
# de cambios de la CNE con fecha y hora (régimen de reporte actual, 2015-06 a 2026).
#   1. Eventos: cambios efectivos de precio por estación-combustible, sin reversiones A -> B -> A
#      de menos de REVERSION_H horas (errores de digitación).
#   2. Estaciones: atributos modales desde INI y marca (Copec, Shell, Aramco u Otras).
#   3. Pares: cada cambio de precio "líder" de domingo a martes (días sin cambio de costo) entre
#      INI_SIM y FIN_SIM, con cada rival de otra marca a <= 10 km. Para cada par:
#        sigue_24: el siguiente cambio de la rival es en la misma dirección y dentro de 24 h,
#        iguala:   sigue_24 y fija el mismo precio que la líder (+-0,5 $/L).
#      Solo pares estación-combustible con reporte en todo INI..FIN (panel balanceado).
#      sgn: dirección del cambio líder (1 alza, -1 baja); marca_i: marca de la líder.
#
# Entrada: data/raw/bencina_en_linea/{2023..2026}.csv, data/processed/{identidad_estaciones,costo_mepco}.csv
# Salida:  data/processed/comovimiento_pares.csv.gz (una fila por cambio líder y rival)

source(here::here("code", "00_configuracion.R"))

DESDE_EVENTOS <- as.Date("2015-06-01")
INI <- as.Date("2021-01-07")    # panel balanceado: dos años de régimen semanal
FIN <- as.Date("2025-12-31")    # antes del salto del MEPCO de marzo de 2026
INI_SIM <- REFORMA_MEPCO - 364L  # 52 semanas a cada lado de la reforma
FIN_SIM <- REFORMA_MEPCO + 363L
HOLGURA_BAL <- 90L               # días: primer evento <= INI + 90 y último >= FIN - 90
REVERSION_H <- 2                 # horas
RADIO_RIVAL <- 10                # km
HORAS_RESPUESTA <- 24
TOL_IGUALA <- 0.5                # $/L
COMB <- c("93", "95", "97", "DI")
COLS <- c("codigo", "distribuidor", "latitud", "longitud", "nom_comuna", "nom_region",
          "combustible", "precio", "fecha_actualizacion", "hora_actualizacion")

# ==============================================================================
# 1. Eventos
# ==============================================================================

# 2023.csv trae también fechas anteriores (desde 2015); 2024..2026.csv, solo las de su año
leer <- function(anio, desde, hasta) {
  x <- read_csv(ruta_raw("bencina_en_linea", paste0(anio, ".csv")), col_select = all_of(COLS), col_types = cols(.default = "c"),
                na = "NA", progress = FALSE) |>
    set_names(c("id", "distribuidor", "lat", "lon", "comuna", "region", "fuel", "precio", "fecha", "hora")) |>
    filter(fuel %in% COMB)
  u <- unique(x$fecha)
  x |>
    mutate(fecha = as.Date(u)[match(fecha, u)]) |>
    filter(fecha >= desde, fecha <= hasta)
}
x <- bind_rows(leer(2023, DESDE_EVENTOS, as.Date("2023-12-31")),
               map(2024:2026, \(a) leer(a, as.Date(sprintf("%d-01-01", a)), as.Date(sprintf("%d-12-31", a)))) |> list_rbind()) |>
  filter(!grepl("prueba|cne", id, ignore.case = TRUE))

h <- unique(x$hora)
seg <- as.numeric(as.POSIXct(paste("1970-01-01", h), tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
# texto no numérico en precio o coordenadas pasa a NA (y sale abajo o en `est`)
x <- x |>
  mutate(precio = suppressWarnings(as.numeric(precio)), lat = suppressWarnings(as.numeric(lat)), lon = suppressWarnings(as.numeric(lon)),
         ts = .POSIXct(as.numeric(fecha) * 86400 + seg[match(hora, h)], tz = "UTC")) |>
  filter(!is.na(ts), !is.na(precio)) |>
  # precios dentro de [0,6; 1,5] x la mediana del combustible-mes
  group_by(fuel, m = format(fecha, "%Y-%m")) |>
  mutate(med = median(precio)) |>
  ungroup() |>
  filter(precio >= 0.6 * med, precio <= 1.5 * med) |>
  select(-med, -m)

# código -> estación, con las recodificaciones de 03_paneles.R
idn <- leer_csv(ruta_proc("identidad_estaciones.csv"), col_select = c(id, station_key))
x <- x |>
  left_join(idn, by = "id") |>
  mutate(station_key = coalesce(station_key, id)) |>
  # un precio por estación-combustible-instante (el último leído); solo cambios efectivos
  arrange(station_key, fuel, ts) |>
  filter(!duplicated(consecutive_id(station_key, fuel, ts), fromLast = TRUE)) |>
  mutate(prev = lag(precio), .by = c(station_key, fuel)) |>
  filter(is.na(prev) | precio != prev) |>
  # reversiones A -> B -> A en menos de REVERSION_H horas: se botan B y la vuelta
  mutate(p_sig = lead(precio), t_sig = lead(ts), p_ant = lag(precio), .by = c(station_key, fuel)) |>
  mutate(error = !is.na(p_sig) & !is.na(p_ant) & p_sig == p_ant & as.numeric(difftime(t_sig, ts, units = "hours")) < REVERSION_H) |>
  mutate(vuelta = lag(error, default = FALSE), .by = c(station_key, fuel)) |>
  filter(!error, !vuelta) |>
  mutate(prev = lag(precio), .by = c(station_key, fuel)) |>
  filter(is.na(prev) | precio != prev) |>
  mutate(dp = precio - prev)

# ==============================================================================
# 2. Estaciones
# ==============================================================================

modal_alfa <- function(v) {   # valor más frecuente; empate: el primero en orden alfabético
  v <- v[!is.na(v) & v != ""]
  if (!length(v)) NA else names(which.max(table(v)))
}
est <- x |>
  filter(fecha >= INI) |>
  summarise(distribuidor = modal_alfa(toupper(trimws(distribuidor))),
            lat = as.numeric(modal_alfa(round(lat, 5))), lon = as.numeric(modal_alfa(round(lon, 5))),
            .by = station_key) |>
  mutate(marca = case_when(distribuidor == "COPEC" ~ "Copec", distribuidor == "SHELL" ~ "Shell",
                           distribuidor %in% c("ARAMCO", "PETROBRAS") ~ "Aramco", .default = "Otras")) |>
  filter(!is.na(lat), lat < 0, lon < 0)
e <- x |>
  filter(station_key %in% est$station_key) |>
  select(station_key, fuel, ts, fecha, precio, dp)

# ==============================================================================
# 3. Pares líder-rival
# ==============================================================================

# panel balanceado: estación-combustible con reporte en todo INI..FIN y en ambos regímenes
bal <- e |>
  summarise(ini = min(fecha), fin = max(fecha),
            n_pre = sum(fecha >= INI & fecha < REFORMA_MEPCO), n_post = sum(fecha >= REFORMA_MEPCO & fecha <= FIN),
            .by = c(station_key, fuel)) |>
  filter(ini <= INI + HOLGURA_BAL, fin >= FIN - HOLGURA_BAL, n_pre > 0, n_post > 0) |>
  select(station_key, fuel)

# día del ciclo: días desde la última actualización del costo (cualquier combustible)
costo <- leer_csv(ruta_proc("costo_mepco.csv")) |>
  transmute(jueves = date, `93` = `93_w/`, `97` = `97_w/`, di = `di_w/`) |>
  pivot_longer(-jueves, names_to = "fuel", values_to = "c") |>
  arrange(fuel, jueves) |>
  mutate(dc = c - lag(c), .by = fuel)
upd <- tibble(f_upd = sort(unique(filter(costo, dc != 0)$jueves)))
ciclo <- tibble(fecha = seq(INI, FIN, by = 1L)) |>
  left_join(upd, join_by(closest(fecha >= f_upd))) |>
  transmute(fecha, dia_ciclo = as.integer(fecha - f_upd))

e <- bal |>
  inner_join(e, by = c("station_key", "fuel")) |>
  filter(fecha >= INI_SIM - 7L, fecha <= FIN_SIM + 7L, !is.na(dp)) |>
  inner_join(ciclo, by = "fecha") |>
  mutate(sgn = sign(dp), reg = if_else(fecha >= REFORMA_MEPCO, "tri", "sem")) |>
  arrange(fecha)

# líderes: cambios de domingo a martes (día 3 a 5 del ciclo semanal, que parte el jueves)
lid <- e |>
  filter(fecha >= INI_SIM, fecha <= FIN_SIM - 2L, (dia_ciclo %% 7L) %in% 3:5) |>
  transmute(i = station_key, fuel, ts0 = ts, p_lid = precio, sgn, reg, dia_ciclo) |>
  mutate(ev = row_number())

# rivales: otra marca a <= RADIO_RIVAL km (dos estaciones "Otras" también son rivales)
D <- dist_propia(est$lat, est$lon)
misma <- outer(est$marca, est$marca, "==") & est$marca != "Otras"
ix <- which(D <= RADIO_RIVAL & !misma, arr.ind = TRUE)
rivales <- tibble(i = est$station_key[ix[, 1]], j = est$station_key[ix[, 2]], d = D[ix]) |>
  mutate(dist = cut(d, c(0, 1, 2, 3, 5, 10), labels = c("0-1", "1-2", "2-3", "3-5", "5-10"))) |>
  filter(!is.na(dist))   # d = 0: dos códigos en el mismo sitio, sin anillo

pares <- rivales |>
  inner_join(lid, by = "i", relationship = "many-to-many")
pares <- inner_join(rename(bal, j = station_key), pares, by = c("j", "fuel"), relationship = "many-to-many")

# siguiente cambio de la rival en o después del cambio líder (join rodante hacia adelante)
ej <- e |> select(j = station_key, fuel, ts, p_j = precio, sgn_j = sgn)
sig <- pares |>
  select(j, fuel, ts0, ev) |>
  left_join(ej, join_by(j, fuel, closest(ts0 <= ts))) |>
  select(ev, j, ts_sig = ts, p_sig = p_j, sgn_sig = sgn_j)

pares <- pares |>
  left_join(sig, by = c("ev", "j")) |>
  mutate(horas = as.numeric(difftime(ts_sig, ts0, units = "hours"))) |>
  mutate(sigue_24 = 100 * coalesce(!is.na(ts_sig) & horas <= HORAS_RESPUESTA & sgn_sig == sgn, FALSE)) |>
  mutate(iguala = 100 * coalesce(sigue_24 > 0 & abs(p_sig - p_lid) < TOL_IGUALA, FALSE)) |>
  # muestra de la tesis: régimen semanal y semanas 2 y 3 del ciclo trisemanal (sin el traspaso por etapas);
  # la semana 1 del trisemanal se guarda para la robustez
  mutate(muestra = case_when(reg == "sem" ~ "Semanal (2022)", dia_ciclo >= 7L ~ "Trisemanal, semanas 2 y 3 (2023)",
                             .default = "Trisemanal, semana 1 (2023)")) |>
  left_join(select(est, i = station_key, marca_i = marca), by = "i")
stopifnot("un cambio líder o una respuesta cae en jueves o viernes" =
            !any(wday(pares$ts0) %in% 5:6) && !any(wday(pares$ts_sig[pares$sigue_24 > 0]) %in% 5:6))

pares |>
  filter(!is.na(muestra)) |>
  select(ev, i, j, marca_i, fuel, reg, dia_ciclo, muestra, dist, sgn, sigue_24, iguala) |>
  escribir_csv(ruta_proc("comovimiento_pares.csv.gz"))
message(sprintf("06_comovimiento.R: %s cambios líderes, %s pares líder-rival",
                format(n_distinct(pares$ev), big.mark = ","), format(nrow(pares), big.mark = ",")))
