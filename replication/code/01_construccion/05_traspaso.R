# 05_traspaso.R
#
# Cambio de precio de cada tratada en cada actualización del costo MEPCO de la gasolina 93,
# frente al cambio medio de los controles estrictos de su región en la misma actualización.
# En la actualización u el cambio se mide entre el día previo (u - 1) y u + DIAS_DESPUES,
# con el precio vigente (último reporte de a lo más VIGENCIA días antes).
# La entrada se fecha en el primer día con reporte de la entrante más cercana que trata a la
# estación (ventana 2012_2026), y se toman las actualizaciones a +-K_SEMANAS de esa fecha.
# El cambio se separa además en el día de actualización y el siguiente (u - 1 a u + DIA_CORTE)
# y el resto de la ventana (u + DIA_CORTE a u + DIAS_DESPUES); ambas partes suman el total.
#
# Entrada: data/processed/{precios,costo_mepco,identidad_estaciones}.csv, panel_2012_2026/panel_mensual.csv
# Salida:  data/processed/traspaso_actualizaciones.csv (una fila por tratada y actualización)

source(here::here("code", "00_configuracion.R"))

DESDE_ENTRADA <- as.Date("2014-10-01")   # primer trimestre completo con costo MEPCO
HASTA_ENTRADA <- as.Date("2025-12-31")
FIN_COSTO     <- as.Date("2026-02-28")   # el 26-mar-2026 se desactiva la estabilización y el costo salta
K_SEMANAS     <- 68L   # semanas a cada lado de la entrada (cubre la ventana de un año del Gráfico B.4)
DIAS_DESPUES  <- 6L    # días tras la actualización en que se mide el precio
DIA_CORTE     <- 1L    # el día de actualización o el siguiente (Tabla 1)
MIN_CTRL_REG  <- 5L    # controles mínimos de la región en cada actualización
VIGENCIA      <- 91L   # días máximos desde el último reporte (13 semanas, como el panel)

# ---- Tratadas y controles estrictos ----

est <- leer_csv(file.path(dir_ventana("2012_2026"), "panel_mensual.csv"),
               col_select = c(station_key, region, role_entry, entrante_key, fecha_entrante)) |>
  distinct(station_key, .keep_all = TRUE)
trat <- est |>
  filter(role_entry == "treated", fecha_entrante >= DESDE_ENTRADA, fecha_entrante <= HASTA_ENTRADA) |>
  select(station_key, region, fecha_e = fecha_entrante)
ctrl <- est |> filter(role_entry == "ctrl_b") |> select(station_key, region)

# ---- Actualizaciones del costo y precio diario ----

# actualizaciones del costo de la 93
upd <- leer_csv(ruta_proc("costo_mepco.csv")) |>
  transmute(u = date, c = `93_w/`) |>
  arrange(u) |>
  mutate(dc = c - lag(c)) |>
  filter(!is.na(dc), dc != 0, u >= DESDE_ENTRADA, u <= FIN_COSTO) |>
  select(u, dc)

# precio del último reporte de cada día, por estación
idn <- leer_csv(ruta_proc("identidad_estaciones.csv"), col_select = c(id, station_key))
p <- leer_csv(ruta_proc("precios.csv"), col_select = c(id, price, date, time, fuel),
              col_types = cols(id = "c", fuel = "c", date = "D")) |>
  filter(fuel == "93") |>
  inner_join(idn, by = "id") |>
  filter(station_key %in% c(trat$station_key, ctrl$station_key)) |>
  arrange(station_key, date, time) |>
  filter(!duplicated(consecutive_id(station_key, date), fromLast = TRUE)) |>
  select(station_key, date, price)

# precio vigente: el del último reporte a lo más VIGENCIA días antes
vigente <- function(sk, d) {
  tibble(station_key = sk, d = d) |>
    left_join(p, join_by(station_key, closest(d >= date))) |>
    mutate(price = if_else(d - date <= VIGENCIA, price, NA)) |>
    pull(price)
}
cambio_log <- function(x) {   # x: station_key, u
  x |>
    mutate(antes = vigente(station_key, u - 1L), corte = vigente(station_key, u + DIA_CORTE),
           despues = vigente(station_key, u + DIAS_DESPUES)) |>
    filter(!is.na(antes), !is.na(corte), !is.na(despues)) |>
    transmute(station_key, u, antes, dlp = 100 * (log(despues) - log(antes)),
              dlp_01 = 100 * (log(corte) - log(antes)), dlp_26 = 100 * (log(despues) - log(corte)))
}

# ---- Cambio de las tratadas frente a los controles de su región ----

# tratadas: actualizaciones a +-K_SEMANAS de la entrada; k = semanas desde la entrada
tu <- cross_join(trat, select(upd, u)) |>
  mutate(k = as.integer(u - fecha_e) %/% 7L) |>
  filter(abs(as.integer(u - fecha_e)) <= 7L * K_SEMANAS + 6L, k >= -K_SEMANAS, k <= K_SEMANAS)
td <- inner_join(tu, cambio_log(select(tu, station_key, u)), by = c("station_key", "u"))

# controles de la misma región: cambio medio en cada actualización que usan las tratadas
cr <- expand_grid(station_key = sort(unique(ctrl$station_key), method = "radix"), u = sort(unique(td$u))) |>
  cambio_log() |>
  inner_join(ctrl, y = _, by = "station_key") |>
  summarise(dlp_cr = mean(dlp), dlp_01_cr = mean(dlp_01), dlp_26_cr = mean(dlp_26), n_cr = n(), .by = c(u, region)) |>
  filter(n_cr >= MIN_CTRL_REG)

# NA si la región tiene menos de MIN_CTRL_REG controles
td <- td |>
  left_join(cr, by = c("u", "region")) |>
  inner_join(upd, by = "u") |>
  mutate(dif_log_reg = dlp - dlp_cr, dif_01 = dlp_01 - dlp_01_cr, dif_26 = dlp_26 - dlp_26_cr) |>
  arrange(station_key, u)
stopifnot("las dos partes no suman el cambio total" =
            with(filter(td, !is.na(dif_log_reg)), all(abs(dif_01 + dif_26 - dif_log_reg) < 1e-8)))

td |>
  select(station_key, region, fecha_e, u, k, dc, antes, dlp, dlp_cr, n_cr, dif_log_reg, dif_01, dif_26) |>
  escribir_csv(ruta_proc("traspaso_actualizaciones.csv"))
message(sprintf("05_traspaso.R: %d tratadas, %s tratada-actualizaciones", n_distinct(td$station_key),
                format(nrow(td), big.mark = ",")))
