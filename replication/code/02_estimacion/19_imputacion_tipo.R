# 19_imputacion_tipo.R
#
# Estimador de imputación de Borusyak, Jaravel y Spiess (2024) por tipo de entrante, con la muestra
# y los efectos fijos de 17_tipo_entrante.R. Los efectos fijos se estiman en las observaciones no
# tratadas (controles y tratadas antes de la entrada); el efecto es observado - imputado, promediado
# por tipo y por bin desde la entrada. Errores estándar: bootstrap por comuna (NBOOT réplicas).
# run_all.R no lo corre por su tiempo (unos 10 minutos); se corre a mano desde replication/:
#   Rscript code/02_estimacion/19_imputacion_tipo.R > logs/19_imputacion_tipo.log 2>&1
#   Gráfico B.18: estudio de eventos por tipo; el log imprime el ATT por tipo y su diferencia.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/grafico_B18_imputacion_tipo.pdf

source(here::here("code", "00_configuracion.R"))
library(fixest)
set.seed(20261107)

TIPOS <- c("De marca", "Independiente")
FV <- c(p93 = "Gasolina 93", pdi = "Diésel")
NBOOT <- 199L
ATT <- 99L   # marca de la fila del ATT en la columna rel

d <- muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal) |>
  mutate(post = as.integer(treated == 1L & ym >= g_evento),
         tipo = if_else(treated == 1L, TIPOS[1L + !(entrante_marca %in% MARCAS_MAYORES)], NA_character_))

imputa <- function(z, fv) {
  z <- mutate(z, y = log(.data[[fv]]) * 100)
  m <- feols(as.formula(sprintf("y ~ 1 | %s", FE_PRINCIPAL)), data = filter(z, post == 0L), notes = FALSE, warn = FALSE)
  t <- filter(z, post == 1L)
  t |> mutate(tau = y - predict(m, newdata = t)) |> filter(!is.na(tau))   # NA: algún efecto fijo sin observaciones no tratadas
}
resumen <- function(t) {
  bind_rows(summarise(t, rel = ATT, att = mean(tau), .by = tipo),
            summarise(t, att = mean(tau), .by = c(tipo, rel)),
            tibble(tipo = "dif", rel = ATT, att = mean(t$tau[t$tipo == TIPOS[2]]) - mean(t$tau[t$tipo == TIPOS[1]])))
}

imp <- map(names(FV), function(fv) {
  z <- filter(d, !is.na(.data[[fv]]))
  t <- imputa(z, fv)
  sin <- anti_join(filter(z, post == 1L), t, by = c("station_key", "ym"))
  message(sprintf("%s: %d de %d obs. tratadas después de la entrada sin imputar (%d tratadas; %s)", FV[[fv]], nrow(sin),
                  sum(z$post == 1L), n_distinct(sin$station_key), paste(summarise(sin, N = n(), .by = tipo) |> mutate(txt = paste(tipo, N)) |> pull(txt), collapse = ", ")))
  coms <- unique(z$comuna)
  bs <- map(seq_len(NBOOT), function(b) {
    s <- tibble(comuna = sample(coms, replace = TRUE)) |> mutate(copia = row_number())
    zb <- s |>
      left_join(z, by = "comuna", relationship = "many-to-many") |>
      mutate(station_key = paste(station_key, copia))   # cada copia, otro cluster
    resumen(imputa(zb, fv))
  }) |> list_rbind()
  bs |>
    summarise(se = sd(att), .by = c(tipo, rel)) |>
    left_join(resumen(t), by = c("tipo", "rel")) |>
    select(tipo, rel, att, se) |>
    mutate(fv = fv)
}) |>
  list_rbind()
imp |> filter(rel == ATT) |> transmute(fv, tipo, att = round(att, 3), se = round(se, 3), p = round(2 * pnorm(-abs(att / se)), 4)) |> print()   # cifras que cita el texto
imp |> filter(rel != ATT, tipo %in% TIPOS) |>
  pivot_wider(id_cols = c(fv, rel), names_from = tipo, values_from = att) |> arrange(fv, rel) |>
  mutate(across(where(is.numeric), \(v) round(v, 3))) |> print(n = Inf)

g <- imp |>
  filter(rel != ATT, tipo %in% TIPOS) |>
  transmute(event_time = rel, estimate = att, se, grupo = factor(tipo, levels = TIPOS), combustible = factor(unname(FV[fv]), levels = FV))
guardar(grafico_es(g, "grupo") + scale_x_continuous(breaks = 0:NBIN, labels = c(0:(NBIN - 1), sprintf("≥%d", NBIN))),
        "grafico_B18_imputacion_tipo.pdf", alto = 3)
