# 14_honest_did.R
#
# Tabla C.7: sensibilidad a tendencias paralelas (Rambachan y Roth, 2023, magnitudes relativas)
# del estudio de eventos principal de Sun y Abraham (bins agregados, vcov W V W'). Para el bin de impacto
# (meses 0 a 5) y el de meses 6 a 11:
# estimador, IC al 95 % y el Mbar de quiebre, el mayor Mbar de la grilla con el que el
# intervalo robusto aún excluye el cero.
#
# Entrada: data/processed/panel_2012_2026/panel_mensual.csv
# Salida:  outputs/tabla_C07_honest_did.tex

source(here::here("code", "00_configuracion.R"))
library(fixest)
library(HonestDiD)

MBAR    <- seq(0.25, 3, by = 0.25)
ALPHA   <- 0.05
GRID_SE <- 60   # semiancho de la búsqueda del intervalo, en errores estándar
HORIZONTES <- c(impacto = 1L, meses_6_11 = 2L)   # posición del bin entre los posteriores
ETIQUETA   <- c(impacto = "Impacto (meses 0 a 5)", meses_6_11 = "Meses 6 a 11")

# ---- Muestra ----

d <- cohortes_sunab(muestra_estacion(leer_panel("2012_2026"), CTRL_ESTRICTO, VENTANAS[["2012_2026"]]$focal))

# ---- Sensibilidad por combustible y bin ----

sensibilidad <- function(z, y) {
  m <- estimar_sa(z, y)
  p <- pesos_sa(m, z)
  bins <- setdiff(-NBIN:NBIN, -1L)                         # -4..-2 y 0..4
  r <- comb_sa(m, z, t(sapply(bins, function(k) w_sa(p, p$k == k))))
  stopifnot("la agregación no reproduce a fixest" = isTRUE(all.equal(unname(r$b), unname(coef(m)[sprintf("per::%d", bins)]), tolerance = 1e-8)))
  b <- unname(r$b)
  V <- unname(r$V)
  nm <- bins
  n_pre <- sum(bins < 0)
  n_post <- length(nm) - n_pre
  n <- n_estaciones(m, z)

  map(names(HORIZONTES), function(h) {
    lv <- replace(numeric(n_post), HORIZONTES[[h]], 1)
    est <- sum(lv * b[-seq_len(n_pre)])
    ee <- sqrt(drop(t(lv) %*% V[-seq_len(n_pre), -seq_len(n_pre)] %*% lv))
    excluye_cero <- function(mb) {
      r <- createSensitivityResults_relativeMagnitudes(b, V, n_pre, n_post, l_vec = lv, Mbarvec = mb, alpha = ALPHA,
                                                       grid.lb = est - GRID_SE * ee, grid.ub = est + GRID_SE * ee)
      r$lb > 0 | r$ub < 0
    }
    # bisección: el intervalo se ensancha con Mbar. 0 = "< 0,25", Inf = "> 3"
    quiebre <- if (excluye_cero(MBAR[length(MBAR)])) Inf else if (!excluye_cero(MBAR[1])) 0 else {
      lo <- 1L; hi <- length(MBAR)
      while (hi - lo > 1L) { mid <- (lo + hi) %/% 2L; if (excluye_cero(MBAR[mid])) lo <- mid else hi <- mid }
      MBAR[lo]
    }
    tibble(y = y, horizonte = h, est = est, lb = est - qnorm(1 - ALPHA / 2) * ee,
           ub = est + qnorm(1 - ALPHA / 2) * ee, quiebre = quiebre, tratadas = n[1], controles = n[2])
  }) |>
    list_rbind()
}

res <- map(names(PRECIOS), \(fv) sensibilidad(filter(d, !is.na(.data[[fv]])), fv)) |> list_rbind()

# ---- Tabla C.7 ----

fmt_quiebre <- function(q) if_else(q == 0, sprintf("$<$%.2f", MBAR[1]),
                                   if_else(is.infinite(q), sprintf("$>$%.0f", max(MBAR)), sprintf("%.2f", q)))
cuerpo <- unlist(lapply(names(HORIZONTES), function(h) {
  r <- res |> filter(horizonte == h) |> slice(match(names(PRECIOS), y))
  c(sprintf("\\multicolumn{5}{l}{\\emph{%s}} \\\\", ETIQUETA[[h]]),
    fila("\\quad Estimador", sprintf("%.3f", r$est)),
    fila("\\quad IC 95\\%", sprintf("[%.3f; %.3f]", r$lb, r$ub)),
    fila("\\quad $\\bar{M}$ de quiebre", fmt_quiebre(r$quiebre)))
}))
r1 <- res |> filter(horizonte == "impacto") |> slice(match(names(PRECIOS), y))
escribir_tabla("lcccc", c(fila("", unname(PRECIOS)), "\\midrule", cuerpo, "\\midrule",
                          fila("Tratadas", r1$tratadas), fila("Controles", r1$controles)),
               "tabla_C07_honest_did.tex")
