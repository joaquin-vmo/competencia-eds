# run_all.R
#
# Script maestro: corre la construcción de la base y luego las estimaciones. Cada script se
# ejecuta en un proceso de R nuevo, así ninguno depende del estado que deja otro. La salida
# de cada uno queda en logs/<script>.log y los tiempos en logs/tiempos.csv.
#
# Uso, desde la carpeta replication/:
#   Rscript run_all.R                  todo
#   Rscript run_all.R construccion     solo code/01_construccion
#   Rscript run_all.R estimacion       solo code/02_estimacion (requiere la construcción)
# El .Rprofile de esta carpeta activa renv, así cada proceso usa las versiones de renv.lock.

# Etapas a correr ---------------------------------------------------------

# fuera de run_all por su tiempo (bootstrap); se corre a mano: Rscript code/02_estimacion/19_imputacion_tipo.R
FUERA <- "19_imputacion_tipo.R"

ETAPAS <- c("construccion", "estimacion")
args <- commandArgs(trailingOnly = TRUE)
etapas <- if (length(args)) args else ETAPAS
if (!all(etapas %in% ETAPAS)) stop("etapa desconocida; usar: ", paste(ETAPAS, collapse = ", "))

raiz <- here::here()
dir.create(file.path(raiz, "logs"), showWarnings = FALSE)
for (d in c(file.path("data", "processed"), "outputs")) dir.create(file.path(raiz, d), showWarnings = FALSE)

tiempos <- data.frame(script = character(), segundos = numeric())
inicio_total <- Sys.time()

# corre code/<carpeta>/<archivo> en un proceso nuevo y anota su tiempo
correr <- function(carpeta, archivo) {
  nombre <- file.path(carpeta, archivo)
  log <- file.path(raiz, "logs", sub("\\.R$", ".log", archivo))
  message(sprintf("%s  %-40s", format(Sys.time(), "%H:%M:%S"), nombre), appendLF = FALSE)
  t0 <- Sys.time()
  # R_MAX_VSIZE: Sun y Abraham en el panel semanal (13_panel_semanal.R) supera el tope de 16 GB de
  # memoria virtual que R fija por defecto en macOS; con 8 GB de RAM el sistema usa memoria comprimida
  estado <- system2(file.path(R.home("bin"), "Rscript"), shQuote(file.path(raiz, "code", nombre)),
                    stdout = log, stderr = log, env = "R_MAX_VSIZE=32Gb")
  seg <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf(" %7.1f s", seg))
  if (estado != 0) stop(nombre, " terminó con error; ver ", log)
  tiempos[nrow(tiempos) + 1, ] <<- list(nombre, round(seg, 1))
}

# Construcción de la base -------------------------------------------------

if ("construccion" %in% etapas) {
  message("1.1. Precios de Bencina en Línea y costos mayoristas")
  correr("01_construccion", "01_precios.R")
  correr("01_construccion", "02_costos.R")

  message("1.2. Identidad de las estaciones, entradas y paneles mensual y semanal")
  correr("01_construccion", "03_paneles.R")

  message("1.3. Cambios de precio, traspaso en las actualizaciones del MEPCO y comovimiento")
  correr("01_construccion", "04_cambios_precio.R")
  correr("01_construccion", "05_traspaso.R")
  correr("01_construccion", "06_comovimiento.R")
}

# Estimación --------------------------------------------------------------

if ("estimacion" %in% etapas) {
  message("2.1. Contexto y datos (sección 3)")
  correr("02_estimacion", "01_descriptivos.R")
  correr("02_estimacion", "02_muestra.R")

  message("2.2. Efecto de la entrada y alcance espacial (secciones 5.1 y 5.2)")
  correr("02_estimacion", "03_es_principal.R")
  correr("02_estimacion", "04_atenuacion.R")
  correr("02_estimacion", "05_comovimiento.R")

  message("2.3. Distribución de precios y heterogeneidad (secciones 5.3 y 5.4)")
  correr("02_estimacion", "06_distribucion.R")
  correr("02_estimacion", "17_tipo_entrante.R")
  correr("02_estimacion", "18_robustez_tipo.R")
  correr("02_estimacion", "20_alcance_independientes.R")
  correr("02_estimacion", "07_ventanas.R")

  message("2.4. Mecanismo: el ajuste en los ciclos del MEPCO (sección 5.5)")
  correr("02_estimacion", "08_traspaso.R")
  correr("02_estimacion", "16_mecanismo.R")

  message("2.5. Robustez (sección 6 y anexos)")
  correr("02_estimacion", "09_placebos.R")
  correr("02_estimacion", "10_persistencia.R")
  correr("02_estimacion", "11_autopista.R")
  correr("02_estimacion", "12_errores_conley.R")
  correr("02_estimacion", "13_panel_semanal.R")
  correr("02_estimacion", "14_honest_did.R")
  correr("02_estimacion", "15_validacion_margen.R")
}

# Resumen -----------------------------------------------------------------

# un script nuevo en code/ que no esté en la lista de arriba no se corre: se avisa
faltan <- setdiff(unlist(lapply(c(construccion = "01_construccion", estimacion = "02_estimacion")[etapas],
                                function(e) file.path(e, list.files(file.path(raiz, "code", e), "\\.R$")))),
                  tiempos$script)
faltan <- setdiff(faltan, file.path("02_estimacion", FUERA))
if (length(faltan)) warning("scripts sin correr porque no están en run_all.R: ", paste(faltan, collapse = ", "))

total <- as.numeric(difftime(Sys.time(), inicio_total, units = "mins"))
write.csv(tiempos, file.path(raiz, "logs", "tiempos.csv"), row.names = FALSE)
message(sprintf("Listo: %d scripts en %.1f minutos (%s, R %s, %s)", nrow(tiempos), total,
                format(Sys.time(), "%Y-%m-%d"), getRversion(), R.version$platform))
