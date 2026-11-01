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

ETAPAS <- c(construccion = "01_construccion", estimacion = "02_estimacion")
args <- commandArgs(trailingOnly = TRUE)
etapas <- if (length(args)) ETAPAS[args] else ETAPAS
if (anyNA(etapas)) stop("etapa desconocida; usar: ", paste(names(ETAPAS), collapse = ", "))

raiz <- here::here()
dir.create(file.path(raiz, "logs"), showWarnings = FALSE)
for (d in c(file.path("data", "processed"), "outputs")) dir.create(file.path(raiz, d), showWarnings = FALSE)

scripts <- unlist(lapply(etapas, function(e) sort(list.files(file.path(raiz, "code", e), "\\.R$", full.names = TRUE))))
tiempos <- data.frame(script = character(), segundos = numeric())
inicio_total <- Sys.time()

for (s in scripts) {
  nombre <- file.path(basename(dirname(s)), basename(s))
  log <- file.path(raiz, "logs", sub("\\.R$", ".log", basename(s)))
  message(sprintf("%s  %-40s", format(Sys.time(), "%H:%M:%S"), nombre), appendLF = FALSE)
  t0 <- Sys.time()
  estado <- system2(file.path(R.home("bin"), "Rscript"), shQuote(s), stdout = log, stderr = log)
  seg <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf(" %7.1f s", seg))
  if (estado != 0) stop(nombre, " terminó con error; ver ", log)
  tiempos[nrow(tiempos) + 1, ] <- list(nombre, round(seg, 1))
}

total <- as.numeric(difftime(Sys.time(), inicio_total, units = "mins"))
write.csv(tiempos, file.path(raiz, "logs", "tiempos.csv"), row.names = FALSE)
message(sprintf("Listo: %d scripts en %.1f minutos (%s, R %s, %s)", length(scripts), total,
                format(Sys.time(), "%Y-%m-%d"), getRversion(), R.version$platform))
