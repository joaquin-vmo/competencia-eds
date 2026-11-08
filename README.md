# Replicación: Efecto de la apertura de competidores sobre los precios en mercados locales

Código que reproduce todas las tablas y gráficos estimados de la tesis (`tesis.pdf`). Los datos
son públicos, pero no se incluyen en el repositorio por su tamaño (1,7 GB): ver [Datos brutos](#datos-brutos).

## Cómo correr

Desde la carpeta `replication/` (o abriendo `replication/replication.Rproj`), con los datos en `replication/data/raw/`:

```sh
Rscript -e 'renv::restore()'   # una vez: instala las versiones de renv.lock
Rscript run_all.R              # todo; también: run_all.R construccion | estimacion
```

Tiempo total: **35 minutos** en un Mac con Apple M1 y 8 GB de RAM (construcción
1,8 min, estimación 33,1 min; sin `19_imputacion_tipo.R`, que se corre aparte). El tiempo por script queda en `logs/tiempos.csv` y
la salida de cada uno en `logs/<script>.log`; las cifras que citan las notas de los gráficos
(número de tratadas, controles y cambios de precio) se imprimen ahí.

Requisitos: R 4.6.2 y las bibliotecas de sistema GDAL, GEOS y PROJ (para `sf`). `run_all.R` sube el
tope de memoria virtual de R a 32 GB (`R_MAX_VSIZE`), que necesita Sun y Abraham en el panel semanal.
Las figuras se guardan con las fuentes incrustadas (`quartz` en macOS, `cairo_pdf` en otros sistemas),
y tablas y ejes usan coma decimal. Usan la letra de la tesis, TeX Gyre Pagella, que debe estar instalada
en el sistema (viene con TeX Live: `texgyrepagella-*.otf`; en macOS, copiarla a `~/Library/Fonts`).

Después de correr, copiar `replication/outputs/*.pdf` a `tex/output/graficos/` y `*.tex` a
`tex/output/tablas/`:

```sh
cp replication/outputs/*.pdf tex/output/graficos/ && cp replication/outputs/*.tex tex/output/tablas/
```

## Estructura

```
tesis.pdf                             la tesis
replication/
  run_all.R                           script maestro
  renv.lock                           versiones de R y de los paquetes
  code/00_configuracion.R             parámetros, rutas, formato de tablas y gráficos, funciones comunes
  code/01_construccion/               datos brutos -> data/processed/
  code/02_estimacion/                 data/processed/ -> outputs/
  data/raw/                           datos brutos, sin modificar (no incluidos)
  data/processed/                     bases construidas (se generan)
  outputs/                            tablas (.tex) y gráficos (.pdf), con su número en la tesis
```

Las estimaciones solo leen las bases de `data/processed/`, eligen la muestra y estiman; toda
variable de estación o de mercado se construye en `01_construccion/`.

## Datos brutos

Van en `replication/data/raw/` con estos nombres:

| Archivo | Fuente |
|---|---|
| `bencina_en_linea/2012.csv` … `2026.csv` | Precios históricos por estación, bencinaenlinea.cl (CNE) |
| `mepco.xlsx` | Precio mayorista semanal con y sin MEPCO, Ministerio de Hacienda |
| `estructura_precios_combustibles.xlsx` | Estructura del precio a público, CNE |
| `mapas/red_vial/` | Red vial 2019, BCN (clase 1 del MOP: autopistas y rutas nacionales) |
| `mapas/comunas/` | División comunal, BCN |

## Construcción (`code/01_construccion/`)

| Script | Hace | Produce (`data/processed/`) |
|---|---|---|
| `01_precios.R` | Une y limpia los dos regímenes de reporte de la CNE (antes y desde 2023) | `precios.csv` |
| `02_costos.R` | Costo mayorista con MEPCO y estructura del precio | `costo_mepco.csv`, `estructura_precios.csv` |
| `03_paneles.R` | Identidad de estaciones, paneles semanal y mensual, entradas y tratamiento por ventana | `identidad_estaciones.csv`, `panel_<ventana>/` |
| `04_cambios_precio.R` | Cambios de precio diarios según el calendario MEPCO | `cambios_precio.csv.gz` |
| `05_traspaso.R` | Cambio de precio de tratadas y controles en cada actualización del costo | `traspaso_actualizaciones.csv` |
| `06_comovimiento.R` | Respuesta de las rivales a cada cambio de precio, con hora | `comovimiento_pares.csv.gz` |

Ventanas: `2012_2026` (principal), `2014_2019` y `2021_2026`. Cada script explica sus reglas
en su encabezado.

## Estimación (`code/02_estimacion/`)

| Script | Produce (`outputs/`) |
|---|---|
| `01_descriptivos.R` | Gráficos 1, 2, B.1 y B.2; Tabla 1 |
| `02_muestra.R` | Gráfico 4; Tablas 3 y C.1 |
| `03_es_principal.R` | Gráficos 5 y B.20; Tablas 4, 5 (número principal y % del margen), C.3 a C.5 y C.15 |
| `04_atenuacion.R` | Gráficos 6 y B.21; Tablas 6 (ATT por radio) y C.11 (equivalencia) |
| `05_comovimiento.R` | Gráficos 7, B.9 y B.11 |
| `06_distribucion.R` | Gráfico 8 |
| `07_ventanas.R` | Gráficos 11 y 12; Tablas C.9 y C.10 |
| `08_traspaso.R` | Gráfico B.4 |
| `09_placebos.R` | Gráficos 14 y B.5; Tabla C.8 |
| `10_persistencia.R` | Gráfico B.3 |
| `11_autopista.R` | Gráfico B.6; Tabla C.6 |
| `12_errores_conley.R` | Gráfico B.7 |
| `13_panel_semanal.R` | Gráfico B.8 |
| `14_honest_did.R` | Tabla C.7 |
| `15_validacion_margen.R` | Tabla C.2 |
| `16_mecanismo.R` | Tabla C.13 |
| `17_tipo_entrante.R` | Tablas 7 y C.16; Gráficos 9 y B.12 a B.17 |
| `18_robustez_tipo.R` | Tablas 8, 9 y C.14; Gráfico B.19 |
| `20_alcance_independientes.R` | Gráficos B.21 y B.22 |
| `19_imputacion_tipo.R` | Gráfico B.18 (imputación con bootstrap, unos 10 minutos; `run_all.R` no lo corre) |

El estimador de los resultados del cuerpo es Sun y Abraham (2021) con cohortes anuales; TWFE se reporta
en el anexo como comparación, salvo en el Gráfico 14 y la Tabla C.8, que usan TWFE (ver sus encabezados).

El Gráfico 3 (esquema de la base) y la Tabla 2 (descripción de las variables) están escritos
directamente en LaTeX y no tienen código.

## Licencia

El código está bajo licencia MIT (`LICENSE`). Los datos pertenecen a sus fuentes.
