# Plan de revisión: informe de Paola Bordón (2 nov 2026)

Contrastado con `tex/sections/*.tex`, `tesis.pdf` (idéntico a `tex/main.pdf`), `replication/outputs/`
(idénticos a `tex/output/`), las bases de `replication/data/processed/` y diagnósticos de solo lectura
corridos el 5 nov 2026. Comentario mayor = **M#**, menor = **m#**.

Orden: **primero las estimaciones que cambian números, después el texto que los cita.**
Reescribir el resumen antes de la Fase 1 obliga a reescribirlo dos veces.

---

## 0. Lo que encontré al verificar (y Paola no dice o dice distinto)

| # | Hallazgo | Evidencia |
|---|---|---|
| H1 | **La cita de Koh et al. (2022) está mal.** Su tratamiento es la entrada a ≤ 1 km y su *control* son las incumbentes con entrada a 1–2 km; no estiman efecto a 2 km ni "sin efecto más lejos". Su resumen dice que los precios caen "marginally". Lo que sí está bien: la caída "immediately before the entries" y que excluyen estaciones de autopista. | `literature/papers/.../koh_etal_2022.pdf`; tesis en `01_introduction.tex` y `02_literature.tex` |
| H2 | **56 de los 386 controles estrictos tienen una entrada de su misma marca a ≤ 5 km** (33 a ≤ 2 km). El código las ignora (`03_paneles.R`, `misma_marca` → `Inf`), así que quedan como control. Es la contaminación que Paola sospecha en M9. | diagnóstico |
| H3 | El "5,6 % / 5,2 % del margen" **no se calcula en ningún script**. Calza con ATT de la Tabla C.8 ÷ margen RM 2021 de la Fig. 1 (0,397/7 ≈ 5,7; 0,587/11 ≈ 5,3). No es reproducible. | `grep` en `code/` |
| H4 | El error de Sun–Abraham es de verdad menor: bin 0 de G93, SA 0,054 frente a TWFE 0,094, **mismas 124.427 obs. y mismo cluster por comuna**. No es un problema de agrupación distinta; hay que explicarlo. | diagnóstico |
| H5 | Clusters: **266 comunas** en G93. Test conjunto de pretendencias TWFE G93: **F = 0,58, p = 0,63**. | diagnóstico |
| H6 | Cadena de la muestra (G93): 2.021 estaciones → 1.538 incumbentes de 2012 → 732 tratadas / 223 excluidas (2–3 km) / 197 control amplio / 386 control estricto → **506 tratadas** con cohorte ≥ 2014 (226 tratadas solo por entradas de 2013 salen) → 501 / 377 con precio de G93 → **501 / 373** tras singletons. | diagnóstico |
| H7 | Los **386 controles del placebo** son todos los controles estrictos con cualquier combustible; 373 es G93 sin singletons (m8 resuelto, solo falta decirlo). | `09_placebos.R:20` |
| H8 | 267 de las 732 tratadas tienen una segunda entrada a ≤ 2 km; 14 de las 501 tratadas de G93 salen del panel ≤ 24 meses tras la entrada (sin segunda entrada). Hay que recalcular dentro de las 506 y dejarlo en `02_muestra.R`. | diagnóstico (preliminar) |
| H9 | Fuentes no incrustadas confirmadas: Helvetica y Symbol (vienen de los PDF de R). | inspección del PDF |
| H10 | **Notas al pie sin el "1": no lo reproduje.** En la página 5 las notas 1 y 2 se ven bien en el render de macOS. Preguntarle a Paola en qué visores lo vio. | render de la pág. 5 |
| H11 | Autopistas: el +0,56 % de G97 **sí se menciona** (sección 5.4) pero no se interpreta; el resumen dice "no aparece". | `05_results.tex` |
| H12 | Ventana 2014–2019: el texto dice "los bins previos son menores que los efectos posteriores en todos los combustibles", y es **falso en G97 con SA** (≤ −4 = 0,52 ***). | `05_results.tex`, Tabla C.9 |
| H13 | `tex/output/` se copia a mano desde `replication/outputs/`. Hoy están sincronizados; después de volver a correr hay que copiar de nuevo. | `cmp` |

---

## 1. Decisiones que tomas tú (o con Paola) antes de empezar

1. **Número principal.** Recomiendo el **ATT estático TWFE por combustible** de una tabla nueva del cuerpo (E1), con SA al lado. En el resumen: G93 y diésel, en % y en $/L. Desaparece el "0,24–0,59 %" sacado de la tabla de placebos.
2. **Mecanismo.** Recomiendo dejar de afirmar "menor traspaso" salvo que E3 muestre d < 0 en las alzas, y reencuadrarlo como **velocidad / sincronización del ajuste** (lo que Paola propone).
3. **Régimen de la medida sin entradas.** Recomiendo dejar de llamar "más limpio" al semanal y presentar ambos regímenes. Si el trisemanal da 3 km, el mensaje pasa a ser "ambos métodos coinciden en ~3 km", más simple que el "núcleo 2 / alcance 3".
4. **Munizaga (FEN).** Conversarlo con Paola antes de circular cualquier versión. No es código, pero condiciona cómo escribes la contribución.

---

## Fase 1. Estimaciones antes de la defensa (cambian números)

### E1. Tabla principal con un solo número (M5, m4 de la hoja de ruta)
- Qué: ATT estático y por semestre; G93, G95, G97 y diésel; TWFE y SA. Tres unidades: % del precio, $/L (ATT × precio medio de las tratadas los 12 meses previos) y % del margen con el **denominador explícito**: margen MEPCO medio de las tratadas los 12 meses previos (`m93`, `m97`, `mdi` del panel; G95 sin costo → "—").
- Dónde: `code/02_estimacion/03_es_principal.R` (los modelos ya existen) → nueva `outputs/tabla_05_att_principal.tex`, al cuerpo.
- Por qué con margen MEPCO: es la misma definición (precio − mayorista) de Fischer et al. En la nota se compara con el rango contable FNE de la Tabla C.2.
- Verificación: el ATT estático debe reproducir la fila "ATT real" de la Tabla C.8 (−0,397 / −0,404 / −0,243 / −0,587). El 5,6 % pasa a salir del código (H3).

### E2. Radios como definición de tratada, anillos a 12 meses y cotas (M4)
- **Radios 1, 2, 3 y 5 km.** En `01_construccion/03_paneles.R::asignar_tratamiento`, calcular `g_entry` para cada radio r. Tratada = primera entrada a ≤ r; excluidas = entrada en (r, 5]; control = el mismo `ctrl_b` en todos los radios, para que cambie solo la definición de tratada. Para r = 5 ya existe (`role5_entry`). Salida: tabla ATT × radio × combustible.
- **Horizonte de los anillos.** `04_atenuacion.R:16` usa `BIN_M` (6 meses); agregar la versión de 12 meses (`2L * BIN_M`) como segunda serie en la Fig. 6.
- **Nulos desde 3 km.** Reportar el IC del 90 % de los anillos 3–4 y 4–5 y un TOST con margen de equivalencia explícito (propuesta: la mitad del efecto a 0–2 km). Si no pasa el TOST, la frase "un radio de 5 km incluiría estaciones sin presión competitiva" se va del texto y de la conclusión.
- Verificación: el radio 2 km debe reproducir E1.

### E3. Mecanismo: separar los días (M1)
- `01_construccion/05_traspaso.R`: además de `dlp` (u−1 → u+6), guardar `dlp_01` (u−1 → u+1) y `dlp_26` (u+1 → u+6), cada uno neto del promedio de los controles de la región (como `dlp_cr`). Por construcción `dlp_01 + dlp_26 = dlp`: dejar un `stopifnot` con tolerancia.
- `02_estimacion/08_traspaso.R`:
  1. Fig. 11 con tres curvas: total, días 0–1 y días 2–6.
  2. Regresión Δp* = a + b·Post + c·Δc + d·Post×Δc, separada para alzas (`dc > 0`) y bajas (`dc < 0`), con cluster por estación. Lectura: b < 0 con d ≈ 0 es un cambio de nivel; d < 0 en las alzas es menor traspaso.
  3. Histograma de la brecha por estación en la semana +9.
- Consecuencia en el texto: según el resultado, reescribir la sección 4.5/5.6, el último párrafo de la introducción, el resumen y el primer párrafo de la conclusión (decisión 2).

### E4. Controles contaminados por entradas de la misma marca (M9, H2)
- `03_paneles.R`: agregar `misma_marca_5km` (entrada de la propia marca a ≤ 5 km) a la asignación.
- Volver a estimar E1 sin esos 56 controles. Columna de robustez en la Tabla C.5. Una frase en la sección 3.2 sobre dónde quedan las incumbentes con entrada de su misma marca.

### E5. Inferencia (M8)
- Número de comunas (clusters) en la nota o en una fila de **todas** las tablas de estudio de eventos: agregar `uniqueN(d$comuna[fixest::obs(m)])` a `n_estaciones()` (`00_configuracion.R`), igual que el diagnóstico que dio 266.
- Fila con el valor p del test conjunto de los bins previos (`wald(m, "::-[234]")`) en las Tablas 4, C.3, C.4, C.5, C.9 y C.10.
- **SE de SA (H4).** Diagnóstico en este orden: (a) `summary(ms, agg = FALSE)` y agregar a mano con los pesos de cohorte; (b) estimador de imputación de Borusyak et al. hecho con `fixest` (estimar los efectos fijos en las obs. no tratadas, predecir, promediar los residuos de las tratadas), sin agregar dependencias; (c) bootstrap por comuna del ATT SA (~200 réplicas). Si (b) o (c) dan errores del orden de TWFE, reportar el conservador y decirlo en una nota. Si confirman el error de SA, explicar por qué (saturar cohorte × bin absorbe la heterogeneidad que en TWFE queda en el residuo).

### E6. Medida sin entradas: lo mínimo para sostener la conclusión (M3)
- Inferencia diádica: `05_comovimiento.R:22`, `cluster = ~i` → `cluster = ~i + j`.
- Alzas frente a bajas: `06_comovimiento.R` debe guardar el signo del cambio líder (columna `dir`). Estimar por separado.
- Texto: presentar ambos regímenes en el cuerpo (decisión 3) y borrar el argumento de que el semanal es "más limpio".
- Para el paper, no ahora: todos los años con registro horario (los datos existen desde 2015-06), la pendiente de reacción y la validación con las entradas.

### E7. Conteos de la muestra (m6, m7, m8, m10)
- En `02_muestra.R`, imprimir y escribir a `outputs/tabla_C01` la cadena de H6, paso por paso, para tratadas y para controles.
- Agregar las tratadas con segunda entrada, las tratadas con una entrada previa a 2–5 km y las tratadas que salen del panel tras la entrada (H8), calculadas dentro de las 506.
- Nota de la Tabla C.8: "386 = todos los controles estrictos; en G93 quedan 373 por el combustible disponible y los singletons" (H7).

**Al cerrar la Fase 1:** `Rscript run_all.R` (~12 min) → copiar `replication/outputs/*` a `tex/output/{graficos,tablas}/` (H13) → compilar. Revisar con `git diff` que las tablas que no debían cambiar no cambiaron.

---

## Fase 2. Texto que depende de la Fase 1

| Tarea | Archivo | Qué |
|---|---|---|
| T1 Resumen (m1) | `main.tex` | "2014 a febrero de 2026"; reescribir la primera frase; número de E1; mecanismo según E3; autopistas: "no se reduce" y mencionar G97; "deja de distinguirse de cero" en vez de "se anula", con la cota de E2. |
| T2 Introducción | `01_introduction.tex` | Mismos números y frases que T1. Párrafo 5: mecanismo y "núcleo 2 km" según E3/E6. Corregir Koh (H1). |
| T3 Resultados 5.1 | `05_results.tex` | Citar la tabla E1; el % del margen con su denominador. |
| T4 Alcance 5.2 | `05_results.tex` | Radios (E2), anillos a 12 meses, TOST; regímenes de la medida sin entradas (E6); reescribir "Ambas estrategias delimitan un mercado escalonado" y el párrafo de la FNE según los resultados. |
| T5 Ventanas (M7) | `05_results.tex` | Corregir H12; mencionar los bins de G97 en 2021–26 (≥ 3 positivos); presentar el contraste como **descriptivo**, porque la muestra cambia entre ventanas. |
| T6 Autopistas | `05_results.tex` | Interpretar el +0,56 % de G97 y decir que se excluyen 77 tratadas con entrante en la red, y por qué. |
| T7 Mecanismo | `04_estrategy.tex` §4.5, `05_results.tex` §5.6 | Reescribir según E3; reencuadrar hacia velocidad / sincronización. |
| T8 Conclusión | `07_conclusion.tex` | Primer párrafo (pass-through), "dos diseños independientes…", radio de 5 km, implicancia MEPCO: todo según E1–E6. |
| T9 Limitaciones | `06_robustness.tex` | Composición urbana/rural de los controles (M2) como limitación explícita, con el diseño de anillos como trabajo futuro. |

---

## Fase 3. Cambios menores que no dependen de nada (se pueden hacer ya)

- **m3 Tabla 4:** quitar la columna 1 de `ESCALERA` (`03_es_principal.R:28`), también en C.3 y C.4; ajustar el último párrafo de §4.2 y la segunda frase de §5.1.
- **m4 Modelo §4.1:** reducirlo a un párrafo (ubicación endógena, momento exógeno por el plazo h) y quitar "las tres predicciones vienen del modelo".
- **m5 Supuestos 1 y 2:** fundirlos. Ojo: `sup:persistencia` y `sup:exogeneidad` se citan 12 veces en `tex/sections`; actualizar cada `\Cref`.
- **m6 Fecha del evento:** en la ec. (5), "t^e_i es el mes de la primera entrada competidora a ≤ 2 km" (así está en el código: `g_entry`). En la Tabla 2, separar "Distancia a la entrante" (anillos, primera a ≤ 5 km) de la fecha del tratamiento.
- **m9 Fig. 8:** en la nota, "cuantiles de la muestra conjunta de tratadas y controles (`06_distribucion.R:22`); intervalos puntuales con cluster por comuna, sin bandas uniformes".
- **m11 Extrapolación:** "del orden de cuatro pesos" → presentarlo como orden de magnitud, con su supuesto.
- **m12 Tabla 1:** una frase sobre el 37,7 % de los jueves en el régimen trisemanal (punto focal heredado).
- **m16 Declaración de IA:** sacarla del `\thanks` y ponerla en una sección sin número antes de las referencias.
- **m17 Referencias:**
  - `TAYLORMUEHLEGGER2026`: cambiarla a `@article`, JAERE 13(5), 1311–1336.
  - `KOH2022`: corregir el texto según H1, en la intro y en §2.
  - `HENRIQUEZ2025`: el 1,5 km no es una estimación independiente; reescribir la frase en la intro, §2 y §5.2.
  - Agregar solo las que se citen tras los cambios (`CALLAWAYSANTANNA2021`, `GENAKOS2022`, `LACHMORAGA2017` y `HASTINGS2004` ya están en el `.bib`).
- **m2 Duplicación intro / §2:** sacar de la introducción el párrafo de Fischer, Davis, Taylor y Koh y dejar una frase que remita a §2. Fusionar las secciones queda para el paper.
- **m15 Decimales con coma** (al final, cuando los números ya no cambien):
  - Texto: reemplazar `(\d)\.(\d)` → `\1,\2` en `tex/sections/*.tex` y `main.tex`, revisando el diff a mano (ojo con `\ref`, rutas y años).
  - Miles: 2,021 → 2.021.
  - Matemáticas: quitar `es-nodecimaldot` de `babel` o usar `{,}`.
  - Tablas de R: cambiar el formato de números donde `00_configuracion.R` escribe las tablas (líneas ~68–91), con `format(..., decimal.mark = ",", big.mark = ".")` o su equivalente en `etable`, para que salga bien desde el código y no a mano.
- **m13 Fuentes de las figuras:** `00_configuracion.R:117`, agregar `device = cairo_pdf` a `ggsave`; volver a correr; confirmar que ninguna fuente queda sin incrustar (script de `pypdf` o `pdffonts`) y que se ven los signos menos de las Figs. 11 y B.4.
- **m14 Notas al pie:** no reproducido (H10). Pedirle a Paola el visor; probar en Acrobat y en Chrome antes de tocar las fuentes.

---

## Queda para la versión paper (no antes de la defensa)

Anillos dentro de cada evento con FE evento × mes (M2.1) · Callaway–Sant'Anna y reponderación (M2.2–3) ·
distancia vial / isócronas y autopistas medidas por la ruta (M4) · medida sin entradas con todos los años,
pendiente de reacción y validación con entradas (M3) · distribución con efectos por cuantil, bandas uniformes
y por tercil/marca, y conciliación con agosto (M6) · una sola muestra con Post × cohorte post-2020 y
Rambachan–Roth por ventana (M7) · Rambachan–Roth mensual sin agrupar (M8) · identidad y precio de la entrante,
fechas externas SEC/municipales (M9) · G93 y diésel en el cuerpo, inglés.

Excepción posible: si se quiere cerrar la objeción M2 antes de la defensa, el diseño de anillos es lo que más
rinde por hora (datos y distancias ya están), pero no está en lo que Paola exige.

---

## Checklist final

- [ ] `run_all.R` sin errores; `logs/` revisados
- [ ] `tex/output/` = `replication/outputs/`
- [ ] Cada número del resumen, la introducción y la conclusión aparece en una tabla o figura y coincide
- [ ] `latexmk` sin referencias indefinidas; fuentes incrustadas
- [ ] README: actualizar la tabla de scripts → outputs si se agregan tablas o figuras
