# Análisis de línea base de arrecifes con BRUVs y AGRRA en R

**Material de capacitación** · César Augusto Egas Vaca · Biólogo Marino, MSc en Ciencia y Sistemas de Información Geográfica · [ORCID 0009-0004-0025-5799](https://orcid.org/0009-0004-0025-5799)

Flujo de trabajo reproducible en R para analizar los datos de una línea base arrecifal del Caribe: videos remotos con cebo (**BRUVs**) y evaluaciones con el protocolo **AGRRA** (bentos, corales y peces), hasta un **Índice de Salud Arrecifal (ISA)** por sitio y un mapa listo para QGIS.

> ⚠️ **Los datos son simulados.** Sitios, coordenadas, conteos y tallas se generaron con `R/01_simular_datos.R` con fines didácticos y no representan ningún arrecife real. Los parámetros talla-peso y los umbrales del ISA también son de ejemplo (ver [Advertencias](#advertencias)).

**👉 Vista rápida:** abre [`analisis_bruvs_agrra.ipynb`](analisis_bruvs_agrra.ipynb) aquí mismo en GitHub para ver el análisis completo con tablas y gráficos, sin instalar nada.

## Qué se practica

| Paso | Script | Métodos |
|---|---|---|
| Control de calidad | `02_control_calidad.R` | Validación de nombres científicos, duplicados, rangos |
| BRUVs: métricas | `03_bruvs_maxn.R` | MaxN, completar ceros, riqueza, frecuencia de ocurrencia |
| BRUVs: modelos | `04_bruvs_modelos.R` | GLMM Poisson vs. binomial negativa (glmmTMB), emmeans, diagnóstico con DHARMa |
| BRUVs: ensamblaje | `05_bruvs_multivariado.R` | nMDS Bray-Curtis, PERMANOVA, SIMPER, curvas de acumulación (vegan) |
| AGRRA: bentos | `06_agrra_bentos.R` | Cobertura por punto-intercepto |
| AGRRA: corales | `07_agrra_corales.R` | Mortalidad, blanqueamiento, estructura de tallas |
| AGRRA: peces | `08_agrra_peces_biomasa.R` | Densidad y biomasa (g/100 m²) con relaciones talla-peso |
| Índice integrado | `09_indice_salud_arrecifal.R` | Índice de Salud Arrecifal (4 indicadores, puntaje 1–5) |
| Mapa | `10_mapa_sitios.R` | Mapa de sitios y exportación a QGIS (EPSG:4326) |

**Pregunta de práctica:** ¿hay señales de presión pesquera al comparar 6 sitios de uso pesquero con 6 de una zona de protección? Los resultados esperados están en [`resultados_esperados/`](resultados_esperados/).

## Cómo ejecutarlo

### Opción A — Anaconda Navigator
1. Descarga este repositorio (**Code → Download ZIP**) y descomprímelo.
2. En Anaconda Navigator: **Environments → Import**, elige `environment.yml` y crea el entorno `arrecifes-r`.
3. En **Home**, selecciona el entorno `arrecifes-r` y abre **JupyterLab**.
4. Abre `analisis_bruvs_agrra.ipynb`, elige el kernel **R** y ejecuta las celdas en orden.

Si el kernel R no aparece: abre **Anaconda Prompt** y ejecuta `conda activate arrecifes-r` y luego `R -e "IRkernel::installspec()"`.

### Opción B — Línea de comandos (conda)
```bash
conda env create -f environment.yml
conda activate arrecifes-r
Rscript R/ejecutar_todo.R          # o: jupyter lab
```

### Opción C — RStudio
Abre `arrecifes-bruvs-agrra-r.Rproj`, ejecuta `R/00_paquetes.R` una vez y luego los scripts del `02` al `10`, línea por línea.

> El entorno usa solo el canal **conda-forge**: el canal R de Anaconda dejó de mantenerse en noviembre de 2025, mientras que conda-forge mantiene los paquetes de R de forma independiente.

## Estructura
```
├── analisis_bruvs_agrra.ipynb   # cuaderno Jupyter (kernel R) con todo el análisis
├── environment.yml              # entorno conda (conda-forge)
├── R/                           # scripts 00–10 y ejecutar_todo.R
├── datos/                       # datos simulados, lista de especies, parámetros y umbrales de ejemplo
└── resultados_esperados/        # tablas y figuras de referencia
```

## Advertencias
- **Datos simulados:** no citarlos como resultados de ningún sitio real.
- **Parámetros talla-peso (a, b):** ilustrativos; reemplazarlos con los de [FishBase](https://www.fishbase.se) para cada especie y tipo de longitud.
- **Umbrales del ISA:** de ejemplo; usar la tabla oficial del Report Card vigente de la [Iniciativa Arrecifes Saludables](https://www.healthyreefs.org).
- **Diseño de muestreo:** confirmar número y largo de transectos, área de las bandas de peces y tiempo de los BRUVs con el protocolo de campo real.
- Probado el 18/11/2026 con R 4.5.3, glmmTMB 1.1.14, vegan 2.7-5, emmeans 2.0.4 y DHARMa 0.5.1 (conda-forge). glmmTMB puede advertir de una diferencia de versión con TMB; no afecta a los resultados del ejercicio.

## Referencias
- Langlois, T. et al. (2020). A field and video annotation guide for baited remote underwater stereo-video surveys of demersal fish assemblages. *Methods in Ecology and Evolution*, 11(11), 1401–1409. https://doi.org/10.1111/2041-210X.13470
- Iniciativa Arrecifes Saludables (Healthy Reefs Initiative): https://www.healthyreefs.org
- Protocolo AGRRA (Atlantic and Gulf Rapid Reef Assessment): https://www.agrra.org

## English summary
Training material: a reproducible R workflow for coral-reef baseline data — baited remote underwater video (BRUVs: MaxN, count GLMMs, multivariate assemblage analysis) and AGRRA surveys (benthic cover, coral condition, fish biomass), integrated into a Reef Health Index and a site map. **All data are simulated** for teaching purposes. Open `analisis_bruvs_agrra.ipynb` for a rendered walkthrough, or create the conda environment with `conda env create -f environment.yml`.

## Licencia
Código bajo licencia [MIT](LICENSE).
