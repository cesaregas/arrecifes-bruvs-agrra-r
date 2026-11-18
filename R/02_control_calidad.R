# =============================================================================
# 02_control_calidad.R — Depuración y validación (QA/QC)
# Regla de oro: nunca se corrige a mano el archivo original. Todas las
# correcciones quedan escritas aquí, para que cualquiera pueda repetirlas.
# =============================================================================
library(dplyr); library(tidyr); library(readr); library(stringr)
dir.create("datos_limpios", showWarnings = FALSE)

sitios      <- read_csv("datos/sitios.csv", show_col_types = FALSE)
despliegues <- read_csv("datos/bruvs_despliegues.csv", show_col_types = FALSE)
bruvs       <- read_csv("datos/bruvs_maxn.csv", show_col_types = FALSE)
referencia  <- read_csv("datos/lista_referencia_bruvs.csv", show_col_types = FALSE)
bentos      <- read_csv("datos/agrra_bentos.csv", show_col_types = FALSE)
corales     <- read_csv("datos/agrra_corales.csv", show_col_types = FALSE)
peces       <- read_csv("datos/agrra_peces.csv", show_col_types = FALSE)

# ---- PASO 1. Mirar la estructura antes de tocar nada ----------------------
glimpse(bruvs)
summary(bruvs$maxn)

# ---- PASO 2. Nombres científicos ------------------------------------------
# 2a. Espacios y mayúsculas: "Género especie"
bruvs <- bruvs |> mutate(especie = str_to_sentence(str_squish(especie)))

# 2b. ¿Qué nombres NO están en la lista de referencia?
no_validos <- bruvs |> distinct(especie) |> anti_join(referencia, by = "especie")
print(no_validos)   # Debe aparecer "Ginglymostoma cirratun" (error de tipeo)

# 2c. Tabla de correcciones (documentada)
correcciones <- c("Ginglymostoma cirratun" = "Ginglymostoma cirratum")
bruvs <- bruvs |> mutate(especie = recode(especie, !!!correcciones))
stopifnot(nrow(anti_join(distinct(bruvs, especie), referencia, by = "especie")) == 0)

# ---- PASO 3. Duplicados ---------------------------------------------------
cat("Filas duplicadas en BRUVs:", sum(duplicated(bruvs)), "\n")
bruvs <- distinct(bruvs)

# Una especie no puede tener dos MaxN en el mismo despliegue
dup_sp <- bruvs |> count(despliegue_id, especie) |> filter(n > 1)
print(dup_sp)   # Debe estar vacío; si no, revisar las planillas de anotación

# ---- PASO 4. Rangos y coherencia -------------------------------------------
stopifnot(all(bruvs$maxn > 0), all(bruvs$maxn == round(bruvs$maxn)))
stopifnot(all(bruvs$despliegue_id %in% despliegues$despliegue_id))

# Corales: mortalidades entre 0 y 100, y antigua + reciente <= 100
problemas_coral <- corales |>
  mutate(fila = row_number()) |>
  filter(mort_antigua_pct > 100 | mort_reciente_pct > 100 |
           mort_antigua_pct + mort_reciente_pct > 100)
print(problemas_coral)   # Debe aparecer la colonia con 130 % de mortalidad antigua
corales <- corales |>
  mutate(mort_antigua_pct = if_else(mort_antigua_pct > 100, NA_real_, mort_antigua_pct))

# Bentos: cada transecto debe tener exactamente 100 puntos
puntos <- bentos |> count(sitio_id, transecto)
stopifnot(all(puntos$n == 100))

# Peces: clases de talla válidas
clases <- c("0-5", "6-10", "11-20", "21-30", "31-40", ">40")
stopifnot(all(peces$clase_talla_cm %in% clases))

# Todos los sitios de los datos existen en la tabla de sitios
stopifnot(all(c(bruvs$sitio_id, bentos$sitio_id, corales$sitio_id, peces$sitio_id) %in% sitios$sitio_id))

# ---- PASO 5. Guardar datos limpios -----------------------------------------
write_csv(bruvs,   "datos_limpios/bruvs_maxn.csv")
write_csv(corales, "datos_limpios/agrra_corales.csv")
file.copy(c("datos/sitios.csv", "datos/bruvs_despliegues.csv", "datos/agrra_bentos.csv",
            "datos/agrra_peces.csv", "datos/lista_referencia_bruvs.csv"),
          "datos_limpios/", overwrite = TRUE)
message("Control de calidad terminado. Datos en datos_limpios/")
