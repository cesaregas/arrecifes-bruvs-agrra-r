# =============================================================================
# 08_agrra_peces_biomasa.R — Densidad y biomasa de peces (transectos de banda)
# En este ejemplo: 6 transectos de 30 x 2 m (60 m2) por sitio.
# Biomasa de cada registro: W = a * L^b (g) x número de individuos, donde L es
# la marca de clase (punto medio) de la talla anotada.
# Resultado en g/100 m2, la unidad que usa la Iniciativa Arrecifes Saludables.
# =============================================================================
library(dplyr); library(tidyr); library(readr); library(ggplot2)

AREA_M2 <- 60   # 30 m x 2 m

sitios <- read_csv("datos_limpios/sitios.csv", show_col_types = FALSE)
peces  <- read_csv("datos_limpios/agrra_peces.csv", show_col_types = FALSE)
lw     <- read_csv("datos/parametros_talla_peso_ILUSTRATIVOS.csv", show_col_types = FALSE)
# ATENCIÓN: a y b son ilustrativos. En un análisis real, tomarlos de FishBase.

# Marca de clase de cada intervalo de talla (supuesto: >40 cm se toma como 45 cm)
marca <- c("0-5" = 2.5, "6-10" = 8, "11-20" = 15.5, "21-30" = 25.5, "31-40" = 35.5, ">40" = 45)

# ---- PASO 1. Biomasa por registro --------------------------------------------
pz <- peces |>
  left_join(lw, by = "especie") |>
  mutate(L_cm = marca[clase_talla_cm],
         biomasa_g = conteo * a * L_cm^b)
stopifnot(!any(is.na(pz$a)))   # toda especie debe tener parámetros talla-peso

# ---- PASO 2. Por transecto y grupo (completar ceros) ------------------------
por_tr <- pz |>
  group_by(sitio_id, transecto, grupo_isa) |>
  summarise(densidad_ind_100m2 = sum(conteo) / AREA_M2 * 100,
            biomasa_g_100m2    = sum(biomasa_g) / AREA_M2 * 100, .groups = "drop") |>
  complete(nesting(sitio_id, transecto), grupo_isa,
           fill = list(densidad_ind_100m2 = 0, biomasa_g_100m2 = 0))

# ---- PASO 3. Promedio por sitio ---------------------------------------------
por_sitio <- por_tr |>
  group_by(sitio_id, grupo_isa) |>
  summarise(densidad_media = mean(densidad_ind_100m2),
            biomasa_media  = mean(biomasa_g_100m2),
            biomasa_ee     = sd(biomasa_g_100m2) / sqrt(n()), .groups = "drop") |>
  left_join(select(sitios, sitio_id, zona), by = "sitio_id")
write_csv(por_sitio, "resultados/tablas/agrra_peces_biomasa_sitio.csv")
print(por_sitio |> select(sitio_id, zona, grupo_isa, biomasa_media) |>
        pivot_wider(names_from = grupo_isa, values_from = biomasa_media) |>
        mutate(across(where(is.numeric), round)))

# ---- PASO 4. Gráfico ----------------------------------------------------------
g <- por_sitio |>
  filter(grupo_isa != "Otro") |>
  ggplot(aes(sitio_id, biomasa_media, fill = zona)) +
  geom_col() +
  geom_errorbar(aes(ymin = biomasa_media - biomasa_ee, ymax = biomasa_media + biomasa_ee), width = 0.3) +
  facet_wrap(~ grupo_isa, scales = "free_y", ncol = 1) +
  labs(x = "Sitio", y = "Biomasa (g / 100 m2, ± EE)", fill = "Zona",
       title = "Biomasa de peces herbívoros y comerciales (DATOS FICTICIOS)") +
  theme_bw()
ggsave("resultados/figuras/agrra_peces_biomasa.png", g, width = 8, height = 6, dpi = 150)

# ---- PASO 5. Estructura de tallas de especies comerciales -------------------
g2 <- pz |>
  filter(grupo_isa == "Comercial") |>
  left_join(select(sitios, sitio_id, zona), by = "sitio_id") |>
  group_by(zona, clase_talla_cm) |> summarise(n = sum(conteo), .groups = "drop") |>
  mutate(clase_talla_cm = factor(clase_talla_cm, levels = names(marca))) |>
  ggplot(aes(clase_talla_cm, n, fill = zona)) +
  geom_col(position = "dodge") +
  labs(x = "Clase de talla (cm)", y = "Individuos", fill = "Zona",
       title = "Tallas de pargos y meros por zona (DATOS FICTICIOS)") + theme_bw()
ggsave("resultados/figuras/agrra_peces_tallas_comerciales.png", g2, width = 7, height = 4, dpi = 150)
# Pregunta: ¿hay menos individuos grandes donde se pesca? Es una señal típica
# de presión pesquera.
