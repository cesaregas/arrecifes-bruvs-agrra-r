# =============================================================================
# 09_indice_salud_arrecifal.R — Índice de Salud Arrecifal (ISA) por sitio
# Integra 4 indicadores: coral vivo, macroalgas carnosas, biomasa de peces
# herbívoros y de peces comerciales. Cada uno recibe un puntaje de 1 (crítico)
# a 5 (muy bueno) y el ISA es el promedio.
# ¡Los umbrales del archivo son de EJEMPLO! Reemplazarlos con la tabla oficial
# del Report Card vigente de la Iniciativa Arrecifes Saludables (HRI).
# =============================================================================
library(dplyr); library(tidyr); library(readr); library(ggplot2)

sitios   <- read_csv("datos_limpios/sitios.csv", show_col_types = FALSE)
bentos   <- read_csv("resultados/tablas/agrra_cobertura_bentonica_sitio.csv", show_col_types = FALSE)
peces    <- read_csv("resultados/tablas/agrra_peces_biomasa_sitio.csv", show_col_types = FALSE)
umbrales <- read_csv("datos/umbrales_ISA_EJEMPLO.csv", show_col_types = FALSE)

# ---- PASO 1. Los 4 indicadores en una tabla larga ----------------------------
ind <- bind_rows(
  bentos |> filter(categoria == "CV")  |> transmute(sitio_id, indicador = "coral_vivo_pct", valor = cobertura_media),
  bentos |> filter(categoria == "MAC") |> transmute(sitio_id, indicador = "macroalgas_pct", valor = cobertura_media),
  peces  |> filter(grupo_isa == "Herbívoro") |> transmute(sitio_id, indicador = "herbivoros_g100m2", valor = biomasa_media),
  peces  |> filter(grupo_isa == "Comercial") |> transmute(sitio_id, indicador = "comerciales_g100m2", valor = biomasa_media)
)

# ---- PASO 2. Asignar puntaje según el intervalo [min, max) ------------------
puntuado <- ind |>
  inner_join(umbrales, by = "indicador", relationship = "many-to-many") |>
  filter(valor >= min, valor < max) |>
  select(sitio_id, indicador, valor, puntaje, categoria)
stopifnot(nrow(puntuado) == nrow(ind))   # cada valor cae en un solo intervalo

# ---- PASO 3. ISA = promedio de los 4 puntajes --------------------------------
cat_isa <- function(x) cut(x, c(0, 1.8, 2.6, 3.4, 4.2, 5.01),
                           labels = c("Crítico", "Pobre", "Regular", "Bueno", "Muy bueno"), right = FALSE)
isa <- puntuado |>
  group_by(sitio_id) |>
  summarise(ISA = mean(puntaje), .groups = "drop") |>
  mutate(categoria_ISA = cat_isa(ISA)) |>
  left_join(select(sitios, sitio_id, zona, lat, lon), by = "sitio_id")
# Nota: los cortes de la categoría del ISA (1.8, 2.6, 3.4, 4.2) también son de
# ejemplo; verificar con la metodología vigente de HRI.

tabla <- puntuado |>
  select(sitio_id, indicador, puntaje) |>
  pivot_wider(names_from = indicador, values_from = puntaje) |>
  left_join(isa, by = "sitio_id") |>
  arrange(desc(ISA))
print(tabla)
write_csv(tabla, "resultados/tablas/indice_salud_arrecifal_sitios.csv")

colores <- c("Crítico" = "#d7191c", "Pobre" = "#fdae61", "Regular" = "#ffffbf",
             "Bueno" = "#a6d96a", "Muy bueno" = "#1a9641")
g <- ggplot(isa, aes(reorder(sitio_id, ISA), ISA, fill = categoria_ISA)) +
  geom_col(colour = "grey30") +
  facet_grid(~ zona, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = colores, drop = FALSE) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(x = "Sitio", y = "ISA (1-5)", fill = "Condición",
       title = "Índice de Salud Arrecifal por sitio (DATOS y UMBRALES DE EJEMPLO)") +
  theme_bw()
ggsave("resultados/figuras/indice_salud_arrecifal.png", g, width = 8, height = 4, dpi = 150)
