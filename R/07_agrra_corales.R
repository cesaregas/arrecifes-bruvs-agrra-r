# =============================================================================
# 07_agrra_corales.R — Condición de las colonias de coral
# Indicadores por sitio: densidad de colonias, tamaño, mortalidad antigua y
# reciente, prevalencia de blanqueamiento y de enfermedades.
# Códigos de blanqueamiento en este ejemplo: NO = sin blanqueamiento,
# P = pálido, PB = parcialmente blanqueado, BL = blanqueado.
# =============================================================================
library(dplyr); library(tidyr); library(readr); library(ggplot2)

sitios  <- read_csv("datos_limpios/sitios.csv", show_col_types = FALSE)
corales <- read_csv("datos_limpios/agrra_corales.csv", show_col_types = FALSE)

# ---- PASO 1. Indicadores por sitio ------------------------------------------
ind <- corales |>
  group_by(sitio_id) |>
  summarise(colonias            = n(),
            colonias_por_10m    = n() / n_distinct(transecto),
            diam_medio_cm       = mean(diam_max_cm),
            mort_antigua_media  = mean(mort_antigua_pct, na.rm = TRUE),   # na.rm: el dato corregido a NA
            mort_reciente_media = mean(mort_reciente_pct),
            blanqueamiento_pct  = 100 * mean(blanqueamiento != "NO"),
            enfermedad_pct      = 100 * mean(enfermedad == "si"),
            .groups = "drop") |>
  left_join(select(sitios, sitio_id, zona), by = "sitio_id") |>
  mutate(across(where(is.numeric), ~ round(.x, 1)))
print(ind)
write_csv(ind, "resultados/tablas/agrra_corales_indicadores_sitio.csv")

# ---- PASO 2. Composición de especies de coral -------------------------------
comp <- corales |> count(especie, sort = TRUE) |> mutate(pct = round(100 * n / sum(n), 1))
print(comp)
write_csv(comp, "resultados/tablas/agrra_corales_especies.csv")

# ---- PASO 3. Gráfico de mortalidad por zona ---------------------------------
g <- corales |>
  left_join(select(sitios, sitio_id, zona), by = "sitio_id") |>
  pivot_longer(c(mort_antigua_pct, mort_reciente_pct), names_to = "tipo", values_to = "mortalidad") |>
  mutate(tipo = recode(tipo, mort_antigua_pct = "Mortalidad antigua", mort_reciente_pct = "Mortalidad reciente")) |>
  ggplot(aes(zona, mortalidad, fill = zona)) +
  geom_boxplot(alpha = 0.6, na.rm = TRUE) +
  facet_wrap(~ tipo, scales = "free_y") +
  labs(x = NULL, y = "% de la superficie de la colonia",
       title = "Mortalidad de colonias de coral (DATOS FICTICIOS)") +
  theme_bw() + theme(legend.position = "none")
ggsave("resultados/figuras/agrra_corales_mortalidad.png", g, width = 8, height = 4, dpi = 150)

# ---- PASO 4. Estructura de tallas -----------------------------------------
g2 <- ggplot(corales, aes(diam_max_cm)) +
  geom_histogram(binwidth = 10, boundary = 0, fill = "steelblue", colour = "white") +
  labs(x = "Diámetro máximo (cm)", y = "Número de colonias",
       title = "Estructura de tallas de corales (DATOS FICTICIOS)") + theme_bw()
ggsave("resultados/figuras/agrra_corales_tallas.png", g2, width = 6, height = 4, dpi = 150)
