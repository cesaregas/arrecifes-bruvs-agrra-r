# =============================================================================
# 03_bruvs_maxn.R — Métricas básicas de BRUVs a partir de MaxN
# MaxN = número máximo de individuos de una especie visibles en un mismo
# cuadro del video. Es un índice de abundancia RELATIVA y conservador
# (evita contar dos veces al mismo pez que entra y sale del cuadro).
# =============================================================================
library(dplyr); library(tidyr); library(readr); library(ggplot2)

sitios      <- read_csv("datos_limpios/sitios.csv", show_col_types = FALSE)
despliegues <- read_csv("datos_limpios/bruvs_despliegues.csv", show_col_types = FALSE)
bruvs       <- read_csv("datos_limpios/bruvs_maxn.csv", show_col_types = FALSE)
referencia  <- read_csv("datos_limpios/lista_referencia_bruvs.csv", show_col_types = FALSE)

# ---- PASO 1. Completar los ceros -------------------------------------------
# La planilla solo trae lo que se vio. Para promediar o modelar necesitamos
# también los despliegues donde la especie NO apareció (MaxN = 0).
maxn <- despliegues |>
  select(despliegue_id, sitio_id, profundidad_m) |>
  crossing(select(referencia, especie, grupo)) |>
  left_join(select(bruvs, despliegue_id, especie, maxn), by = c("despliegue_id", "especie")) |>
  mutate(maxn = replace_na(maxn, 0)) |>
  left_join(select(sitios, sitio_id, zona, habitat), by = "sitio_id")

cat("Filas: ", nrow(maxn), " = ", n_distinct(maxn$despliegue_id), " despliegues x ",
    n_distinct(maxn$especie), " especies\n")
write_csv(maxn, "datos_limpios/bruvs_maxn_con_ceros.csv")

# ---- PASO 2. Métricas por despliegue ----------------------------------------
por_despliegue <- maxn |>
  group_by(despliegue_id, sitio_id, zona, habitat, profundidad_m) |>
  summarise(riqueza          = sum(maxn > 0),
            maxn_total       = sum(maxn),
            maxn_elasmo      = sum(maxn[grupo == "Elasmobranquio"]),
            maxn_comercial   = sum(maxn[grupo == "Comercial"]),
            .groups = "drop")
write_csv(por_despliegue, "resultados/tablas/bruvs_metricas_por_despliegue.csv")

# ---- PASO 3. Tabla por especie y zona --------------------------------------
# Frecuencia de ocurrencia (FO) = % de despliegues donde se vio la especie
tabla_sp <- maxn |>
  group_by(grupo, especie, zona) |>
  summarise(n_despliegues = n(),
            FO_pct        = round(100 * mean(maxn > 0), 1),
            maxn_media    = round(mean(maxn), 2),
            maxn_ee       = round(sd(maxn) / sqrt(n()), 2),   # error estándar
            .groups = "drop") |>
  arrange(grupo, especie, zona)
print(tabla_sp, n = 40)
write_csv(tabla_sp, "resultados/tablas/bruvs_tabla_especies.csv")

# ---- PASO 4. Gráficos -------------------------------------------------------
g1 <- ggplot(tabla_sp, aes(x = maxn_media, y = reorder(especie, maxn_media), fill = zona)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  geom_errorbar(aes(xmin = maxn_media - maxn_ee, xmax = maxn_media + maxn_ee),
                position = position_dodge(0.8), width = 0.3) +
  facet_grid(grupo ~ ., scales = "free_y", space = "free_y") +
  labs(x = "MaxN medio por despliegue (± EE)", y = NULL, fill = "Zona",
       title = "BRUVs: abundancia relativa por especie (DATOS FICTICIOS)") +
  theme_bw() + theme(axis.text.y = element_text(face = "italic"), legend.position = "bottom")
ggsave("resultados/figuras/bruvs_maxn_especies.png", g1, width = 8, height = 8, dpi = 150)

g2 <- por_despliegue |>
  pivot_longer(c(riqueza, maxn_elasmo, maxn_comercial), names_to = "metrica", values_to = "valor") |>
  mutate(metrica = recode(metrica, riqueza = "Riqueza de especies",
                          maxn_elasmo = "MaxN tiburones y rayas", maxn_comercial = "MaxN especies comerciales")) |>
  ggplot(aes(zona, valor, fill = zona)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 1.5) +
  facet_wrap(~ metrica, scales = "free_y") +
  labs(x = NULL, y = "Valor por despliegue", title = "BRUVs por zona (DATOS FICTICIOS)") +
  theme_bw() + theme(legend.position = "none")
ggsave("resultados/figuras/bruvs_metricas_por_zona.png", g2, width = 9, height = 4, dpi = 150)

message("Listo: tablas y figuras de BRUVs en resultados/")
