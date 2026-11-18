# =============================================================================
# 05_bruvs_multivariado.R — Composición del ensamblaje y esfuerzo de muestreo
# =============================================================================
library(dplyr); library(tidyr); library(readr); library(ggplot2); library(vegan)
set.seed(123)   # las permutaciones y el nMDS usan números aleatorios

maxn <- read_csv("datos_limpios/bruvs_maxn_con_ceros.csv", show_col_types = FALSE)

# ---- PASO 1. Matriz despliegues x especies ---------------------------------
ancho <- maxn |>
  select(despliegue_id, sitio_id, zona, habitat, especie, maxn) |>
  pivot_wider(names_from = especie, values_from = maxn)
meta <- select(ancho, despliegue_id, sitio_id, zona, habitat)
mat  <- as.matrix(select(ancho, -despliegue_id, -sitio_id, -zona, -habitat))
mat_t <- sqrt(mat)   # raíz cuadrada: reduce el peso de las especies muy abundantes

# ---- PASO 2. nMDS con distancia de Bray-Curtis ------------------------------
nmds <- metaMDS(mat_t, distance = "bray", k = 2, trymax = 100, trace = FALSE)
cat("Estrés del nMDS:", round(nmds$stress, 3), "(< 0.2 es aceptable)\n")
# Si el estrés es > 0.2, la representación en 2 dimensiones distorsiona las
# distancias: prueba con k = 3, o trabaja con promedios por sitio (Paso 3).
pts <- bind_cols(meta, as.data.frame(scores(nmds, display = "sites")))
g <- ggplot(pts, aes(NMDS1, NMDS2, colour = zona, shape = habitat)) +
  geom_point(size = 3) + stat_ellipse(aes(group = zona), linetype = 2) +
  labs(title = paste0("nMDS Bray-Curtis (estrés = ", round(nmds$stress, 2), ") - DATOS FICTICIOS"),
       colour = "Zona", shape = "Hábitat") +
  theme_bw()
ggsave("resultados/figuras/bruvs_nmds.png", g, width = 7, height = 5, dpi = 150)

# ---- PASO 3. PERMANOVA: ¿la composición difiere entre zonas? ---------------
# Los despliegues de un mismo sitio no son independientes (pseudorréplicas).
# La forma más simple y correcta es analizar a nivel de SITIO, promediando
# los despliegues de cada sitio.
sitio_mat <- maxn |>
  group_by(sitio_id, zona, habitat, especie) |>
  summarise(maxn = mean(maxn), .groups = "drop") |>
  pivot_wider(names_from = especie, values_from = maxn)
meta_s <- select(sitio_mat, sitio_id, zona, habitat)
mat_s  <- sqrt(as.matrix(select(sitio_mat, -sitio_id, -zona, -habitat)))
dist_s <- vegdist(mat_s, method = "bray")

perma <- adonis2(dist_s ~ zona + habitat, data = meta_s, permutations = 999, by = "margin")
print(perma)
write_csv(cbind(termino = rownames(perma), as.data.frame(perma)),
          "resultados/tablas/bruvs_permanova.csv")

# Supuesto: dispersión similar entre grupos (si p < 0.05, interpretar con cautela)
print(permutest(betadisper(dist_s, meta_s$zona), permutations = 999))

# ¿Qué especies explican la diferencia? (SIMPER)
sim <- summary(simper(mat_s, meta_s$zona, permutations = 99))
print(head(sim[[1]], 8))

# ---- PASO 4. Curvas de acumulación de especies (¿alcanzó el esfuerzo?) -----
curvas <- lapply(split(seq_len(nrow(mat)), meta$zona), function(i) {
  sa <- specaccum(mat[i, ], method = "random", permutations = 200)
  data.frame(despliegues = sa$sites, riqueza = sa$richness, sd = sa$sd)
})
curvas <- bind_rows(curvas, .id = "zona")
g2 <- ggplot(curvas, aes(despliegues, riqueza, colour = zona, fill = zona)) +
  geom_ribbon(aes(ymin = riqueza - sd, ymax = riqueza + sd), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1) +
  labs(x = "Número de despliegues", y = "Especies acumuladas",
       title = "Curvas de acumulación (DATOS FICTICIOS)") + theme_bw()
ggsave("resultados/figuras/bruvs_acumulacion_especies.png", g2, width = 6, height = 4, dpi = 150)
# Si la curva aún sube al final, faltan despliegues para describir el ensamblaje.
