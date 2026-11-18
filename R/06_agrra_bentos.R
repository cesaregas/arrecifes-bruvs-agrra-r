# =============================================================================
# 06_agrra_bentos.R — Cobertura bentónica (punto-intercepto)
# En este ejemplo: 5 transectos de 10 m por sitio, un punto cada 10 cm
# (100 puntos). Cobertura (%) = puntos de la categoría / puntos totales x 100.
# La unidad de réplica es el TRANSECTO; luego se resume por sitio.
# =============================================================================
library(dplyr); library(tidyr); library(readr); library(ggplot2)

sitios <- read_csv("datos_limpios/sitios.csv", show_col_types = FALSE)
bentos <- read_csv("datos_limpios/agrra_bentos.csv", show_col_types = FALSE)

nombres <- c(CV = "Coral vivo", MAC = "Macroalgas carnosas", ACC = "Algas calcáreas costrosas",
             TUR = "Césped algal", ESP = "Esponjas", ARE = "Arena / abiótico")

# ---- PASO 1. Cobertura por transecto (completar categorías ausentes = 0 %) --
cob_tr <- bentos |>
  count(sitio_id, transecto, categoria) |>
  complete(nesting(sitio_id, transecto), categoria, fill = list(n = 0)) |>
  group_by(sitio_id, transecto) |>
  mutate(cobertura_pct = 100 * n / sum(n)) |>
  ungroup()

# ---- PASO 2. Promedio por sitio (± error estándar entre transectos) --------
cob_sitio <- cob_tr |>
  group_by(sitio_id, categoria) |>
  summarise(cobertura_media = mean(cobertura_pct),
            ee = sd(cobertura_pct) / sqrt(n()), .groups = "drop") |>
  left_join(select(sitios, sitio_id, zona, habitat), by = "sitio_id")
write_csv(cob_sitio, "resultados/tablas/agrra_cobertura_bentonica_sitio.csv")

# Tabla ancha, cómoda para el informe
tabla <- cob_sitio |>
  mutate(categoria = nombres[categoria], cobertura_media = round(cobertura_media, 1)) |>
  select(sitio_id, zona, categoria, cobertura_media) |>
  pivot_wider(names_from = categoria, values_from = cobertura_media)
print(tabla)
write_csv(tabla, "resultados/tablas/agrra_cobertura_bentonica_tabla.csv")

# ---- PASO 3. Gráfico de barras apiladas --------------------------------------
g <- cob_sitio |>
  mutate(categoria = factor(nombres[categoria], levels = nombres)) |>
  ggplot(aes(sitio_id, cobertura_media, fill = categoria)) +
  geom_col() +
  facet_grid(~ zona, scales = "free_x", space = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Sitio", y = "Cobertura (%)", fill = NULL,
       title = "Cobertura bentónica por sitio (DATOS FICTICIOS)") +
  theme_bw() + theme(legend.position = "bottom")
ggsave("resultados/figuras/agrra_cobertura_bentonica.png", g, width = 9, height = 5, dpi = 150)

# ---- PASO 4. ¿Difiere el coral vivo entre zonas? ----------------------------
# Modelo simple con el sitio como unidad (n = 12). Para algo más fino:
# glmmTMB(cbind(n, 100 - n) ~ zona + (1|sitio_id), family = binomial) por transecto.
cv <- filter(cob_sitio, categoria == "CV")
print(t.test(cobertura_media ~ zona, data = cv))
