# =============================================================================
# 04_bruvs_modelos.R — ¿Hay diferencias entre zonas? Modelos para conteos
# Pregunta de manejo: ¿hay menos tiburones y rayas donde se pesca más?
# - Respuesta: MaxN de elasmobranquios por despliegue (conteo, muchos ceros)
# - Efectos fijos: zona y profundidad
# - Efecto aleatorio: sitio (los 3 despliegues de un sitio no son independientes)
# =============================================================================
library(dplyr); library(readr); library(glmmTMB); library(emmeans); library(ggplot2)

d <- read_csv("resultados/tablas/bruvs_metricas_por_despliegue.csv", show_col_types = FALSE) |>
  mutate(zona = factor(zona),   # niveles en orden alfabético: Protección (referencia), Uso pesquero
         prof_c = profundidad_m - mean(profundidad_m))   # profundidad centrada

# ---- PASO 1. Poisson vs. binomial negativa ---------------------------------
m_pois <- glmmTMB(maxn_elasmo ~ zona + prof_c + (1 | sitio_id), family = poisson, data = d)
m_nb   <- glmmTMB(maxn_elasmo ~ zona + prof_c + (1 | sitio_id), family = nbinom2, data = d)
print(AIC(m_pois, m_nb))   # el menor AIC indica el modelo con mejor ajuste

# Sobredispersión del Poisson: si el cociente es >> 1, preferir binomial negativa
sobredisp <- sum(residuals(m_pois, type = "pearson")^2) / df.residual(m_pois)
cat("Cociente de sobredispersión (Poisson):", round(sobredisp, 2), "\n")

m <- if (AIC(m_nb) < AIC(m_pois)) m_nb else m_pois
summary(m)

# ---- PASO 2. Diagnóstico de residuos (paquete DHARMa, si está instalado) ----
if (requireNamespace("DHARMa", quietly = TRUE)) {
  res <- DHARMa::simulateResiduals(m)
  png("resultados/figuras/bruvs_modelo_residuos.png", width = 1000, height = 500)
  plot(res); dev.off()
  print(DHARMa::testDispersion(res))
  print(DHARMa::testZeroInflation(res))
} else {
  message("Instala DHARMa para revisar residuos: install.packages('DHARMa')")
}

# ---- PASO 3. Interpretar en la escala original ------------------------------
em <- emmeans(m, ~ zona, type = "response")
print(em)                       # MaxN esperado por zona (a profundidad media)
contraste <- contrast(emmeans(m, ~ zona), method = "revpairwise", type = "response")
print(contraste)                # razón Uso pesquero / Protección
# Ej.: un 'ratio' de 0.45 significa que en la zona de uso pesquero se espera
# un 55 % menos de MaxN de elasmobranquios que en la zona de protección.

coefs <- as.data.frame(summary(m)$coefficients$cond)
coefs$termino <- rownames(coefs)
write_csv(coefs, "resultados/tablas/bruvs_modelo_elasmobranquios_coeficientes.csv")
write_csv(as.data.frame(em), "resultados/tablas/bruvs_modelo_elasmobranquios_medias.csv")

g <- ggplot(as.data.frame(em), aes(zona, response)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.15) +
  labs(x = NULL, y = "MaxN esperado de tiburones y rayas (IC 95 %)",
       title = "Modelo GLMM binomial negativo (DATOS FICTICIOS)") +
  theme_bw()
ggsave("resultados/figuras/bruvs_modelo_elasmobranquios.png", g, width = 5, height = 4, dpi = 150)

# ---- EJERCICIO ------------------------------------------------------------
# Repite el modelo con 'maxn_comercial' como respuesta y compara la razón
# entre zonas con la de los elasmobranquios. ¿Qué grupo responde más a la pesca?
