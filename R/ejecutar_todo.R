# =============================================================================
# ejecutar_todo.R — Corre el flujo completo de una sola vez.
# Para aprender, es mejor abrir cada script y ejecutarlo línea por línea.
# =============================================================================
scripts <- c("00_paquetes.R", "02_control_calidad.R", "03_bruvs_maxn.R",
             "04_bruvs_modelos.R", "05_bruvs_multivariado.R", "06_agrra_bentos.R",
             "07_agrra_corales.R", "08_agrra_peces_biomasa.R",
             "09_indice_salud_arrecifal.R", "10_mapa_sitios.R")
for (s in scripts) {
  message("\n==== ", s, " ====")
  source(file.path("R", s), encoding = "UTF-8", echo = FALSE)
}
message("\nFlujo completo. Revisa resultados/tablas y resultados/figuras.")
