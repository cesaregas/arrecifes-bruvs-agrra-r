# =============================================================================
# 00_paquetes.R — Instala (una sola vez) y carga los paquetes del proyecto
# =============================================================================
paquetes <- c(
  "dplyr", "tidyr", "readr", "stringr",   # manejo de datos (tidyverse)
  "ggplot2",                               # gráficos
  "glmmTMB",                               # modelos lineales generalizados mixtos
  "emmeans",                               # medias marginales y contrastes
  "vegan",                                 # ecología de comunidades (nMDS, PERMANOVA)
  "DHARMa"                                 # diagnóstico de residuos (recomendado)
)
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)

# Los scripts y datos usan tildes (Protección, Herbívoro). R debe trabajar en UTF-8:
if (!isTRUE(l10n_info()$`UTF-8`)) {
  warning("Tu sesión de R no está en UTF-8. En RStudio: File > Reopen with Encoding > UTF-8, ",
          "y actualiza R a la versión 4.2 o superior.")
}

# Carpetas de salida
for (d in c("datos_limpios", "resultados/tablas", "resultados/figuras")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
message("Listo. Paquetes instalados: ", paste(paquetes[paquetes %in% rownames(installed.packages())], collapse = ", "))
