# =============================================================================
# 10_mapa_sitios.R — Mapa simple de sitios coloreados por el ISA
# Coordenadas FICTICIAS. Para un mapa de informe, exporta la tabla a QGIS o usa
# el paquete 'sf' con una capa de línea de costa y arrecifes.
# =============================================================================
library(dplyr); library(readr); library(ggplot2)

isa <- read_csv("resultados/tablas/indice_salud_arrecifal_sitios.csv", show_col_types = FALSE) |>
  mutate(categoria_ISA = factor(categoria_ISA, levels = c("Crítico", "Pobre", "Regular", "Bueno", "Muy bueno")))
colores <- c("Crítico" = "#d7191c", "Pobre" = "#fdae61", "Regular" = "#ffffbf",
             "Bueno" = "#a6d96a", "Muy bueno" = "#1a9641")

g <- ggplot(isa, aes(lon, lat)) +
  geom_point(aes(fill = categoria_ISA, shape = zona), size = 5, colour = "grey20") +
  geom_text(aes(label = sitio_id), nudge_y = 0.012, size = 3) +
  scale_fill_manual(values = colores, drop = FALSE) +
  scale_shape_manual(values = c("Protección" = 21, "Uso pesquero" = 24)) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  coord_quickmap() +
  labs(x = "Longitud", y = "Latitud", fill = "ISA", shape = "Zona",
       title = "Sitios de muestreo e ISA (COORDENADAS FICTICIAS)") +
  theme_bw()
ggsave("resultados/figuras/mapa_sitios_isa.png", g, width = 7, height = 5, dpi = 150)

# Exportar para QGIS (capa de puntos CSV con lon/lat, EPSG:4326)
write_csv(isa, "resultados/tablas/sitios_isa_para_qgis.csv")

# Con 'sf' (opcional):
# library(sf)
# pts <- st_as_sf(isa, coords = c("lon", "lat"), crs = 4326)
# st_write(pts, "resultados/sitios_isa.gpkg")
