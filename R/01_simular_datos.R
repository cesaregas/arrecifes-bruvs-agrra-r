# =============================================================================
# 01_simular_datos.R
# Genera DATOS FICTICIOS para practicar. NO son datos reales de los Cayos
# Miskitos: sitios, coordenadas, conteos y tallas son inventados.
# Los CSV ya vienen en la carpeta datos/; solo ejecuta este script si quieres
# regenerarlos o cambiar el diseño (más sitios, otro efecto de la pesca, etc.).
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)

set.seed(2026)
dir.create("datos", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Sitios: 12 sitios, mitad en zona de uso pesquero y mitad en una zona
#    hipotética de protección. Coordenadas ficticias.
# -----------------------------------------------------------------------------
sitios <- tibble(
  sitio_id      = sprintf("S%02d", 1:12),
  zona          = rep(c("Uso pesquero", "Protección"), each = 6),
  habitat       = rep(c("Arrecife frontal", "Arrecife de parche"), times = 6),
  profundidad_m = round(runif(12, 6, 18), 1),
  lat           = round(runif(12, 15.80, 16.05), 5),
  lon           = round(runif(12, -83.40, -83.00), 5)
)
write_csv(sitios, "datos/sitios.csv")

# Efecto de la pesca: multiplicador < 1 en la zona de uso pesquero
efecto_pesca <- function(zona, fuerza) ifelse(zona == "Uso pesquero", fuerza, 1)

# -----------------------------------------------------------------------------
# 2. BRUVs: 3 despliegues de 60 min por sitio. Valor anotado: MaxN.
#    Como en la vida real, la planilla de anotación SOLO trae las especies
#    vistas (no hay filas con MaxN = 0). Completar ceros es parte del ejercicio.
# -----------------------------------------------------------------------------
sp_bruvs <- tribble(
  ~especie,                    ~grupo,          ~lambda, ~sensible_pesca,
  "Ginglymostoma cirratum",    "Elasmobranquio", 0.9,    0.5,
  "Carcharhinus perezi",       "Elasmobranquio", 0.6,    0.3,
  "Hypanus americanus",        "Elasmobranquio", 0.5,    0.7,
  "Aetobatus narinari",        "Elasmobranquio", 0.3,    0.8,
  "Lutjanus apodus",           "Comercial",      3.0,    0.5,
  "Lutjanus griseus",          "Comercial",      1.5,    0.5,
  "Ocyurus chrysurus",         "Comercial",      4.0,    0.6,
  "Epinephelus striatus",      "Comercial",      0.5,    0.3,
  "Mycteroperca bonaci",       "Comercial",      0.4,    0.3,
  "Sphyraena barracuda",       "Otro",           1.0,    0.9,
  "Caranx ruber",              "Otro",           3.5,    1.0,
  "Haemulon plumierii",        "Otro",           6.0,    1.0,
  "Sparisoma viride",          "Herbívoro",      2.5,    0.8,
  "Acanthurus coeruleus",      "Herbívoro",      4.0,    1.0,
  "Pterois volitans",          "Invasor",        0.8,    1.0
)

despliegues <- sitios |>
  slice(rep(1:n(), each = 3)) |>
  group_by(sitio_id) |>
  mutate(despliegue_id = paste0(sitio_id, "_B", row_number())) |>
  ungroup() |>
  mutate(fecha = as.Date("2026-05-10") + sample(0:12, n(), replace = TRUE),
         profundidad_m = round(profundidad_m + rnorm(n(), 0, 1), 1),
         tiempo_min = 60)

bruvs <- despliegues |>
  crossing(sp_bruvs) |>
  mutate(mu   = lambda * efecto_pesca(zona, sensible_pesca),
         maxn = rnbinom(n(), mu = mu, size = 1.2)) |>
  filter(maxn > 0) |>                       # solo especies vistas
  select(despliegue_id, sitio_id, fecha, profundidad_m, tiempo_min, especie, maxn)

# Errores intencionales para practicar el control de calidad:
i1 <- which(bruvs$especie == "Ginglymostoma cirratum")[2]
i2 <- which(bruvs$especie == "Lutjanus apodus")[3]
bruvs$especie[i1] <- "Ginglymostoma cirratun"           # error de tipeo
bruvs$especie[i2] <- tolower(bruvs$especie[i2])        # minúscula
bruvs <- bind_rows(bruvs, bruvs[5, ])            # fila duplicada
write_csv(bruvs, "datos/bruvs_maxn.csv")
# Lista de referencia de especies (en un proyecto real: nombres validados en WoRMS)
write_csv(sp_bruvs |> select(especie, grupo), "datos/lista_referencia_bruvs.csv")
write_csv(despliegues |> select(despliegue_id, sitio_id, fecha, profundidad_m, tiempo_min),
          "datos/bruvs_despliegues.csv")

# -----------------------------------------------------------------------------
# 3. AGRRA - bentos: 5 transectos de 10 m por sitio, 100 puntos por transecto
#    (un punto cada 10 cm). Categorías simplificadas.
# -----------------------------------------------------------------------------
cats <- c("CV", "MAC", "ACC", "TUR", "ESP", "ARE")
# CV coral vivo, MAC macroalga carnosa, ACC alga calcárea costrosa,
# TUR césped algal (turf), ESP esponja, ARE arena/sustrato abiótico
bentos <- sitios |>
  crossing(transecto = 1:5, punto = 1:100) |>
  rowwise() |>
  mutate(categoria = {
    p <- if (zona == "Uso pesquero") c(.12, .20, .08, .38, .07, .15) else c(.20, .10, .10, .35, .08, .17)
    sample(cats, 1, prob = p)
  }) |>
  ungroup() |>
  select(sitio_id, transecto, punto, categoria)
write_csv(bentos, "datos/agrra_bentos.csv")

# -----------------------------------------------------------------------------
# 4. AGRRA - corales: colonias >= 4 cm a lo largo de los transectos.
# -----------------------------------------------------------------------------
sp_coral <- c("Orbicella annularis", "Orbicella faveolata", "Montastraea cavernosa",
              "Siderastrea siderea", "Porites astreoides", "Agaricia agaricites",
              "Pseudodiploria strigosa", "Acropora palmata")
corales <- sitios |>
  crossing(transecto = 1:5) |>
  mutate(n = rpois(n(), 9)) |>
  uncount(n) |>
  mutate(especie          = sample(sp_coral, n(), replace = TRUE),
         diam_max_cm      = round(pmax(4, rlnorm(n(), log(25), 0.7))),
         mort_antigua_pct = round(pmin(100, rbeta(n(), 1.2, 4) * 100)),
         mort_reciente_pct = round(pmin(100 - mort_antigua_pct, rbeta(n(), 0.4, 8) * 100)),
         blanqueamiento   = sample(c("NO", "P", "PB", "BL"), n(), replace = TRUE,
                                   prob = c(.75, .12, .08, .05)),
         enfermedad       = sample(c("no", "si"), n(), replace = TRUE, prob = c(.9, .1))) |>
  select(sitio_id, transecto, especie, diam_max_cm, mort_antigua_pct,
         mort_reciente_pct, blanqueamiento, enfermedad)
corales$mort_antigua_pct[7] <- 130   # error intencional: fuera de rango
write_csv(corales, "datos/agrra_corales.csv")

# -----------------------------------------------------------------------------
# 5. AGRRA - peces: 6 transectos de banda de 30 x 2 m (60 m2) por sitio,
#    conteo por especie y clase de talla.
# -----------------------------------------------------------------------------
clases <- c("0-5", "6-10", "11-20", "21-30", "31-40", ">40")
sp_peces <- tribble(
  ~especie,               ~familia,        ~lambda, ~sensible_pesca, ~talla_media,
  "Sparisoma viride",     "Scaridae",      2.0,     0.8,             22,
  "Scarus iseri",         "Scaridae",      4.0,     1.0,             12,
  "Scarus taeniopterus",  "Scaridae",      2.5,     0.9,             16,
  "Acanthurus coeruleus", "Acanthuridae",  3.0,     1.0,             15,
  "Acanthurus chirurgus", "Acanthuridae",  1.5,     1.0,             16,
  "Lutjanus apodus",      "Lutjanidae",    1.5,     0.5,             26,
  "Ocyurus chrysurus",    "Lutjanidae",    2.0,     0.6,             24,
  "Epinephelus guttatus", "Serranidae",    0.5,     0.4,             30,
  "Cephalopholis fulva",  "Serranidae",    1.0,     0.7,             20,
  "Haemulon flavolineatum","Haemulidae",   5.0,     1.0,             14,
  "Stegastes partitus",   "Pomacentridae", 6.0,     1.0,              5
)
a_clase <- function(L) as.character(cut(L, c(-Inf, 5, 10, 20, 30, 40, Inf), labels = clases))

peces <- sitios |>
  crossing(transecto = 1:6, sp_peces) |>
  mutate(n_ind = rnbinom(n(), mu = lambda * efecto_pesca(zona, sensible_pesca), size = 1.5)) |>
  filter(n_ind > 0) |>
  uncount(n_ind) |>
  mutate(talla = rlnorm(n(), log(talla_media * efecto_pesca(zona, 0.85)), 0.3),
         clase_talla_cm = a_clase(talla)) |>
  count(sitio_id, transecto, especie, familia, clase_talla_cm, name = "conteo")
write_csv(peces, "datos/agrra_peces.csv")

# -----------------------------------------------------------------------------
# 6. Tabla de parámetros talla-peso W = a * L^b (W en g, L en cm).
#    ¡VALORES ILUSTRATIVOS! Antes de un análisis real, reemplazar a y b con los
#    de FishBase (www.fishbase.se) para cada especie y tipo de longitud.
# -----------------------------------------------------------------------------
lw <- tribble(
  ~especie,                 ~a,     ~b,    ~grupo_isa,
  "Sparisoma viride",       0.0150, 3.05,  "Herbívoro",
  "Scarus iseri",           0.0150, 3.05,  "Herbívoro",
  "Scarus taeniopterus",    0.0150, 3.05,  "Herbívoro",
  "Acanthurus coeruleus",   0.0250, 3.00,  "Herbívoro",
  "Acanthurus chirurgus",   0.0250, 3.00,  "Herbívoro",
  "Lutjanus apodus",        0.0170, 3.00,  "Comercial",
  "Ocyurus chrysurus",      0.0150, 2.95,  "Comercial",
  "Epinephelus guttatus",   0.0120, 3.05,  "Comercial",
  "Cephalopholis fulva",    0.0130, 3.05,  "Comercial",
  "Haemulon flavolineatum", 0.0170, 3.00,  "Otro",
  "Stegastes partitus",     0.0200, 3.00,  "Otro"
)
write_csv(lw, "datos/parametros_talla_peso_ILUSTRATIVOS.csv")

# -----------------------------------------------------------------------------
# 7. Umbrales del Índice de Salud Arrecifal (ISA). ¡SOLO DE EJEMPLO!
#    Reemplazar con la tabla oficial del Report Card vigente de la Iniciativa
#    Arrecifes Saludables (www.healthyreefs.org) antes de un análisis real.
#    Intervalos: valor >= min y valor < max.
# -----------------------------------------------------------------------------
umbrales <- tribble(
  ~indicador,           ~puntaje, ~categoria,  ~min,  ~max,
  "coral_vivo_pct",      1, "Crítico",    0,     5,
  "coral_vivo_pct",      2, "Pobre",      5,     10,
  "coral_vivo_pct",      3, "Regular",    10,    20,
  "coral_vivo_pct",      4, "Bueno",      20,    40,
  "coral_vivo_pct",      5, "Muy bueno",  40,    101,
  "macroalgas_pct",      5, "Muy bueno",  0,     1,
  "macroalgas_pct",      4, "Bueno",      1,     5,
  "macroalgas_pct",      3, "Regular",    5,     12,
  "macroalgas_pct",      2, "Pobre",      12,    25,
  "macroalgas_pct",      1, "Crítico",    25,    101,
  "herbivoros_g100m2",   1, "Crítico",    0,     960,
  "herbivoros_g100m2",   2, "Pobre",      960,   1860,
  "herbivoros_g100m2",   3, "Regular",    1860,  2740,
  "herbivoros_g100m2",   4, "Bueno",      2740,  3290,
  "herbivoros_g100m2",   5, "Muy bueno",  3290,  Inf,
  "comerciales_g100m2",  1, "Crítico",    0,     140,
  "comerciales_g100m2",  2, "Pobre",      140,   410,
  "comerciales_g100m2",  3, "Regular",    410,   1210,
  "comerciales_g100m2",  4, "Bueno",      1210,  1620,
  "comerciales_g100m2",  5, "Muy bueno",  1620,  Inf
)
write_csv(umbrales, "datos/umbrales_ISA_EJEMPLO.csv")

message("Datos ficticios generados en datos/")
