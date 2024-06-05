### Spatial layers for the manuscript: Seropositivity to Dengue virus DENV in 
### three neighborhoods in the periphery of a city with a recent history of 
### outbreaks in Argentina: what can we learn from unreported cases?
### Author: Tamara Ricardo
### Last update: 
# Tue Jun  4 14:19:20 2024 ------------------------------


# Load packages -----------------------------------------------------------
# devtools::install_github("Issengard83/xlsx2geojson")
pacman::p_load(
  xlsx2geojson,
  sf,
  rio,
  janitor,
  tidyverse
)


# Load spatial datasets ---------------------------------------------------
## Sampled neighborhoods
neighborhood <- excel_to_geojson(data = "../GIS/shp/Registro Nacional de Barrios Populares 2023 - Santa Fe.xlsx") |> 
  
  # filter sampled neighborhoods
  filter(nombre_del_barrio %in% c("Chalet", "La Vuelta del Paraguayo", "Colastine Sur")
  ) |> 
  
  # select relevant columns
  select(name = nombre_del_barrio,
         category = tipologia_barrial,
         electricity = situacion_predominante_sobre_la_conexion_a_la_energia_electrica,
         sewage = situacion_predominante_sobre_la_conexion_a_la_red_cloacal,
         tap_water = situacion_predominante_sobre_la_conexion_a_la_red_de_agua) |>
  
  # add identifiers
  mutate(id = case_when(name == "Chalet" ~ "CH",
                        name == "Colastine Sur" ~ "CS",
                        TRUE ~ "VP"))


# Export layer
st_write(neighborhood, "gis/neighborhoods.geojson", 
         delete_dsn = T,
         delete.layer = T)


## Health centers and hospitals
health_units <- import("../GIS/shp/Establecimientos de salud sin internación - ud - Público - Santa Fe.xlsx") |> 
  
  # filter relevant rows
  filter(Tipología == 2 & 
           Dependencia %in% c(3, 8) & 
           Departamento == "LA CAPITAL" & 
           Localidad != "RECREO" & 
           !grepl("COVID", Nombre)) |> 
  
  # Add public hospitals
  full_join(import("../GIS/shp/Establecimientos de salud con internación - ud - Público - Santa Fe.xlsx"
  ) |> 
    
    # filter relevant rows
    filter(Categoría == 2 | grepl("CEMA|PROV", Nombre))) |> 
  
  # clean variable names
  clean_names() |> 
  
  # categorize health centers
  mutate(category = if_else(grepl("HOSP", nombre), 
                             "Hospital", "Health unit")) |> 
  
  # select relevant columns
  select(name = nombre, category, latitud, longitud) |> 
  
  # transform to spatial object
  st_as_sf(coords = c(x = "longitud", y = "latitud"), crs = 4326)


# Export layer
st_write(health_units, "gis/health_units.geojson", 
         delete_dsn = T,
         delete.layer = T)


## Health vulnerability
health_v <- excel_to_geojson(data = "../GIS/shp/Vulnerabilidad sanitaria 2010-2018 - ud - Santa Fe.xlsx") |> 
  
  select(census_tract = radio, 
         population = poblacion_total, 
         households = total_de_hogares, 
         sanitary_vulnerability = vulnerabilidad_sanitaria)

# Export layer
st_write(health_v, "gis/health_v.geojson", 
         delete_dsn = T,
         delete.layer = T)


## Population density and marginality
strata <- excel_to_geojson(data = "../GIS/shp/Estratificación de radios censales - ud - Santa Fe.xlsx") |> 
  
  # filter relevant columns
  select(census_tract = codigo_de_radio_1,
         area_km2 = superficie_en_km2_67,
         population = poblacion_total,
         density_km2 = densidad_de_poblacion_x_km2,
         households = cantidad_total_de_hogares,
         households_nbi = hogares_con_al_menos_un_indicador_nbi,
         households_overcrowding = hogares_con_hacinamiento_3_personas_por_cuarto,
         households_no_tap_water = hogares_sin_caneria_de_agua_en_la_vivienda,
         households_no_sewage = hogares_sin_cloaca_red_publica,
         households_poor_housing = hogares_con_calidad_de_la_vivienda_precaria,
         households_high_marginality = hogares_con_indicadores_de_alta_marginalidad,
         socioeconomic_strata = estrato_por_radio) |> 
  
  # Modify columns
  mutate(
    # round decimals
    density_km2 = round(density_km2, 1),
    # convert to percentages
  across(.cols = starts_with("households_"), 
                .fns = ~ round(.x*100/households, 1)))


# Export layer
st_write(strata, "gis/strata_ct.geojson", 
         delete_dsn = T,
         delete.layer = T)
