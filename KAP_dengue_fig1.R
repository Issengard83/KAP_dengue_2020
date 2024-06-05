### Figure 1 of the manuscript: Seropositivity to Dengue virus DENV in 
### three neighborhoods in the periphery of a city with a recent history of 
### outbreaks in Argentina: what can we learn from unreported cases?
### Last update:
# Wed Jun  5 09:38:18 2024 ------------------------------


# Load packages -----------------------------------------------------------
pacman::p_load(
  ### Maps
  sf, 
  tmap,
  tmaptools,
  maptiles,
  
  ### Graphic tools
  scico, 
  
  ### Data management
  tidyverse
)

# Load spatial layers -----------------------------------------------------
# sampled neighborhoods
neighborhood <- st_read("../MAPAS/map_dengue_2020/neighborhoods.geojson")

# health units
health <- st_read("../MAPAS/map_dengue_2020/health_units.geojson")

## Sociodemographic layers
strata_ct <- st_read("../MAPAS/map_dengue_2020/strata_ct.geojson") |> 
  # add sanitary deficiencies layer
  st_join(st_read("../MAPAS/map_dengue_2020/health_v.geojson"), 
          join = st_equals) |> 
  
  # modify scales
  mutate(sanitary_vulnerability = round(sanitary_vulnerability*100, 1)) |> 
  select(-ends_with(".y"))


## Argentina shapefile
arg <- st_read("../GIS/shp/ign_provincia.json") %>% 
  
  # validate geometry
  st_make_valid() %>% 
  
  # crop Antarctica
  st_crop(y = st_bbox(c(xmin = -74, xmax = -50,
                        ymin = -60, ymax = -22))) %>% 
  
  # set color scale
  mutate(col_sf = if_else(nam == "Santa Fe", "si", "no"))


# Fig. 1: Map of the study area -------------------------------------------
### Generate map pins
hosp_icons <- tmap_icons(file = c("../GIS/icons/placeholder_429238.png",
                                  "../GIS/icons/hospital_11384594.png"))

icons <- tmap_icons(file = "../GIS/icons/pin_10075433.png")


### Argentina minimap
## South America bbox
SA_bbox <- st_bbox(c(xmin = -81.0, ymin = -56.0, xmax = -34.0, ymax = 13.0), 
                   crs = st_crs(4326))

## Plot map
arg_map <- 
  # OSM basemap
  tm_shape(shp = read_osm(x = SA_bbox)) +
  tm_rgb() +
  
  # add provinces polygon
  tm_shape(shp = arg) +
  tm_polygons(col = "col_sf", 
              palette = scico(n = 2, palette = "buda", direction = -1, alpha = .75),
              legend.show = F) +
  
  # add coordinates
  tm_graticules(lines = F,
                labels.cardinal = T) +
  
  # North arrow
  tm_compass(size = .75, text.size = .5) +
  
  # map layout
  tm_layout(inner.margins = rep(0, 4),
            outer.margins = rep(.1, 4))

### Map of the study area
sf_map <- 
  ## OSM basemap
  tm_shape(shp = read_osm(x = st_bbox(strata_ct)), zoom = 8) +
  tm_rgb() +
  
  # add population density and sanitary vulnerability
  tm_shape(shp = strata_ct) +
  tm_polygons(col = c("sanitary_vulnerability", "density_km2"),
              style = "quantile",
              palette = scico(n = 5, palette = "tokyo", alpha = .75, direction = -1),
              title = c("Sanitary vulnerability (%)", "Population density (km2)")) +
  
  # add health centers
  tm_shape(shp = health) +
  tm_markers(shape = "category",
             shapes = hosp_icons,
             border.lwd = 0,
             border.col = "white",
             scale = .25,
             legend.shape.show = F) +
  
  ## add sampled neighborhoods
  tm_shape(shp = neighborhood) +
  tm_bubbles(shape = icons, 
             scale = .3, 
             border.lwd = 0) +
  
  tm_text(text = "id", 
          fontface = "bold",
          size = .5,
          ymod = -.5, 
          shadow = T) +
  
  ## Add coordinates
  tm_graticules(lines = F, n.x = 5, n.y = 5) +
  
  ## North arrow
  tm_compass(size = 1.25, text.size = .5) +
  
  ## Scale bar
  tm_scale_bar() +
  
  ## Layout
  tm_layout(inner.margins = c(0,0,0,0),
            legend.outside = F,
            legend.position = c("right", "top"),
            legend.title.size = .75,
            legend.text.size = .5) +
  
  tm_facets(nrow = 2, 
            free.scales = T)


## unite maps
fig1 <- tmap_arrange(arg_map, sf_map,
                     asp = NA,
                     outer.margins = 0.01,
                     widths = c(.5, 1))


# ## export map
tmap_save(fig1, "fig1.jpeg", width = 190, height = 100, units = "mm", dpi = 500)

rm(list = ls())
