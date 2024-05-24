library(UsefulFunctions)
library(leaflet)
library(htmltools)
library(geojsonR)

## get geojson from geoportal API - ultra generalised police force areas 
pcc_regions_ultra_genrealised <- sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Police_Force_Areas_December_2023_EW_BUC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

## get geojson from geoportal API - full resolution police force areas
pcc_regions_full_resolution <- sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Police_Force_Areas_December_2023_EW_BFC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

## draw police force area boundaries using leaflet
pcc_regions_full_resolution %>% 
  leaflet() %>% 
  setView(lng = -2.4, lat = 55.5, zoom = 6) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik,
                   group = "Base Map") %>% 
  addPolygons(fillOpacity = 0,
              fillColor = "white",
              color = "#4C1354",
              opacity = 1,
              weight = 5,
              label = ~PFA23NM)
