# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


library(sf)
library(sp)
library(tidyverse)
library(dplyr)
library(raster)
library(MASS)
library(kde1d)




vector_heatmap <- function(capa,radius=NA,resolution=NA){


  #Standar resolution
  if(is.na(resolution)) {resolution=200}
  if(is.na(radius)) {radius=600}

  # Read information
  Capa_heatmap <- st_read(capa)

  # Transform to a crs according to the metric system
  Capa_heatmap <- st_transform(Capa_heatmap,3857)


  # Capture the latitude and longitude of the points
  Capa_heatmap <- Capa_heatmap %>% dplyr::mutate(long = unlist(map(Capa_heatmap$geometry,1)),
                                                 lat = unlist(map(Capa_heatmap$geometry,2)))

  # Increase the size of the raster

  prop <- 0.999
  min_long <- if_else(min(Capa_heatmap$long)>0,min(Capa_heatmap$long)*prop,min(Capa_heatmap$long)/prop)
  max_long <- if_else(max(Capa_heatmap$long)>0,max(Capa_heatmap$long)/prop,max(Capa_heatmap$long)*prop)
  min_lat <- if_else(min(Capa_heatmap$lat)>0,min(Capa_heatmap$lat)*prop,min(Capa_heatmap$lat)/prop)
  max_lat <- if_else(max(Capa_heatmap$lat)>0,max(Capa_heatmap$lat)/prop,max(Capa_heatmap$lat)*prop)
  lim <- c(min_long,max_long,min_lat,max_lat)

  # Generate de heatmap layer

  k = kde2d(Capa_heatmap$long,Capa_heatmap$lat,h=radius,n=resolution,lims = lim)
  r = raster(k)

  # Eliminate lower values
  clases1 <- reclassify(r, c(-Inf,0.00000001,NA))


  # Vectorized de raster
  Vectorizado = rasterToPolygons(clases1, dissolve = FALSE) %>% st_as_sf() %>% dplyr::mutate(layer=(layer*100000000)-1) %>% dplyr::mutate(concentración=ceiling(layer)) %>% dplyr::select(concentración)


  # Give the vector layer de original crs
  st_crs(Vectorizado) <- crs(Capa_heatmap)


  # Eliminate errors
  Vectorizado_unido <- Vectorizado %>% st_buffer(0.1)


  # Unify polygons
  Vectorizado_unido <- Vectorizado_unido %>% dplyr::group_by(concentración) %>% dplyr::summarise(geometry=st_union(geometry)) %>% dplyr::ungroup()
}

