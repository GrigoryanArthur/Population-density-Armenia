library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)

# load kontur data

#data <- st_read("data/kontur_population_US_20220630.gpkg")
data <- st_read("data/kontur_population_AM_20220630.gpkg")

# define aspect ratio based on bounding box

bb <- st_bbox(data)


bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

# check by plotting points

data |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side

w_ratio <- 1
h_ratio <- 1

# convert to raster so we can then convert to matrix

size <- 1000

data_rast <- st_rasterize(data, nx = floor(size * w_ratio),ny = floor(size * h_ratio))

mat <- matrix(data_rast$population, nrow = floor(size * w_ratio),ncol = floor(size * h_ratio))

# create color palette

c1 <- met.brewer("Paquin")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 2)(256)
swatchplot(texture)

# plot that 3d thing!

rgl::rgl.close()

mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 30,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = -20, phi = 45, zoom = .55)


