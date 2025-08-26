library(sf)
library(ggplot2)
library(ggspatial)
library(tidyverse)
library(maptiles)
library(terra)

# create a data frame for points in WGS84 (the Coordinate system used by Google Maps)
pts_wgs84 <- data.frame(
  name = c("Black-a-Tor", "Woodlands Walk", "Yarner Wood", "Wistman's Wood"),
  lon  = c(-4.0351189, -3.8217419, -3.7375203, -3.9646789),
  lat  = c(50.6833152, 50.6130137, 50.5963214, 50.5760747),#,
  type = c("Detected", "Undetected", "Undetected", "Undetected")  # used for the legend
) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

# reproject to British National Grid
pts_bng <- st_transform(pts_wgs84, 27700)

# create an area of interest by buffering around all points
buffer_m <- 5000
aoi_bng <- st_buffer(st_union(pts_bng), buffer_m)
# extract the bounding box (xmin, ymin, xmax, ymax) of the buffered area
bb_bng  <- st_bbox(aoi_bng)

# reproject aoi back to wgs to fetch basemap tiles
# because get_tiles() expects lon/lat input
aoi_wgs84 <- st_transform(aoi_bng, 4326)
# fetch basemap tiles (EPSG:3857 / Web Mercator)
basemap <- get_tiles(aoi_wgs84, provider = "OpenStreetMap", zoom = 12)

# reproject basemap tiles from Web Mercator (EPSG:3857) to BNG (EPSG:27700)
# choose bilinear resampling for smoother results
basemap <- project(basemap, "EPSG:27700", method = "bilinear")

# plot basemap, points, accurate scale, north arrow, legend
ggplot() +
  layer_spatial(basemap) +
  geom_sf(data = pts_bng, aes(shape = type, colour = name), size = 3) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.5) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.2, "cm"),
                         pad_y = unit(0.2, "cm"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(crs = st_crs(27700),
           xlim = c(bb_bng["xmin"], bb_bng["xmax"]),
           ylim = c(bb_bng["ymin"], bb_bng["ymax"]),
           expand = FALSE) +
  labs(title = "Biophone Deployment Sites",
       shape = "Red Fox Detection",
       colour = "Site Name",
       hjust = 0.5) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )
