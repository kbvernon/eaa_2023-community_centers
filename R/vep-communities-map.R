
library(ggspatial)
library(here)
library(magick)
library(patchwork)
library(sf)
library(terra)
library(tidyterra)
library(tidyverse)

build_region <- function(gpkg, region){
  
  region_query <- paste0("SELECT * FROM regions WHERE region = '", region, "'")
  r <- read_sf(gpkg, query = region_query)
  
  site_query <- paste0("SELECT * FROM sites WHERE region = '", region, "'")
  s <- read_sf(gpkg, query = site_query) |> st_filter(r)
  
  community_query <- paste0("SELECT * FROM communities WHERE region = '", region, "'")
  k <- read_sf(gpkg, query = community_query)
  
  list(
    name = region,
    preferred_projection = ifelse(region == "nrg", 26913, 26912),
    region = r,
    sites = s,
    communities = k
  )
  
}

add_elevation <- function(x){
  
  dem_fn <- paste0("dem-", x$name, ".tif")
  x$dem <- here("data", dem_fn) |> rast()
  
  return(x)
  
}

visualize_communities <- function(x, sites = TRUE){
  
  hell <- function(x, n = 1000){
    
    slope <- terrain(x, "slope", unit = "radians")
    aspect <- terrain(x, "aspect", unit = "radians")
    
    hill <- shade(slope, aspect, 30, 45)
    hill <- setValues(hill, scales::rescale(values(hill), to = c(1, n)))
    hill <- round(hill)
    
    list(
      shade = hill,
      grays = hcl.colors(n, "Grays")[values(hill)]
    )
    
  }
  
  h <- hell(x$dem)
  
  ggplot() +
    geom_spatraster(
      data = h$shade, 
      fill = h$grays,
      maxcell = Inf
    ) +
    geom_spatraster(data = x$dem, alpha = 0.5,) +
    scale_fill_distiller(palette = "Greys", guide = "none") +
    geom_sf(
      data = x$communities,
      fill = alpha("#EEFC57", 0.5),
      color = "gray95",
      linewidth = 0.3
    ) + 
    annotation_scale(
      aes(location = "bl", line_col = "white", text_col = "white"),
      height = unit(0.18, "cm"),
      line_width = 0.5
    ) +
    coord_sf(expand = FALSE) +
    theme_bw(12) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.title = element_text(margin = margin(b = 2))
    )
  
}

gpkg <- here("data", "community-centers.gpkg")

cmv <- build_region(gpkg, "cmv") |> add_elevation()
nrg <- build_region(gpkg, "nrg") |> add_elevation()

gg_cmv <- visualize_communities(cmv) + 
  ggtitle("Central Mesa Verde") + 
  theme(plot.margin = margin(r=2))

gg_nrg <- visualize_communities(nrg) + 
  ggtitle("Northern Rio Grande") + 
  theme(plot.margin = margin(l=2))

gg_cmv + gg_nrg

fn <- here("figures", "communities-no-sites.png")

ggsave(
  filename = fn,
  width = 9, 
  height = 5,
  dpi = 600
)

image_read(path = fn) |> 
  image_trim() |> 
  image_write(path = fn)