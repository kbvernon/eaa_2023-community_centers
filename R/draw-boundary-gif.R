library(gifski)
library(here)
library(magick)
library(patchwork)
library(rmapshaper)
library(sf)
library(tigris)
library(tidyverse)

cbsa <- core_based_statistical_areas() |> 
  filter(str_detect(NAME, "Fargo|Tuscaloosa|Pittsburgh|New York")) |> 
  rename_with(tolower) |> 
  select(name) |> 
  mutate(type = "CBSA")

xy <- lapply(cbsa$geometry, \(x){ 
  
  n <- st_coordinates(x) |> nrow()
  
  g <- ms_simplify(st_sfc(x), keep = 100/n)
  
  st_coordinates(g)[, 1:2] 
  
})

# make them all the same number of rows
xy[[1]] <- xy[[1]][c(1:101, 101), ]

bb8 <- lapply(cbsa$geometry, st_bbox)

plot_cbsa <- function(x, bb8, ...){
  
  ggplot() +
    geom_sf(
      data = x,
      fill = "#15616D",
      color = "#001524",
      linewidth = 1
    ) +
    coord_sf(
      crs = 4326, 
      datum = 4326, 
      expand = FALSE,
      xlim = bb8[c("xmin", "xmax")],
      ylim = bb8[c("ymin", "ymax")]
    ) +
    labs(caption = "bob") +
    theme_void(12) +
    theme(
      plot.caption = element_text(
        size = rel(1), 
        hjust = 0.5, 
        color = "white",
        margin = margin(t = 8)
      ),
      plot.margin = margin(...)
    )
  
}

build_geometry <- function(x, i){
  
  shp <- if (i == 1){ 
    
    NULL 
    
  } else if (i == nrow(x)){  
    
    st_polygon(list(x[1:i, , drop = FALSE]))
    
  } else { 
    
    st_linestring(x[1:i, , drop = FALSE])
    
  }
  
  st_sf(geom = st_sfc(shp), crs = 4326)
  
}

make_plot <- function(x, bb8){
  
  cli::cli_progress_bar("Generating plots.", total = 102)

  for (i in 1:102){
    
    g <- 
      plot_cbsa(build_geometry(x[[1]], i), bb8[[1]], r = 24) +
      plot_cbsa(build_geometry(x[[2]], i), bb8[[2]], r = 24, l = 24) +
      plot_cbsa(build_geometry(x[[3]], i), bb8[[3]], r = 24, l = 24) +
      plot_cbsa(build_geometry(x[[4]], i), bb8[[4]], l = 24) +
      plot_layout(ncol = 4)
    
    print(g)
    
    cli::cli_progress_update()

  }
  
}

save_gif(
  make_plot(xy, bb8),
  gif_file = here("figures", "draw-example-boundary.gif"),
  width = 300 * 9.85, 
  height = 300 * 2.5, 
  res = 300,
  delay = 0.05,
  loop = FALSE
)
