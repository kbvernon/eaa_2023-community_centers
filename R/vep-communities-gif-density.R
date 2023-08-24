library(ggnewscale)
library(ggspatial)
library(gifski)
library(here)
library(patchwork)
library(sf)
library(terra)
library(tidyterra)
library(tidyverse)
library(tweenr)

build_region <- function(gpkg, region){
  
  region_query <- paste0("SELECT * FROM regions WHERE region = '", region, "'")
  r <- read_sf(gpkg, query = region_query)
  
  site_query <- paste0("SELECT * FROM sites WHERE region = '", region, "'")
  s <- read_sf(gpkg, query = site_query) |> st_filter(r)
  
  community_query <- paste0("SELECT * FROM communities WHERE region = '", region, "'")
  k <- read_sf(gpkg, query = community_query)
  
  phase_query <- paste0("SELECT * FROM phases WHERE region = '", region, "'")
  p <- read_sf(gpkg, query = phase_query)
  
  list(
    name = region,
    preferred_projection = ifelse(region == "nrg", 26913, 26912),
    region = r,
    sites = s,
    communities = k,
    phases = p
  )
  
}

add_elevation <- function(x){
  
  dem_fn <- paste0("dem-", x$name, ".tif")
  x$dem <- here("data", dem_fn) |> rast()
  
  return(x)
  
}

complete_phases <- function(x){
  
  x$communities$area <- st_area(x$communities) |> 
    units::set_units(km^2) |> 
    units::drop_units()
  
  x$density <- x$sites |> 
    st_join(x$communities[, c("community", "area")]) |> 
    st_drop_geometry() |> 
    drop_na(community) |> 
    rowwise() |> 
    mutate(
      uniform_count = n_room/(1 + final_phase - initial_phase),
      uniform_density = uniform_count/area,
      phase = list(seq(initial_phase, final_phase, by = 1))
    ) |> 
    unnest(phase) |> 
    ungroup() |> 
    select(community, phase, uniform_density) |> 
    group_by(community, phase) |> 
    summarize(density = sum(uniform_density)) |> 
    ungroup() |> 
    complete(community, phase, fill = list(density = 0))
  
  return(x)

}

fudge_density <- function(x){
  
  # range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  
  x$density <- x$density |> mutate(
    density = log(density + 1),
    # density = (density - mean(density))/sd(density)
  )
  
  return(x)
  
}

tween_density <- function(x, bb8, ..., nframes = 15){
  
  xrng <- bb8[c("xmin", "xmax")] |> unname()
  dx <- (xrng[2]-xrng[1]) * 0.025
  
  xrng <- xrng + c(dx, -dx)
  
  yrng <- bb8[c("ymin", "ymax")] |> unname()
  dy <- (yrng[2]-yrng[1]) * 0.025
  
  yrng <- yrng + c(dy, -dy)
  
  d <- x$density |> 
    mutate(
      map_phase = scales::rescale(phase, to = xrng),
      map_density = scales::rescale(density, to = yrng)
    )
  
  d <- bind_rows(
    d,
    d |> 
      group_by(phase) |> 
      summarize(
        community = 9000, 
        density = median(density),
        map_density_05 = quantile(map_density, p = 0.05) |> unname(),
        map_density_25 = quantile(map_density, p = 0.25) |> unname(),
        map_density_50 = quantile(map_density, p = 0.50) |> unname(),
        map_density_75 = quantile(map_density, p = 0.75) |> unname(),
        map_density_95 = quantile(map_density, p = 0.95) |> unname(),
        map_phase = unique(map_phase)
      ) |> 
      ungroup()
  )

  s <- split(d, d$phase)
  
  q <- vector(mode = "list", length = length(s)-1)
  
  suppressWarnings({
    
    for (i in 1:(length(s)-1)){
      
      fp <- nframes * (i-1)
      
      q[[i]] <- tween_state(
        s[[i]], 
        to = s[[i+1]], 
        ease = "linear", 
        nframes = nframes, 
        id = community
      ) |> mutate(.frame = .frame + fp)
      
    }
    
  })
  
  x$density_tween <- bind_rows(q)
  
  return(x)
  
}

build_basemap <- function(x){
  
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
    new_scale_fill()
  
}

add_defaults <- function(x, ...){
  
  x +
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
      plot.margin = margin(...),
      plot.title = element_text(margin = margin(b = 2))
    )
  
}

demographic_profile <- function(x, bb8, frame, ...){
  
  n <- x |> filter(!is.na(begin)) |> pull(phase) |> unique() |> length()
  
  x <- x |> filter(.frame %in% 1:frame)
  
  if (frame == 1) x <- bind_rows(x, x)
  
  yrng <- bb8[["ymax"]] - bb8[["ymin"]]
  xrng <- bb8[["xmax"]] - bb8[["xmin"]]
  
  lbls <- x |> 
    filter(!is.na(begin), community == 9000) |> 
    select(phase, begin, map_phase, map_density_05, map_density_95) |> 
    distinct() |> 
    mutate(map_density_95 = map_density_95 + (yrng * 0.05))
    
  ggplot() + 
    ggtitle("Relative Density") +
    geom_sf(
      data = st_as_sfc(bb8, crs = 4326),
      color = "transparent",
      fill = "transparent"
    ) +
    geom_segment(
      data = lbls,
      aes(map_phase, map_density_05, xend = map_phase, yend = map_density_95),
      color = "#ced4da",
      linewidth = 0.2
    ) +
    geom_text(
      data = lbls,
      aes(map_phase, map_density_95, label = begin),
      size = 9/.pt,
      hjust = seq(0.3, 0.7, length = n)[1:nrow(lbls)],
      vjust = 0,
      nudge_y = (yrng * 0.01),
      color = "#6c757d"
    ) +
    geom_ribbon(
      data = x |> filter(community == 9000),
      aes(map_phase, ymin = map_density_05, ymax = map_density_95, group = community),
      fill = alpha("#FF7D00", 0.2),
      color = "#FF7D00",
      linewidth = 0.2
    ) +
    geom_ribbon(
      data = x |> filter(community == 9000),
      aes(map_phase, ymin = map_density_25, ymax = map_density_75, group = community),
      fill = alpha("#FF7D00", 0.2),
      color = "#FF7D00",
      linewidth = 0.2
    ) +
    geom_line(
      data = x |> filter(community == 9000),
      aes(map_phase, map_density_50, group = community),
      color = "#ffffff",
      linewidth = 3
    ) +
    geom_line(
      data = x |> filter(community == 9000),
      aes(map_phase, map_density_50, group = community),
      color = "#78290F",
      linewidth = 0.9
    ) +
    coord_sf(
      expand = FALSE,
      xlim = bb8[c("xmin", "xmax")],
      ylim = bb8[c("ymin", "ymax")]
    ) +
    theme_bw(12) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.margin = margin(...),
      plot.title = element_text(margin = margin(b = 2))
    )
  
}

make_plot <- function(x, bb8){
  
  frames <- unique(x$density_tween$.frame)

  gg <- build_basemap(x)
  
  r <- if (x$name == "nrg"){
    "Northern Rio Grande"
  } else {
    "Central Mesa Verde"
  }
  
  limits <- x$density_tween |> pull(density) |> range()
  
  temp_dir <- tempdir()
  
  image_files <- vector("character")
  
  cli::cli_progress_bar("Generating plots.", total = length(frames))
  
  for (frame in frames) {
    
    z <- x$density_tween |> filter(.frame == frame)
    
    year <- x$phases |> filter(phase_id == floor(z$phase[1])) |> pull(begin)
    
    d <- x$communities |> left_join(z, by = "community")

    g <- gg + 
      geom_sf(
        data = d,
        aes(fill = density),
        color = "gray35",
        linewidth = 0.3
      ) + 
      scale_fill_distiller(
        palette = "YlOrBr", 
        direction = 1,
        limits = limits
      ) +
      ggtitle(paste0(r, " [", year, " CE]"))
    
    frame_c <- if (frame < 10) paste0("0", frame) else frame
    
    fn <- file.path(temp_dir, paste0(x$name, "-frame-", frame_c, ".png"))
    
    image_files <- c(image_files, fn)
    
    if (x$name == "cmv"){
      
      g <- add_defaults(g, r = 2)
      
      p <- demographic_profile(x$density_tween, bb8, frame, l=2)
      
      ggsave(
        filename = fn,
        plot = g + p,
        width = 9,
        height = 5,
        dpi = 300
      )
      
    } else {
      
      g <- add_defaults(g, l = 2)
      
      p <- demographic_profile(x$density_tween, bb8, frame, r=2)
      
      ggsave(
        filename = fn,
        plot = p + g,
        width = 9,
        height = 5,
        dpi = 300
      )
      
    }
    
    magick::image_read(path = fn) |> 
      magick::image_trim() |> 
      magick::image_write(path = fn)
    
    cli::cli_progress_update()
    
  }
  
  return(image_files)
  
}

gpkg <- here("data", "community-centers.gpkg")

cmv <- build_region(gpkg, "cmv") |> 
  add_elevation() |> 
  complete_phases() |> 
  fudge_density()

nrg <- build_region(gpkg, "nrg") |> 
  add_elevation() |> 
  complete_phases() |> 
  fudge_density()

nrg$density <- nrg$density |> filter(phase < 8)

cmv <- tween_density(cmv, st_bbox(nrg$dem))
nrg <- tween_density(nrg, st_bbox(cmv$dem))

cmv$density_tween <- cmv$density_tween |> 
  left_join(cmv$phases, by = join_by(phase == phase_id))

nrg$density_tween <- nrg$density_tween |> 
  left_join(nrg$phases, by = join_by(phase == phase_id))

cmv_image_files <- make_plot(cmv, bb8 = st_bbox(nrg$dem))
nrg_image_files <- make_plot(nrg, bb8 = st_bbox(cmv$dem))

gifski(
  cmv_image_files,
  gif_file = here("figures", "cmv-density.gif"), 
  width = 300 * 9, # N dots = X dots-per-inch * Y inches
  height = 300 * 5, 
  delay = 0.1, 
  loop = FALSE
)

gifski(
  nrg_image_files, 
  gif_file = here("figures", "nrg-density.gif"), 
  width = 300 * 9,
  height = 300 * 5, 
  delay = 0.1, 
  loop = FALSE
)

cmv_image_files |> 
  image_read() |> 
  image_extent(geometry_area(2550, 1444)) |> 
  image_write_video(
    path = here("figures", "cmv-density.mp4"),
    framerate = 10
  )

nrg_image_files |> 
  image_read() |> 
  image_extent(geometry_area(2550, 1444)) |> 
  image_write_video(
    path = here("figures", "nrg-density.mp4"),
    framerate = 10
  )

