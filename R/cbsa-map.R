library(here)
library(patchwork)
library(sf)
library(tigris)
library(tidyverse)

cbsa <- core_based_statistical_areas() |> 
  filter(str_detect(NAME, "Fargo|Tuscaloosa|Pittsburgh|New York")) |> 
  rename_with(tolower) |> 
  select(name) |> 
  mutate(type = "CBSA")

the_metro <- urban_areas() |> 
  st_filter(cbsa) |> 
  st_intersection(cbsa) |> 
  mutate(type = "Urban Core") |> 
  select(type) |> 
  st_join(cbsa[, "name"])

everything <- bind_rows(cbsa, the_metro) |> 
  group_by(name) |> 
  mutate(
    name = str_split_i(name, ",", 1),
    id = cur_group_id()
  ) |> 
  ungroup()
  
plot_cbsa <- function(x, ...){
  
  d <- everything |> filter(id == x)
  
  ggplot() +
    geom_sf(
      data = d,
      aes(fill = type),
      color = "#001524",
      linewidth = 0.1
    ) +
    scale_fill_manual(name = NULL, values = c("#15616D", "#FFECD1")) +
    labs(caption = unique(d$name)) +
    coord_sf(
      crs = 4326, 
      datum = 4326, 
      expand = FALSE
    ) +
    theme_void(12) +
    theme(
      legend.position = "none",
      plot.caption = element_text(
        size = rel(1), 
        hjust = 0.5, 
        color = "#495057",
        margin = margin(t = 8)
      ),
      plot.margin = margin(...)
    )
  
}

plot_cbsa(1, r = 24) +
  plot_cbsa(2, r = 24, l = 24) +
  plot_cbsa(3, r = 24, l = 24) +
  plot_cbsa(4, l = 24) +
  plot_layout(ncol = 4)

fn <- here("figures", "example-cbsa.png")

ggsave(
  filename = fn,
  width = 9.85, 
  height = 2.5,
  dpi = 600, 
  bg = "white"
)