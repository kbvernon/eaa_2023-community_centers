# get elevation data

library(FedData)
library(here)
library(purrr)
library(sf)
library(terra)

download_dem <- function(region, geometry){
  
  r <- geometry |> 
    st_sfc(crs = 4326) |> 
    get_ned(res = "1", label = region) |> 
    rast() |> 
    aggregate(fact = 3)
  
  names(r) <- region
  
  return(r)
  
}

write_dem <- function(x){
  
  fn <- here("data", paste0("dem-", names(x), ".tif"))
  
  writeRaster(x, filename = fn, overwrite = TRUE)
  
}

# for some reason, values aren't read in after downloading the first time,
# so have to call this twice
here("data", "community-centers.gpkg") |> 
  read_sf(layer = "regions") |> 
  pmap(download_dem)

here("data", "community-centers.gpkg") |> 
  read_sf(layer = "regions") |> 
  pmap(download_dem) |> 
  walk(write_dem)