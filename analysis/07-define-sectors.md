Creating concentric sectors
================
Qingqing Chen
Last compiled date: 18 September, 2021

<style>
body {text-align: justify}
</style>

## Calculate centroids of hexgonal grids

The grid centroids are saved under `data/derived_data/` directory.

``` r
#aggregated grids 
grids <- read_sf(here("data/derived_data/spatial_hex_grid.shp"), quiet = T) %>% 
  st_transform(crs = 3414)

#aggregated grid centroids 
grid_centroids <- grids %>% st_centroid()
```

``` r
# visualize the centroids
tm_shape(grids) +
  tm_borders(col = "grey") +
  tm_shape(grids %>% filter(grid_id == 1594)) +
  tm_polygons(col = "red") +
  tm_shape(grid_centroids) +
  tm_dots(col = "black") +
  tm_layout(frame = FALSE) + 
  tm_compass(type = "8star", size = 2, position = c(0, 0.05)) +
  tm_scale_bar(position = c(0, 0.001))
```

<img src="07-define-sectors_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

## Create buffers

To create sectors around the centroid of a grid cell, we first draw
eight buffers with a radius of 1km, 3km, 5km, 7km, 10km, 20km, 30km, and
60km. The starting point - 1km - is chosen to roughly correspond to the
walking distance of a neighborhood and the final radius - 60km - is
chosen to cover the entirety of Singapore.

The created grid buffers are saved under `data/derived_data/` directory.

``` r
# buffer radius
radius <- c(1000, 3000, 5000, 7000, 10000, 20000, 30000, 60000)

## draw buffers 
draw_buffers <- function(df_centroids, radius, grid_index){
  grid_centroid <- df_centroids %>% filter(grid_id == grid_index)
  buffers <- list()
  for (i in 1:length(radius)){
    if(i == 1){
      buffers[[i]] <- grid_centroid %>% 
        st_buffer(., dist = radius[1]) %>% 
        mutate(radius = radius[1])
    } else{
      buffers[[i]] <- st_difference(
        grid_centroid %>% st_buffer(., dist = radius[i]),
        grid_centroid %>% st_buffer(., dist = radius[i-1])) %>% 
        dplyr::select(-grid_id.1) %>% 
        mutate(radius = radius[i])
    }
  }
  do.call(rbind, buffers)
}

if(file.exists(here("data/derived_data/grid_buffers.rds"))){
  grid_buffers <- readRDS(here("data/derived_data/grid_buffers.rds"))
}else{
  grid_buffers <- do.call(rbind, map(grid_centroids$grid_id, with_progress(function(x) draw_buffers(grid_centroids, radius, x))))
  saveRDS(grid_buffers, file = here("data/derived_data/grid_buffers.rds"))
}
```

``` r
## plot one grid buffer as an example 
buffer_example <- grid_buffers %>% 
  filter(grid_id == 1594) %>% 
  mutate(radius = radius/1000,
         radius = paste0(radius, "km"), 
         radius = factor(radius, levels = c("1km", "2km", "3km", "5km", "7km", "10km", "20km", "30km", "60km")))
  
tm_shape(grids) +
  tm_polygons(col = "white", alpha = 0.5, border.col = "grey") +
  tm_shape(buffer_example) +
  tm_borders(col = "purple") +
  # tm_polygons(col = "radius", title = "Radius", alpha = 0.8) +
  tm_shape(grids %>% filter(grid_id == 1594)) + 
  tm_polygons(col = "red") + 
  tm_shape(grids %>% filter(grid_id == 1594)) + 
  tm_dots(col = "black") +
  tm_layout(legend.outside = T)
```

<img src="07-define-sectors_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

## Create cirlce sectors

Apart from distance alone, users coming from different directions (e.g.,
north, south, east, west, or center) in the same distance band may
represent distinct demographic or neighborhood backgrounds. As such, we
cut each buffer into four directions (???top-left???, ???top-right???,
???bottom-left???, and ???bottom-right???) by two lines that meet at a right
angle (i.e., 90 degrees) at the center of the grid cell. We consider
users within the walking distance buffer (1km) to all belong to the same
neighborhood, so refrain from cutting the first inner buffer. As such,
the total number of created sectors for each grid cell can be calculated
as *4 \* (n-1) + 1*, where n is the number of buffers used. In our case,
we create a total of 29 circular sectors for a single grid cell and do
so for all grid cells.

The created grid sectors are saved under `data/derived_data/` directory.

``` r
##cut single buffer
cut_buffer <- function(buffer, buffer_id, blades, grid_index){
  lwgeom::st_split(st_geometry(buffer[buffer_id, ]), blades) %>%
    st_collection_extract("POLYGON") %>%
    st_sf() %>%
    mutate(grid_id = grid_index) %>%
    dplyr::select(grid_id) 
}

get_cut_buffer <- function(df_centroids, df_buffers, shift, grid_index){
  # get input grid centroid
  centroid <- df_centroids %>% 
    filter(grid_id == grid_index) %>% 
    st_coordinates() %>%
    as_tibble() %>%
    set_names(c("lon", "lat")) # convert geometry to lon and lat
  # create blades
  blades <- st_linestring(
    rbind(c(centroid$lon+shift, centroid$lat),
          c(centroid$lon-shift, centroid$lat),
          c(centroid$lon, centroid$lat),
          c(centroid$lon, centroid$lat+shift),
          c(centroid$lon, centroid$lat-shift))) %>%
    st_sfc(., crs = 3414)
  # get buffer for input grid 
  buffer <- df_buffers %>% filter(grid_id == grid_index)
  buffer1 <- buffer[1, ] %>% dplyr::select(grid_id) 
  buffer <- buffer[-1, ] ## do not cut the first inner buffer
  buffer_ids <- 1:nrow(buffer)
  # process all buffers 
  rbind(buffer1, do.call(rbind, map(buffer_ids, function(x) cut_buffer(buffer, x, blades, grid_index)))) %>%
    rowid_to_column(var = "sector_id") 
}

# process all grids 
if(file.exists(here::here("data/derived_data/grid_sectors.rds"))){
  grid_sectors <- readRDS(here::here("data/derived_data/grid_sectors.rds"))
}else{
  grid_sectors <- do.call(rbind, map(grid_centroids$grid_id, with_progress(function(x) get_cut_buffer(grid_centroids, grid_buffers, shift = 60000, x))))
  saveRDS(grid_sectors, file = here::here("data/derived_data/grid_sectors.rds"))
}
```

## Visualize sectors

### Show case: sectors of grid 1594

``` r
## plot one cut buffer as an example
## users with home locations in the same circle sector are identified as the same species
grid_sectors_example <- grid_sectors %>% 
  filter(grid_id == 1594) %>% 
  mutate(sector_id = factor(sector_id))

distinct_users <- readRDS(here("data/derived_data/distinct_users.rds"))
users_grid1594 <- distinct_users %>% 
  filter(grid_id == 1594) %>% 
  filter(type == "visitor") %>% 
  left_join(., grids, by =c("home" = "grid_id")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 3414) %>% 
  st_centroid() %>% 
  st_join(., grid_sectors_example, largest = T)


tm_shape(grids) +
  tm_polygons(col = "white", alpha = 0.5, border.col = "grey") +
  tm_shape(grids %>% filter(grid_id == 1594)) +
  tm_polygons(col = "red") +  ## target grid 
  tm_shape(users_grid1594) +
  tm_dots(col = "sector_id", jitter = 0.08, size = 0.015, palette = "Dark2") +
  tm_shape(grid_sectors_example) +
  tm_borders(col = "purple") +
  tm_text(text = "sector_id", size = 0.6, col = "black") +
  tm_layout(legend.show = FALSE)
```

<img src="07-define-sectors_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

### Show case: sectors within Singapore boundary of grid 1594

``` r
sectors_inSG <- grid_sectors_example %>% 
  st_intersection(grids, .) %>% 
  group_by(sector_id) %>% 
  summarise() 


tm_shape(grids) +
  tm_polygons(col = "white", alpha = 0.1, border.col = "grey") +
  tm_shape(sectors_inSG) +
  tm_polygons(col = "sector_id", border.col = "purple", alpha = 0.9) +
  tm_shape(grids %>% filter(grid_id == 1594)) +
  tm_polygons(col = "red") +  ## target grid 
  tm_shape(users_grid1594) +
  tm_dots(col = "sector_id", jitter = 0.08, size = 0.015, palette = "Dark2") +
  # tm_dots(col = "black", jitter = 0.08, size = 0.015) +
  tm_shape(grid_sectors_example) +
  tm_borders(col = "purple", lty = 2) +
  tm_text(text = "sector_id", size = 0.6, col = "black") + 
  tm_layout(legend.show = FALSE)
```

<img src="07-define-sectors_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

### Show case: circle sector 1 of grid 1594

``` r
tm_shape(grid_sectors_example %>% filter(sector_id %in% seq(1))) +
  tm_polygons(col = "purple", alpha = 0.1) +
  tm_shape(users_grid1594 %>% filter(sector_id == 1)) +
  tm_dots(col = "#1B9E77", jitter = 0.2, size = 0.2) +
  tm_shape(grids %>% filter(grid_id == 1594)) +
  tm_polygons(col = "red") +
  tm_shape(grids) +
  tm_borders(col = "grey") +
  tm_layout(frame = F)
```

<img src="07-define-sectors_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

### Show case: circle sector 5 of grid 1594

``` r
tm_shape(grid_sectors_example %>% filter(sector_id %in% c(5))) +
  tm_polygons(col = "purple", alpha = 0.1, border.col = "purple") +
  tm_shape(users_grid1594 %>% filter(sector_id == 5)) +
  tm_dots(col = "#D95F02", jitter = 0.1, size = 0.2) +
  tm_shape(grids) +
  tm_borders(col = "grey") +
  tm_shape(grids %>% filter(grid_id == 1594)) +
  tm_polygons(col = "red") +
  tm_layout(frame = F)
```

<img src="07-define-sectors_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />
