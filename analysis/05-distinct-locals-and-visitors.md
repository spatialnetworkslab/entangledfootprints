Distinct locals and visitors
================
Qingqing Chen
Last compiled date: 18 September, 2021

Locals are users with homes within that specific location or hexagonal
cell. Conversely, visitors are all other users with homes outside of the
cell.

## Load data

``` r
# load aggregated grids 
grids <- read_sf(here("data/derived_data/spatial_hex_grid.shp"), quiet = T) %>% 
  st_transform(crs = 3414)

# load twitter dataset 
df <- readRDS(here("data/derived_data/deidentified_sg_tweets_updated.rds")) 

# load identified home locations 
identified_hms <- read_csv(here("data/derived_data/identified_hms.csv"))
```

## Distinct locals and visitors

We only consider users with identified home locations. For each grid, a
user is assigned as ‘local’ or ‘visitor’ according to his/her home
location.

-   **Local**: If the visited grid is the same as the home location of
    the user, the user is assigned as ‘local’.
-   **Visitor**: If the visited grid is different from the home location
    of the user, the user is assigned as ‘visitor’.

The distinct users in grids are saved under `data/derived_data/`
directory.

``` r
# users in each of grids 
users_in_grids <- df %>% dplyr::select(-created_at) %>% distinct(u_id, grid_id)

# distinct users 
distinct_users_in_grid <- function(df_users_in_grids, df_home, grid_index){
  output <- df_users_in_grids %>% 
    filter(grid_id == grid_index) %>% 
    left_join(., df_home) %>% 
    drop_na() %>% #remove users without identified home location
    mutate(type = if_else(grid_id == home, "local", "visitor"))
  return(output)
}

if(file.exists(here("data/derived_data/distinct_users.rds"))){
  distinct_users <- readRDS(here("data/derived_data/distinct_users.rds"))
}else{
  distinct_users <- do.call(bind_rows, map(grids$grid_id, with_progress(function(x) distinct_users_in_grid(users_in_grids, identified_hms, x))))
  saveRDS(distinct_users, file = here("data/derived_data/distinct_users.rds"))
}

head(distinct_users)
```

    ## # A tibble: 6 × 4
    ##       u_id grid_id  home type   
    ##      <dbl>   <dbl> <dbl> <chr>  
    ## 1 82965004       3  1594 visitor
    ## 2 90218329       3   626 visitor
    ## 3 49010226       3  1461 visitor
    ## 4 37768255       4  1158 visitor
    ## 5 37716383       8  1387 visitor
    ## 6 12470940       8   442 visitor
