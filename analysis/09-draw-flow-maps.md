Flow Mapping
================
Qingqing Chen
Last compiled date: 18 September, 2021

<style>
body {text-align: justify}
</style>

Flows into or out of a neighbourhood can happen in all cardinal
directions and with different intensities and distances. Visualizing the
entire network of these connections is not very insightful for making
neighbourhood-level inferences. This is why we create a single metric
that summarizes the overall direction and strength of a flow to or from
a neighbourhood, inspired by Tobler’s (1981) work on vector fields to
display flow patterns.

We first aggregate the sectors of a grid into four zones, with each zone
corresponding to a specific angle and vector, relative to ‘East’ (0°):
the North East (NE) zone (45°), North West (NW) zone (135°), South West
(SW) zone (225°), and South East (SE) zone (315°). This simplification
alleviates computational complexity while maintaining the overall flow
orientation of a neighbourhood. For each vector, its direction
represents where visitors come from or where locals go out to, and its
magnitude indicates the number of people in that flow. The resultant
vector - the sum of the four vectors - is used to represent the overall
flow direction of each grid. To visualize all resulting vectors
uniformly on a single map, we log-transform the length of each vector
(to prevent some arrows from being much longer than others).
Furthermore, we also represent the number of users active in the grid
with the width of the resultant vector arrows.

## Load data

``` r
# sg boundary
sg_boundary <- readRDS(here("data/derived_data/sg_boundary.rds"))
# hexagonal grids 
grids <- read_sf(here("data/derived_data/spatial_hex_grid.shp"), quiet = T) %>%
  st_transform(crs = 3414)
# grid centroids 
grid_centroids <- grids %>% st_centroid()

# divided sectors 
grid_sectors <- readRDS(here("data/derived_data/grid_sectors.rds"))

# inflow and outflow distance
dist_visitors <- readRDS(here("data/derived_data/dist_visitor_points.rds"))
dist_locals <- readRDS(here("data/derived_data/dist_local_points.rds"))

# inflow and outflow diversity
inflow_diversity <- readRDS(here("data/derived_data/inflow_diversity.rds"))
outflow_diversity <- readRDS(here("data/derived_data/outflow_diversity.rds")) 
```

## Vector flow analysis

### Inflow vectors

``` r
if(file.exists(here("data/derived_data/inflow_vectors.rds"))){
  inflow_vectors <- readRDS(here("data/derived_data/inflow_vectors.rds"))
}else{
  dist_visitors_nest <- dist_visitors %>% 
  nest(data = c(u_id, home, created_at, dist_hm2grid, dist_hm2grid_km))
  inflow_vectors <- do.call(bind_rows, map2(dist_visitors_nest$grid_id, dist_visitors_nest$data, with_progress(cal_inflow_vectors)))
  saveRDS(inflow_vectors, file = here("data/derived_data/inflow_vectors.rds"))
}

# scale inflow vectors
inflow_vectors_scaled <- inflow_vectors %>% 
  mutate(magnitude_rescaled = scales::rescale(log(magnitude))) %>% 
  mutate(start_x = X + 1 + magnitude_rescaled * 1000 * cos(angle),
         start_y = Y + 1 + magnitude_rescaled * 1000 * sin(angle),
         total_user = scales::rescale(log(total_user)))
head(inflow_vectors_scaled)
```

    ## # A tibble: 6 × 9
    ##       X      Y grid_id angle magnitude total_user magnitude_rescaled start_x
    ##   <dbl>  <dbl>   <dbl> <dbl>     <dbl>      <dbl>              <dbl>   <dbl>
    ## 1 3043. 25492.       8 0.785      12        0.140              0.284   3244.
    ## 2 3418. 26141.      14 0.785      15        0.168              0.309   3637.
    ## 3 7168. 33935.     123 0.620      12.2      0.159              0.285   7401.
    ## 4 7543. 34585.     135 0.574      28.6      0.272              0.383   7865.
    ## 5 7918. 33935.     147 0.700      35.1      0.286              0.406   8229.
    ## 6 7918. 35234.     148 0.537      81.5      0.407              0.502   8350.
    ## # … with 1 more variable: start_y <dbl>

### Outflow vectors

``` r
if(file.exists(here("data/derived_data/outflow_vectors.rds"))){
  outflow_vectors <- readRDS(here("data/derived_data/outflow_vectors.rds"))
}else{
  dist_locals_nest <- dist_locals %>% 
  nest(data = c(u_id, grid_id, created_at, dist_hm2grid, dist_hm2grid_km)) 
  outflow_vectors <- do.call(bind_rows, map2(dist_locals_nest$home, dist_locals_nest$data, with_progress(cal_outflow_vectors)))
  saveRDS(outflow_vectors, file = here("data/derived_data/outflow_vectors.rds"))
}

# scale outflow vectors 
outflow_vectors_scaled <- outflow_vectors %>%
  mutate(magnitude_rescaled = scales::rescale(log(magnitude)),
         end_x = X + 1 + magnitude_rescaled * 1000 * cos(angle),
         end_y = Y + 1 + magnitude_rescaled * 1000 * sin(angle),
         total_user = scales::rescale(log(total_user)))
head(outflow_vectors_scaled)
```

    ## # A tibble: 6 × 9
    ##        X      Y home_id   angle magnitude total_user magnitude_rescaled  end_x
    ##    <dbl>  <dbl>   <dbl>   <dbl>     <dbl>      <dbl>              <dbl>  <dbl>
    ## 1 10168. 35234.     234  0.0891      667.      0.477              0.592 10758.
    ## 2 11293. 35884.     286 -0.0182      777.      0.515              0.618 11912.
    ## 3 11293. 37183.     287 -0.374      1020.      0.560              0.666 11914.
    ## 4 11668. 36533.     304 -0.214       567.      0.437              0.563 12219.
    ## 5 12043. 35884.     321 -0.103      4729.      0.914              0.937 12976.
    ## 6 12043. 38482.     323 -0.201       491.      0.414              0.538 12570.
    ## # … with 1 more variable: end_y <dbl>

## Flow map visualization

### Background preparation

#### Background: Travel distance

``` r
# mean inflow distance 
bg_dist_inflow <- dist_visitors %>% 
  group_by(grid_id, u_id) %>% 
  dplyr::summarise(mean_dist_user = mean(dist_hm2grid_km)) %>% 
  group_by(grid_id) %>% 
  dplyr::summarise(mean_dist_grid = mean(mean_dist_user),
                   sd_dist_grid = sd(mean_dist_user)) %>% 
  mutate(cut_mean_dist_grid = cut(mean_dist_grid, 
                                  breaks=c(quantile(mean_dist_grid, probs = seq(0, 1, by = 0.20))), include.lowest=TRUE), 
         cut_sd_dist_grid = cut(sd_dist_grid, 
                                breaks = c(quantile(sd_dist_grid, probs = seq(0, 1, by = 0.2))), include.lowest = TRUE)) %>% 
  left_join(., grids) %>% 
  st_as_sf()

# mean outflow distance 
bg_dist_outflow <- dist_locals %>% 
  group_by(home, u_id) %>% 
  dplyr::summarise(mean_dist_user = mean(dist_hm2grid_km)) %>% 
  group_by(home) %>% 
  dplyr::summarise(mean_dist_grid = mean(mean_dist_user), 
                   sd_dist_grid = sd(mean_dist_user)) %>% 
  mutate(cut_mean_dist_grid = cut(mean_dist_grid, 
                                  breaks=c(quantile(mean_dist_grid, probs = seq(0, 1, by = 0.20))), include.lowest=TRUE), 
         cut_sd_dist_grid = cut(sd_dist_grid, 
                                breaks = c(quantile(sd_dist_grid, probs = seq(0, 1, by = 0.2))), include.lowest = TRUE)) %>% 
  left_join(., grids, by = c("home" = "grid_id")) %>% 
  st_as_sf()
```

#### Background: Neighbourhood ratio

``` r
if(file.exists(here("data/derived_data/neigh_ratio_inflow.rds"))){
  bg_neighRatio_inflow <- readRDS(here("data/derived_data/neigh_ratio_inflow.rds")) %>% 
    dplyr::select(grid_id, type, ratio_divide_sumWeigh) %>% 
    na.omit() %>% 
    spread(key = "type", value = "ratio_divide_sumWeigh") %>% 
    replace(., is.na(.), 0) %>% 
    left_join(., grids) %>% 
    st_as_sf(crs = 3414) %>% 
    mutate(ratio = cut(inner, breaks=c(quantile(inner, probs = seq(0, 1, by = 0.20))), include.lowest=TRUE))
}else{
  dist_visitors_weight <- dist_visitors %>% 
  dplyr::select(grid_id, u_id, home, created_at) %>% 
  group_by(u_id) %>% 
  mutate(total_tweets = n(), weight = 1/total_tweets) %>% 
  ungroup()
  
  neigh_ratio_inflow <- do.call(rbind, map(dist_visitors_nest$grid_id, with_progress(function(x) cal_neigh_ratio(dist_visitors_weight, x, user_type = "visitor"))))
  saveRDS(neigh_ratio_inflow, file = here("data/derived_data/neigh_ratio_inflow.rds"))
}

if(file.exists(here("data/derived_data/neigh_ratio_outflow.rds"))){
  bg_neighRatio_outflow <- readRDS(here("data/derived_data/neigh_ratio_outflow.rds")) %>% 
    dplyr::select(home, type, ratio_divide_sumWeigh) %>% 
    na.omit() %>% 
    spread(key = "type", value = "ratio_divide_sumWeigh") %>% 
    replace(., is.na(.), 0) %>% 
    left_join(., grids, by = c("home" = "grid_id")) %>% 
    st_as_sf(crs = 3414) %>% 
    mutate(ratio = cut(inner, breaks=c(quantile(inner, probs = seq(0, 1, by = 0.20))), include.lowest=TRUE))
}else{
  dist_locals_weight <- dist_locals %>% 
  dplyr::select(home, u_id, grid_id, created_at) %>% 
  group_by(u_id) %>% 
  mutate(total_tweets = n(), weight = 1/total_tweets) %>% 
  ungroup()
  
  neigh_ratio_outflow <- do.call(rbind, map(dist_locals_nest$home, with_progress(function(x) cal_neigh_ratio(dist_locals_weight, x, user_type = "local"))))
  saveRDS(neigh_ratio_outflow, file = here("data/derived_data/neigh_ratio_outflow.rds"))
}
```

#### Background: Diversity

``` r
bg_div_inflow <-  inflow_diversity %>% 
  dplyr::select(grid_id, norm_div_shannon) %>% 
   mutate(cut_div = cut(norm_div_shannon, breaks=c(quantile(norm_div_shannon, probs = seq(0, 1, by = 0.20))), include.lowest=TRUE))

bg_div_outflow <-  outflow_diversity %>% 
  dplyr::select(grid_id, norm_div_shannon) %>% 
  mutate(cut_div = cut(norm_div_shannon, breaks=c(quantile(norm_div_shannon, probs = seq(0, 1, by = 0.20))), include.lowest=TRUE))
```

### Flow maps

#### Inflow with neighbourhood ratio background

``` r
viz_flows(bg_neighRatio_inflow, inflow_vectors_scaled,
          bg.fill.var = "ratio", 
          quantile.var = "inner",
          palette = "OrRd",
          start.x = "start_x", start.y = "start_y", 
          end.x = "X", end.y = "Y", 
          title = expression(paste("(a) ", bold("Direction of Flows: "), "background is neighbourhood ratio")), 
          legend.nm = "Neighborhood ratio")
```

<img src="09-draw-flow-maps_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

#### Outflow with neighbourhood ratio background

``` r
viz_flows(bg_neighRatio_outflow, outflow_vectors_scaled, 
          quantile.var = "inner",
          palette = "Purples",
          bg.fill.var = "ratio", 
          start.x = "X", start.y = "Y", 
          end.x = "end_x", end.y = "end_y",
          title = expression(paste("(a) ", bold("Direction of Flows: "), "background is neighbourhood ratio")), 
          legend.nm = "Neighborhood ratio")
```

<img src="09-draw-flow-maps_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

#### Inflow with average distance background

``` r
viz_flows(bg_dist_inflow, inflow_vectors_scaled,
          bg.fill.var = "cut_mean_dist_grid",
          quantile.var = "mean_dist_grid",
          palette = "OrRd",
          start.x = "start_x", start.y = "start_y",
          end.x = "X", end.y = "Y",
          title = expression(paste("(b) ", bold("Direction of Flows: "), "background is average incoming distance")),
          legend.nm  = "Avg.distance (km)")
```

<img src="09-draw-flow-maps_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

### Outflow average distance background

``` r
viz_flows(bg_dist_outflow, outflow_vectors_scaled, 
          bg.fill.var = "cut_mean_dist_grid", 
          quantile.var = "mean_dist_grid", 
          palette = "Purples",
          start.x = "X", start.y = "Y", 
          end.x = "end_x", end.y = "end_y", 
          title = expression(paste("(b) ", bold("Direction of Flows: "), "background is average outgoing distance")),
          legend.nm = "Avg.distance (km)")
```

<img src="09-draw-flow-maps_files/figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

#### Inflow with S.D.distance background

``` r
viz_flows(bg_dist_inflow, inflow_vectors_scaled, 
          bg.fill.var = "cut_sd_dist_grid", 
          quantile.var = "sd_dist_grid",
          palette = "OrRd",
          start.x = "start_x", start.y = "start_y", 
          end.x = "X", end.y = "Y", 
          title = expression(paste("(c) ", bold("Direction of Flows: "), "background is S.D. of incoming distance")),
          legend.nm = "S.D.distance(km)")
```

<img src="09-draw-flow-maps_files/figure-gfm/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

#### Outflow with S.D.distance background

``` r
viz_flows(bg_dist_outflow, outflow_vectors_scaled, 
          bg.fill.var = "cut_sd_dist_grid", 
          quantile.var = "sd_dist_grid",
          palette = "Purples",
          start.x = "X", start.y = "Y", 
          end.x = "end_x", end.y = "end_y", 
          title = expression(paste("(c) ", bold("Direction of Flows: "), "background is S.D. of outgoing distance")),
          legend.nm = "S.D.distance(km)")
```

<img src="09-draw-flow-maps_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

#### Inflow with diversity background

``` r
viz_flows(bg_div_inflow %>% filter(grid_id %in% inflow_vectors_scaled$grid_id), inflow_vectors_scaled,
          bg.fill.var = "cut_div", 
          quantile.var = "norm_div_shannon",
          palette = "OrRd",
          start.x = "start_x", start.y = "start_y", 
          end.x = "X", end.y = "Y", 
          title = expression(paste("(d) ", bold("Direction of Flows: "), "background is normalized inflow diversity")),
          legend.nm = "Norm.diversity")
```

<img src="09-draw-flow-maps_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

#### Outflow with diversity background

``` r
viz_flows(bg_div_outflow %>% filter(grid_id %in% outflow_vectors_scaled$home_id),  outflow_vectors_scaled,
          bg.fill.var = "cut_div", 
          quantile.var = "norm_div_shannon",
           palette = "Purples",
          start.x = "X", start.y = "Y", 
          end.x = "end_x", end.y = "end_y", 
          title = expression(paste("(d) ", bold("Direction of Flows: "), "background is normalized outflow diversity")),
          legend.nm = "Norm.diversity")
```

<img src="09-draw-flow-maps_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />
