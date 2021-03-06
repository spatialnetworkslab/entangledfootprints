Dataset information
================
Qingqing Chen
Last compiled date: 17 September, 2021

## Load data

``` r
df <- readRDS(here::here("data/derived_data/deidentified_sg_tweets_updated.rds")) 
sg_boundary <- readRDS(here("data/derived_data/sg_boundary.rds"))
streets <- readRDS(here("data/derived_data/streets.rds"))
area_hatched <- readRDS(here("data/derived_data/area_hatched.rds"))
area_centers <- readRDS(here("data/derived_data/area_centers.rds"))
grids <-  read_sf(here("data/derived_data/spatial_hex_grid.shp"), quiet = T) %>% 
  st_transform(crs = 3414)
```

## Visualization

``` r
sf_counts <- df %>% 
  group_by(grid_id) %>% 
  summarise(n_tweets = n(), 
            n_users = n_distinct(u_id)) %>% 
  left_join(., grids, by = c("grid_id" = "grid_id")) %>% 
  st_as_sf()
```

### Spatial distribution of tweets

``` r
tm_shape(sg_boundary) +
  tm_borders(col = "grey") + 
  tm_shape(sf_counts) +
  tm_fill("n_tweets",
          palette = "PuRd",
          breaks = c(0, 500, 1000, 5000, 10000, 30000, 50000, 100000, 200000, 250000),
          style = "fixed",
          alpha = 0.8,
          legend.is.portrait = F,
          legend.hist = TRUE,
          legend.format = list(text.align = "center"),
          title = "Number of tweets") + 
  tm_shape(streets) +
  tm_lines(col = rgb(80, 80, 80, maxColorValue = 255), lwd = 1, alpha = 0.8) +
  tm_shape(area_hatched) + 
  tm_lines(col = rgb(80, 110, 120, maxColorValue = 255), alpha = 0.8) +
  tm_layout(title.position = c("left", "top"),
            main.title = "Spatial distribution of tweets",
            main.title.size = 1.5,
            frame = F,
            legend.position = c("right", "bottom"),
            legend.bg.color = "white",
            legend.width = 0.4,
            legend.title.size  = 1.2,
            legend.text.size = 0.5,
            legend.hist.height = 0.27,
            legend.hist.width = 0.4,
            legend.hist.size = 0.6)
```

![](02-dataset-information_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Spatial distribution of users

``` r
tm_shape(sg_boundary) +
  tm_borders(col = "grey") + 
  tm_shape(sf_counts) +
    tm_fill("n_users",
            palette = "PuRd",
            breaks = c(0, 100, 300, 500, 1000, 3000, 5000, 10000, 30000, 50000),
            style = "fixed",
            alpha = 0.8,
            legend.is.portrait = F,
            legend.hist = TRUE,
            title = "Number of users") + 
  tm_shape(streets) +
  tm_lines(col = rgb(80, 80, 80, maxColorValue = 255), lwd = 1, alpha = 0.8) +
  tm_shape(area_hatched) + 
  tm_lines(col = rgb(80, 110, 120, maxColorValue = 255), alpha = 0.8) +
  tm_layout(title.position = c("left", "top"),
            main.title = "Spatial distribution of users",
            main.title.size = 1.5,
            frame = F,
            legend.position = c("right", "bottom"),
            legend.bg.color = "white",
            legend.width = 0.4,
            legend.title.size  = 1.2,
            legend.text.size = 0.5,
            legend.hist.height = 0.27,
            legend.hist.width = 0.4,
            legend.hist.size = 0.6)
```

![](02-dataset-information_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Distribution of tweets and users over time

``` r
counts_ym <- df %>%
  mutate(year_month = format(df$created_at, "%Y-%m")) %>% 
  group_by(year_month) %>% 
  dplyr::summarise(n_tweets = n(), 
                   n_users = n_distinct(u_id)) %>% 
  arrange(year_month)

counts_ym %>% 
  arrange(year_month) %>% 
  mutate(year_month = factor(year_month, levels = year_month)) %>% 
  gather(key = "key", value = "value", -year_month) %>% 
  mutate(key = recode(key, "n_tweets" = "Number of Tweets", "n_users" = "Number of Users")) %>% 
  ggplot() + 
  geom_bar(aes(x = year_month, y = value, fill = key), stat = "identity") +
  facet_wrap(~key, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(legend.position = "NULL", 
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Count", title = "Distribution of tweets and users from 2012 - 2016")
```

![](02-dataset-information_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
