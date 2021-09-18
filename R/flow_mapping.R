# ------------------------------------------------------------------------------
# calculate vector flows
vector_components <- function(angle, magnitude) {
  radians <- angle * pi / 180
  x <- magnitude * cos(radians)
  y <- magnitude * sin(radians)
  return(list(x = x, y = y))
}

vector_addition <- function(x_vector, y_vector) {
  tot_x <- sum(x_vector)
  tot_y <- sum(y_vector)
  magnitude <- sqrt(tot_x^2 + tot_y^2)
  angle <- atan2(tot_y, tot_x)
  return(c(angle, magnitude))
}

# inflow vectors
cal_inflow_vectors <- function(grid_index, data){
  # group sectors to 4 regions
  grid_region <- grid_sectors %>%
    filter(grid_id == grid_index) %>%
    mutate(region = case_when(
      sector_id %in% c(2, 6, 10, 14, 18, 22, 26) ~ "South East",
      sector_id %in% c(3, 7, 11, 15, 19, 23, 27) ~ "South West",
      sector_id %in% c(4, 8, 12, 16, 20, 24, 28) ~ "North West",
      sector_id %in% c(5, 9, 13, 17, 21, 25, 29) ~ "North East",
      TRUE ~ "Within 1km buffer"
    )) %>%
    group_by(region) %>%
    dplyr::summarise()  %>%
    mutate(angle = case_when(
      region == "South East" ~ 315,
      region == "South West" ~ 225,
      region == "North West" ~ 135,
      region == "North East" ~ 45
    )) %>%
    filter(angle > 0)
  # coordinate of visited grid
  coord_visited_grid <- grids %>%
    filter(grid_id == grid_index) %>%
    st_centroid() %>%
    st_coordinates() %>%
    as_tibble()
  # users in regions
  region_vectors <- data %>%
    group_by(home) %>%
    summarise(n_user = n_distinct(u_id)) %>%
    left_join(., grid_centroids, by = c("home" = "grid_id")) %>% # assign homes to sectors
    st_as_sf() %>%
    st_join(., grid_region) %>%
    st_set_geometry(NULL) %>%
    group_by(region, angle) %>%
    summarise(n_user = sum(n_user)) %>%
    ungroup() %>%
    filter(angle > 0)
  components <- vector_components(region_vectors$angle, region_vectors$n_user)
  resulting_vector <- vector_addition(components$x, components$y)
  coord_visited_grid %>%
    mutate(grid_id = grid_index,
           angle = resulting_vector[1],
           magnitude = resulting_vector[2],
           total_user = sum(region_vectors$n_user))
}

# outflow vectors
cal_outflow_vectors <- function(home_index, data){
  # group sectors to 4 regions
  grid_region <- grid_sectors %>%
    filter(grid_id == home_index) %>%
    mutate(region = case_when(
      sector_id %in% c(2, 6, 10, 14, 18, 22, 26) ~ "South East",
      sector_id %in% c(3, 7, 11, 15, 19, 23, 27) ~ "South West",
      sector_id %in% c(4, 8, 12, 16, 20, 24, 28) ~ "North West",
      sector_id %in% c(5, 9, 13, 17, 21, 25, 29) ~ "North East",
      TRUE ~ "Within 1km buffer"
    )) %>%
    group_by(region) %>%
    dplyr::summarise()  %>%
    mutate(angle = case_when(
      region == "South East" ~ 315,
      region == "South West" ~ 225,
      region == "North West" ~ 135,
      region == "North East" ~ 45
    )) %>%
    filter(angle > 0)

  # coordinate of home grid
  coord_home_grid <- grids %>%
    filter(grid_id == home_index) %>%
    st_centroid() %>%
    st_coordinates() %>%
    as_tibble()

  # users in regions
  region_vectors <- data %>%
    group_by(grid_id) %>%
    summarise(n_user = n_distinct(u_id)) %>%
    left_join(., grid_centroids, by = c("grid_id" = "grid_id")) %>% # assign homes to sectors
    st_as_sf() %>%
    st_join(., grid_region) %>%
    st_set_geometry(NULL) %>%
    group_by(region, angle) %>%
    summarise(n_user = sum(n_user)) %>%
    ungroup() %>%
    filter(angle > 0)


  # calculate user region coordinates
  components <- vector_components(region_vectors$angle, region_vectors$n_user)
  resulting_vector <- vector_addition(components$x, components$y)
  coord_home_grid %>%
    mutate(home_id = home_index,
           angle = resulting_vector[1],
           magnitude = resulting_vector[2],
           total_user = sum(region_vectors$n_user))
}




# ------------------------------------------------------------------------------
# calculate neighborhood ratio: treshold 3km
cal_neigh_ratio <- function(df_tweet_weight, grid_index, user_type){
  # separate inner ring and outer ring - 3km
  rings <- grid_sectors %>%
    filter(grid_id == grid_index) %>%
    mutate(type = case_when(
      sector_id %in% seq(1, 5, 1) ~ "inner",
      TRUE ~ "outer")
    ) %>%
    group_by(type) %>%
    summarise() %>%
    ungroup()
  # calculate weight
  if(user_type == "visitor"){
    df_tweet_weight %>%
      filter(grid_id == grid_index) %>%
      left_join(., grid_centroids, by = c("home" = "grid_id")) %>%
      st_as_sf(crs = 3414) %>%
      st_join(., rings) %>%
      st_set_geometry(NULL) %>%
      mutate(total_weight = sum(weight),
             n_user = n_distinct(u_id)) %>%
      group_by(grid_id, type, total_weight, n_user) %>%
      dplyr::summarise(type_weight = sum(weight)) %>%
      ungroup() %>%
      mutate(ratio_divide_sumWeigh = type_weight/total_weight) %>%
      mutate(ratio_divide_users = type_weight/n_user)
  }else{
    df_tweet_weight %>%
      filter(home == grid_index) %>%
      left_join(., grids_centroids, by = c("grid_id" = "grid_id")) %>%
      st_as_sf(crs = 3414) %>%
      st_join(., rings) %>%
      st_set_geometry(NULL) %>%
      mutate(total_weight = sum(weight),
             n_user = n_distinct(u_id)) %>%
      group_by(home, type, total_weight, n_user) %>%
      dplyr::summarise(type_weight = sum(weight)) %>%
      ungroup() %>%
      mutate(ratio_divide_sumWeigh = type_weight/total_weight) %>%
      mutate(ratio_divide_users = type_weight/n_user)
  }
}


# ------------------------------------------------------------------------------
# visualize flow maps
viz_flows <- function(df_bg, df_flow, bg.fill.var, quantile.var,
                      start.x, start.y, end.x, end.y, title, legend.nm,
                      palette){
  bg.fill.var <- rlang::sym(bg.fill.var)
  start.x <- rlang::sym(start.x)
  start.y <- rlang::sym(start.y)
  end.x <- rlang::sym(end.x)
  end.y <- rlang::sym(end.y)

  # prepare legend
  quantile.var <- rlang::sym(quantile.var)
  quantiles <- quantile(df_bg %>% pull({{quantile.var}}), probs = seq(0, 1, by = 0.20)) %>%
    as.numeric() %>% round(., 2)
  quantile_labels <- paste(quantiles[1:5], "to", quantiles[2:6])

  # basic map: background map
  basic.plot <- ggplot() +
    geom_sf(data = sg_boundary, color = "grey", fill = "white", alpha = 0.1) +
    geom_sf(data = df_bg, aes(fill = {{bg.fill.var}}), lwd = 0) +
    scale_fill_brewer(palette = palette, direction = "horizontal", labels = quantile_labels) +
    guides(fill = guide_legend(nrow=1,
                               override.aes = list(alpha = 1),
                               label.position="bottom",
                               label.hjust = 0))
  # add flow vectors
  basic.plot +
    geom_curve(data = df_flow %>% filter(magnitude_rescaled > 0),
               aes(x = {{start.x}}, y = {{start.y}}, xend = {{end.x}}, yend = {{end.y}},
                   size = total_user * 0.5), arrow = arrow(length = unit(0.1,"cm")), curvature = 0.1) +
    scale_size_identity() +
    coord_sf(datum = NA) +
    annotation_north_arrow(location = "bl",
                           which_north = "true",
                           height = unit(0.7, "cm"),
                           width = unit(0.4, "cm"),
                           pad_x = unit(0.03, "in"),
                           pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", width_hint = 0.2, height = unit(0.07, "cm")) +
    labs(x = "", y = "", title = title, fill = legend.nm) +
    theme_minimal() +
    theme(legend.position = c(0.76, 0.03),
          legend.spacing.x = unit(0, 'cm'),
          legend.text.align = 1,
          legend.text = element_text(margin = margin(r = 3, unit = "pt")),
          legend.key.height = unit(0.35, 'cm'),
          plot.caption = element_text(hjust = 0.02),
          plot.caption.position = "plot")
}
