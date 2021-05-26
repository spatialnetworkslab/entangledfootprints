sg_boundary <- readRDS(here("data/derived_data/sg_boundary.rds"))
streets <- readRDS(here("data/derived_data/streets.rds"))
area_hatched <- readRDS(here("data/derived_data/area_hatched.rds"))
area_centers <- readRDS(here("data/derived_data/area_centers.rds"))

# function for spatial mapping
spatial_viz <- function(df_attribute, fill_var, legend_title, main_title, main.title_size = 1,
                        legend.title_size = 0.5, legend.text_size = 0.25, digits = 2,
                        legend.hist_height = 0.1, legend.hist_width = 0.3, legend_width = 0.5,
                        legend.hist_size = 0.4, palette = "OrRd",  alpha = 1, style = "quantile",
                        breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25)){
  if(style == "quantile"){
    p <- tm_shape(sg_boundary) + tm_borders(col = "grey") + # sg boundary
      tm_shape(df_attribute) +
      tm_fill(fill_var,
              palette = palette,
              style = "quantile",
              legend.hist=TRUE,
              legend.is.portrait = F,
              legend.format = list(digits = digits),
              alpha = alpha,
              title = legend_title)
  }else if(style == "fixed"){
    p <- tm_shape(sg_boundary) + tm_borders(col = "grey") + # sg boundary
      tm_shape(df_attribute) +
      tm_fill(fill_var,
              palette = palette,
              style = "fixed",
              breaks = breaks,
              legend.hist=TRUE,
              legend.is.portrait = F,
              legend.format = list(digits = digits),
              alpha = alpha,
              title = legend_title)
  }
    p +
      tm_shape(streets) + tm_lines(col = rgb(80, 80, 80, maxColorValue = 255), lwd = 1, alpha = 0.8) + # streets
      tm_shape(area_hatched) + tm_lines(col = rgb(80, 110, 120, maxColorValue = 255), alpha = 0.8) + # reference areas
      tm_layout(main.title = main_title,
              main.title.size = main.title_size,
              frame = FALSE,
              legend.frame = TRUE,
              legend.frame.lwd = 0.4,
              legend.bg.color = "white",
              legend.hist.size = legend.hist_size,
              legend.title.size = legend.title_size,
              legend.text.size = legend.text_size,
              legend.hist.height = legend.hist_height,
              legend.hist.width = legend.hist_width,
              legend.width = legend_width,
              legend.position = c(0.57, 0.001))
   # +
    # tm_compass(type = "8star", size = 2, position = c(0, 0.05)) +
    # tm_scale_bar(position = c(0, 0.001), breaks = c(0, 2.5, 5, 7.5, 10))
}

# function for violin plot
violin_viz <- function(df_attribute, var, type, labs.x, labs.y, breaks, y.shift, x.shift, text.size, digits = 2){
  var <- rlang::sym(var)
  df_mean <- df_attribute %>%
    st_set_geometry(NULL) %>%
    dplyr::summarise(mean = round(mean({{var}}), digits))

  df_attribute %>%
    mutate(type = "") %>%
    ggplot(., aes(x = type, y = {{var}})) +
    geom_violin(trim = F) +
    geom_boxplot(width = 0.1, fill = '#A4A4A4', alpha = 0.5) +
    stat_summary(fun = mean, geom = "point", shape=23, size=2, fill = "red") +
    geom_hline(yintercept = df_mean$mean, linetype="dotted", color = "red", size = 1) +
    geom_text(mapping = aes(label = paste0("Mean: ", df_mean$mean), y = df_mean$mean + y.shift, x = x.shift), col = "red", size = text.size) +
    coord_flip() +
    scale_y_continuous(breaks = breaks) +
    labs(x = labs.x, y = labs.y) +
    theme_classic()
}
