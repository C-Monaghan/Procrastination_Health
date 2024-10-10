################################################################################
# Functions for use in GAM analysis
################################################################################

# Fitting a GAM model ----------------------------------------------------------
fit_gam_models <- function(data, formulas) {
  require(purrr)
  require(mgcv)
  
  map2(names(formulas), 
       formulas, ~ gam(as.formula(paste(.x, .y)),
                       data = data,
                       family = "binomial",
                       method = "REML"))
}

# Tidying and combining GAM output ---------------------------------------------
tidy_and_combine <- function(fits, responses, terms) {
  require(purrr)
  require(broom)
  require(dplyr)
  
  map(fits, tidy) %>%
    bind_rows() %>%
    mutate(response = responses, .before = term,
           term = terms) %>%
    mutate(across(edf:p.value, \(x) round(x, digits = 3)))
}

plot_predictions <- function(model, data, x_var, y_var, x_label, y_label, gender) {
  
  # Visualizing GAM curve
  plot <- visreg(
    fit = model,
    xvar = x_var,
    gg = TRUE,
    scale = "response",
    rug = FALSE) +
    # Adding jittered points
    geom_jitter(
      data = data, 
      aes_string(x = x_var, y = y_var),
      height = 0.05, alpha = 0.5, size = 0.8) +
    # Adjusting axis scales
    scale_x_continuous(breaks = seq(0, 60, by = 10)) +
    # Labelling
    labs(
      title = paste(y_label, "and", x_label),
      x = x_label, 
      y = paste("Prob(", y_label, ")")) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5))
  
  if(gender == "female" && y_label %in% c("Mammograms", "Cholesterol screenings")) {
    plot <- plot +
      theme(plot.title = element_text(colour = "#D2042D"))
  }
  
  return(plot)
}

# Creating a heat map (with alpha blending)
create_heatmap <- function(data, preds, se, title) {
  ggplot(data, aes(x = Total_procrastination, y = Total_depression)) +
    
    # Heat map layer (with alpha blending)
    geom_raster(aes(fill = rescale(!!sym(preds)), alpha = (1/!!sym(se))^2)) +
    
    # Contour lines
    geom_contour(aes(z = !!sym(preds)), color = "black", size = 0.3) +
    
    # Data points
    geom_jitter(data = health_data, aes(x = Total_procrastination, y = Total_depression),
                width = 0.2, height = 0.15, alpha = 0.5) +
    
    # Colour scheme
    scale_fill_viridis_c(option = "plasma") +
    
    # Adjusting axis
    scale_x_continuous(breaks = seq(0, 60, by = 10)) +
    scale_y_continuous(breaks = seq(0, 8, by = 1)) +
    
    # Plot title and labels
    labs(title = title, 
         x = "Procrastination", 
         y = "Depression", 
         fill = expression(hat(p))) +
    
    # Setting themes and legends
    theme_classic() +
    theme(
      plot.title = element_text(
        size = 12,
        lineheight = 1.1,
        hjust = 0.5,
        margin = margin(t = 5, b = 5)),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
    ) +
    guides(alpha = "none")
}

# Exporting Plot Function ------------------------------------------------------
save_gam_plot <- function(filename, plot, height = 10, aspect_ratio = NULL){
  # Getting file path
  file_path <- file.path(export_path_graphics, filename)
  
  # Seperate saving if the aspect ratio value is present
  if(!is.null(aspect_ratio)){
    save_plot(filename = file_path, plot = plot, base_height = height, base_aspect_ratio = aspect_ratio)
  } else{
    save_plot(filename = file_path, plot = plot, base_height = height)
  }
}
