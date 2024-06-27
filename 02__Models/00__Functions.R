# Custom Functions -------------------------------------------------------------
# Create a histogram
create_histogram <- function(data, x_variable, x_label, binwidth){
  require(ggplot2)
  require(ggeasy)
  
  ggplot(data = data, aes({{x_variable}})) +
    geom_histogram(binwidth = binwidth, fill = "skyblue",
                   colour = "black", alpha = 0.8) +
    labs(title = "", x = x_label, y = "Frequency") +
    theme_bw() +
    ggeasy::easy_center_title()
}

# Create a scatterplot
create_scatter_plot <- function(data, y_variable, y_label){
  require(ggplot2)
  require(ggeasy)
  
  ggplot(data = data, aes(x = Total_procrastination, y = {{y_variable}})) +
    geom_jitter() +
    geom_smooth(method = "lm", se = TRUE) + 
    labs(title = paste0("Relationship between Procrastination and ", y_label),
         x = "Procrastination", y = y_label) + 
    theme_bw(base_size = 12) +
    ggeasy::easy_center_title()
}

# Create a health + procrastination bar chart
generate_health_plot <- function(data, gender, gender_title, variables) {
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  require(ggeasy)
  
  data %>%
    filter(Gender == gender) %>%
    select({{ variables }}, Total_procrastination) %>%
    tidyr::pivot_longer(
      cols = !Total_procrastination,
      names_to = "Health_Protection",
      values_to = "Did"
    ) %>%
    group_by(Health_Protection, Did) %>%
    summarize(
      mean_procrastination = round(mean(
        Total_procrastination, na.rm = TRUE), digits = 2
      )
    ) %>%
    filter(complete.cases(Did)) %>%
    mutate(
      Health_Protection = factor(case_when(
        Health_Protection == "Flu_shot" ~ "Flu Shot",
        Health_Protection == "Cholesterol_screening" ~ "Cholesterol Screening",
        Health_Protection == "Dental_visit_2_years" ~ "Dental Visit (2yrs)",
        Health_Protection == "Prostate_exam" ~ "Prostate Exam",
        Health_Protection == "Mammogram" ~ "Mammogram",
        Health_Protection == "Pap_smear" ~ "Pap Smear"
      ),
      levels = c(
        "Flu Shot", "Cholesterol Screening", "Dental Visit (2yrs)",
        "Prostate Exam", "Mammogram", "Pap Smear"
      )),
      Did = ifelse(Did == 0, "Didn\'t Get", "Got"),
      Did = factor(Did)
    ) %>%
    ggplot(aes(x = Health_Protection, y = mean_procrastination, fill = Did)) +
    geom_bar(stat = "identity", position = "dodge", width = .75) +
    labs(x = "", y = ifelse(gender == 0, "Mean Procrastination", ""),
         title = ifelse(gender == 0, paste(gender_title, "Health Protective Behaviours in 2020"), paste(gender_title, "Health Protective Behaviours in 2020"))) +
    theme_bw(base_size = 12) +
    geom_text(aes(label = round(mean_procrastination, 2)),
              position = position_dodge(width = 0.75),
              vjust = -0.5, size = 4) +
    ylim(0, 30 * 1.1) +
    ggeasy::easy_center_title() +
    ggeasy::easy_add_legend_title("") +
    ggeasy::easy_move_legend(to = c("bottom"))
}

# Create a frequency count bar chart
process_health_data <- function(data, variables, gender_title, type) {
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  require(ggeasy)
  
  if (type == "problems_frequency") {
    processed_data <- data %>%
      select({{variables}}) %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "Health_Problem",
                          values_to = "Present") %>%
      mutate(Health_Problem = factor(case_when(
        Health_Problem == "Back_pain" ~ "Back Pain",
        Health_Problem == "Headache" ~ "Headache",
        Health_Problem == "Fatigue" ~ "Fatigue",
        Health_Problem == "Alcohol" ~ "Currently Drinking",
        Health_Problem == "Smoker_current" ~ "Smoking Status",
        Health_Problem == "Blood_pressure" ~ "Blood Pressure",
        Health_Problem == "Diabetes" ~ "Diabetes",
        Health_Problem == "Cholesterol" ~ "Cholesterol",
        Health_Problem == "Heart_condition" ~ "Heart Condition"), 
        levels = c("Back Pain", "Headache", "Fatigue", "Currently Drinking",
                   "Smoking Status", "Blood Pressure", "Diabetes", 
                   "Cholesterol", "Heart Condition"))) %>%
      mutate(Present = ifelse(Present == 0, "No", "Yes"),
             Present = factor(Present)) %>%
      filter(complete.cases(Present)) %>%
      ggplot(aes(x = Health_Problem, fill = Present)) +
      geom_bar(position = "dodge") +
      geom_text(stat = "count", aes(label = paste("n =", ..count..)), 
                vjust = -0.5, size = 5, position = position_dodge(width = 0.9)) +
      labs(x = "", y = "Frequency", 
           title = "Health Problems") +
      theme_bw(base_size = 15) +
      ggeasy::easy_move_legend(to = "bottom") +
      ggeasy::easy_remove_legend_title() +
      ggeasy::easy_center_title()
  } else if(type == "protection_frequency"){
    processed_data <- data %>%
      select({{variables}}) %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "Protection", 
                          values_to = "Did") %>%
      mutate(Protection = factor(case_when(
        Protection == "Flu_shot" ~ "Flu Shot",
        Protection == "Cholesterol_screening" ~ "Cholesterol Screening",
        Protection == "Dental_visit_2_years" ~ "Dental Visit (2yrs)",
        Protection == "Prostate_exam" ~ "Prostate Exam",
        Protection == "Mammogram" ~ "Mammogram",
        Protection == "Pap_smear" ~ "Pap Smear"),
        levels = c("Flu Shot", "Cholesterol Screening", "Dental Visit (2yrs)",
                   "Prostate Exam", "Mammogram", "Pap Smear"))) %>%
      mutate(Did = factor(ifelse(Did == 0, "Didn\'t Get", "Got"))) %>%
      filter(complete.cases(Did)) %>%
      ggplot(aes(x = Protection, fill = Did)) +
      geom_bar(position = "dodge") +
      geom_text(stat = "count", aes(label = paste("n =", ..count..)), 
                vjust = -0.5, size = 4.5, position = position_dodge(width = 0.9)) +
      ylim(0, 700) +
      labs(x = "", y = "Frequency", 
           title = paste0(gender_title, " ", "Health Protective Behaviours")) +
      theme_bw(base_size = 14) +
      ggeasy::easy_move_legend(to = "bottom") +
      ggeasy::easy_remove_legend_title() +
      ggeasy::easy_center_title()
  } else {
    stop("Invalid type. Choose either 'problems_frequency' or 'protection_frequency'.")
  }
  return(processed_data)
}

# Running binary logistic regression
logit_model <- function(outcome, predictor, data, type) {
  require(DescTools)
  
  if(type == "base") {
  # Fit the model
  model <- glm(formula = paste(outcome, " ~ ", predictor), 
               family = binomial(link = "logit"), 
               data = data)
  
  # Generating goodness of fit stats (using DescTools)
  goodness_fit <- DescTools::PseudoR2(model, which = "all")
  
  # Saving model summary
  model_summary <- summary(model)
  
  # Saving odds ratio
  odds <- exp(model$coefficients[[2]])
  
  # Returning info
  return(list(
    type = paste("Binary Logistic Regression for", outcome, "and", predictor),
    model = model, 
    model_summary = model_summary,
    goodness = goodness_fit,
    odds = paste("Odds Ratio: ", odds)
    ))
  
  } else if(type == "control") {
    model <- glm(formula = paste(outcome, " ~ ", predictor, " + Total_depression + Age"), 
                 family = binomial(link = "logit"), 
                 data = data)
    
    # Generating goodness of fit stats (using DescTools)
    goodness_fit <- DescTools::PseudoR2(model, which = "all")
    
    # Saving model summary
    model_summary <- summary(model)
    
    anova <- anova(model, test = "Chisq")
    
    # Saving odds ratio
    odds <- exp(model$coefficients[[2]])
    
    # Returning info
    return(list(
      type = paste("Binary Logistic Regression (with control) for", outcome, "and", predictor),
      model = model, 
      model_summary = model_summary,
      anova = anova,
      goodness = goodness_fit,
      odds = paste("Odds Ratio: ", odds)
    ))
  }
}

# Create a logistic regression curve
generate_log_plot <- function(predictor, data, title) {
  require(ggplot2)
  require(ggeasy)
  
  ggplot(data = data, aes(x = Total_procrastination, y = {{predictor}})) +
    geom_jitter(size = 0.75, width = 0, height = .1) +
    geom_smooth(method = "glm", se = TRUE, 
                method.args = list(family = binomial(link = "logit")), 
                col = "red", lty = 2) +
    labs(x = "Procrastination", y = "", title = title) +
    scale_y_continuous(breaks = c(0, 1)) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold")) +
    easy_remove_gridlines() +
    easy_center_title()
}

# Process GLM results
process_glm_results <- function(model_list, type){
  if(type == "base"){
  # Creating an empty list to be filled
  glm_results <- list(
    odds = rep(NA, length(model_list)),
    ci_lower = rep(NA, length(model_list)),
    ci_upper = rep(NA, length(model_list))
  )
  
  # Extracting relevant variables
  for (i in seq_along(model_list)) {
    # Odds ratio
    glm_results$odds[i] <- exp(model_list[[i]]$model$coefficients[[2]])
    
    # Confidence Intervals
    conf_int <- confint(model_list[[i]]$model)[2, ]
    glm_results$ci_lower[i] <- exp(conf_int[1])
    glm_results$ci_upper[i] <- exp(conf_int[2])
  }
  
  return(glm_results)
  } else if(type == "control"){
    # Create an empty nested list to be filled 
    glm_results <- list(
      odds = vector("list", length(model_list)),
      ci_lower = vector("list", length(model_list)),
      ci_upper = vector("list", length(model_list))
    )
    
    # Extracting relevant variables
    for(i in seq_along(model_list)){
      # Odds Ratio
      glm_results$odds[[i]] <- exp(coef(model_list[[i]]$model)[-1])
      
      # Confidence Intervals
      ci <- exp(confint(model_list[[i]]$model))
      
      glm_results$ci_lower[[i]] <- ci[-1, 1]
      glm_results$ci_upper[[i]] <- ci[-1, 2]
    }
    
    # Converting to data frame
    glm_results <- do.call(
      rbind, lapply(seq_along(glm_results$odds), function(i) {
        data.frame(
          odds = glm_results$odds[[i]],
          ci_lower = glm_results$ci_lower[[i]],
          ci_upper = glm_results$ci_upper[[i]],
          row.names = NULL)}))
    
    return(glm_results)
  }
}

# Create log odds plot
log_odds_plot <- function(data, title, xlim_lower = 0.50, xlim_upper = 1.50, size_font = 8){
  require(ggplot2)
  require(ggeasy)
  
  # If the x limits equal default values run this code
  if(xlim_lower == 0.50 & xlim_upper == 1.50){
    # Calculate the range based on ci_lower and ci_upper
    range_ci <- range(c(data$ci_lower, data$ci_upper))
    
    # Determine the symmetric xlim with 1 in the center
    xlim_lower <- min(c(range_ci[1], 2 - range_ci[2]))
    xlim_upper <- max(c(range_ci[2], 2 - range_ci[1]))
  }

  # Plotting odds
  ggplot(data = data, aes(y = response)) +
    geom_point(aes(x = odds), colour = "skyblue", size = 3) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.5, linewidth = 1, color = "skyblue") +
    geom_vline(xintercept = 1, color = "red", linetype = "dashed") +
    xlim(c(xlim_lower, xlim_upper)) +
    labs(x = "Odds (95% CI)", y = "", title = title) +
    theme_bw() +
    easy_center_title() +
    theme(title = element_text(size = size_font))
}

# Linearity Plot
linearity_plot <- function(data, x_variable, y_variable, title){
  require(ggplot2)
  
  ggplot(data = data, aes(x = {{x_variable}}, y = {{y_variable}})) +
    geom_jitter(width = 0, height = .1) +
    geom_smooth(formula = y ~ poly(x,2), se = FALSE, method = "glm", 
                method.args = list(family = "binomial")) +
    labs(x = "Procrastination", y = "", title = title) +
    scale_y_continuous(breaks = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 60, by = 10)) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold")) +
    ggeasy::easy_center_title()
}

# Curve Plot
non_linear_fit <- function(response, data, title){
  require(ggplot2)
  
  fit <- glm(formula = paste(response, "~ Total_procrastination + I(Total_procrastination^2)"), 
             data = data, family = "binomial")
  
  f <- function(x){
    exp(coef(fit)[2]) * exp(coef(fit)[3]) * exp(2*x*coef(fit)[3])
  }
  
  # Generate x values
  x_values <- seq(0, 60, length.out = 100)
  
  # Create a data frame with x and f(x) values
  df <- data.frame(x = x_values, y = f(x_values))
  
  # Plot the curve using ggplot
  ggplot(df, aes(x, y)) +
    geom_line() +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
    xlim(0, 60) +
    labs(x = "Procrastination", y = "", title = title) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold")) +
    ggeasy::easy_center_title()
}