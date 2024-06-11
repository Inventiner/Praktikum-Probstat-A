# ANOVA One-Way
  data <- data.frame(
    machine = rep(colnames(data), each = nrow(data)),
    value = unlist(data)
  )
  
  # Hipotesis:
  # H0: μ1 = μ2 = μ3 = μ4 = μ5 = μ6
  # H1: At least two of the means are not equal.
  
  alpha <- 0.05
  conflvl <- 1 - alpha
  
  model <- aov(value ~ machine, data = data)
  anova_table <- summary(model)
  
  p_value <- anova_table[[1]][1, "Pr(>F)"]
  
  print(anova_table)
  if(p_value < alpha) {
    print("Reject H0 (ada satu grup yang rata2 nya tidak sama)")
    tukey_results <- TukeyHSD(model, conf.level = conflvl)
    
    print(tukey_results)
    
    significant_pairs <- tukey_results$machine[tukey_results$machine[, "p adj"] < alpha, ]
    
    print("Data yang memiliki rata2 berbeda:")
    print(significant_pairs)
  } else {
    print("Fail to reject H0 (data rata-rata sama)")
  }

# ANOVA Two-Way
  data <- data.frame(
    tablet = c(rep("Tablet A", 5), rep("Tablet B", 5)),
    dosage = c(rep("Low", 5), rep("High", 5)),
    outcome = rnorm(10) 
  )
  
  data_long <- data %>%
    pivot_longer(cols = c("tablet", "dosage"),  # Specify the columns to reshape
                 names_to = "factor", 
                 values_to = "level") %>% 
    pivot_wider(names_from = c("factor", "level"), 
                values_from = "outcome")
  
  # Hipotesis:
  # H0: μ1 = μ2 = μ3 = μ4 = μ5 = μ6
  # H1: At least two of the means are not equal.
  model <- aov(outcome ~ tablet * dosage, data = data_long)
  anova_table <- summary(model)
  
  p_value <- anova_table[[1]][1, "Pr(>F)"]
  
  print(anova_table)
  if(p_value < alpha) {
    print("Data yang memiliki rata2 berbeda:")
  } else {
    print("Fail to reject H0 (data rata-rata sama)")
  }