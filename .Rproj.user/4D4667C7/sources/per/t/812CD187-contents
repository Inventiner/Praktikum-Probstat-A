data <- read.table("13.1.txt", header = TRUE, sep = "\t", dec = ",")
summary(data)

data <- data.frame(
  machine = rep(colnames(data), each = nrow(data)),
  value = unlist(data)
)

# One Way ANOVA
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