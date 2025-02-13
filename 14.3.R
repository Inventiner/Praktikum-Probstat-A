# ANOVA Two-Way
data <- read.table("14.3.txt", header = TRUE, sep = "\t")
# Hipotesis:
# H0: μ1 = μ2 = μ3 = μ4 = μ5 = μ6
# H1: At least two of the means are not equal.
alpha <- 0.01
model <- aov(ErrorScore ~ Strain * Environment, data = data)
anova_table <- summary(model)
p_value <- anova_table[[1]][1, "Pr(>F)"]
print(anova_table)
if(p_value < alpha) {
  print("Data yang memiliki rata2 berbeda:")
} else {
  print("Fail to reject H0 (data rata-rata sama)")
}
