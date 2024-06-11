data <- read.table("10.35.txt", header = TRUE, sep = "\t", dec = ",")
filtered_data <- na.omit(data)
summary(data)

# Uji Hipotesis method 6 (Uji Pooled-Variance t (σi tidak diketahui, sama))
# Hipotesis: (left-tail)
# H0 : µ1 − µ2 ≥ 0 
# H1 : µ1 − µ2 < 0

x1avg <- mean(filtered_data$No.Treatment)
n1 <- length(filtered_data$No.Treatment)
var1 <- var(filtered_data$No.Treatment)
s1 <- sqrt(var1)

x2avg <- mean(data$Treatment)
n2 <- length(data$Treatment)
var2 <- var(data$Treatment)
s2 <- sqrt(var2)

alpha <- 0.05

sp <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / ((n1 - 1) + (n2 - 1)))

tval <- (x1avg - x2avg) / (sqrt(sp^2 * (1/n1 + 1/n2)))

ltail <- qt(p=alpha, df = n1 + n2 - 2)

if(tval < ltail) {
  print("Reject H0, (Hipotesis Benar)")
} else {
  print("Accept H0, (Hipotesis Salah)")
}