data <- read.table("10.39.txt", header = TRUE, sep = "\t", dec = ",")
filtered_data <- na.omit(data)
summary(data)

# Uji Hipotesis method 7 (Uji Pooled-Variance t (σi tidak diketahui, tidak sama))
# Hipotesis: (right-tail)
# H0 : µ2 − µ1 ≤ 10 
# H1 : µ2 − µ1 > 10

x1avg <- mean(data$Company.2)
n1 <- length(data$Company.2)
var1 <- var(data$Company.2)

x2avg <- mean(filtered_data$Company.1)
n2 <- length(filtered_data$Company.1)
var2 <- var(filtered_data$Company.1)

d0 <- 10
alpha <- 0.1

v <- (var1/n1 + var2/n2)^2/(((var1/n1)^2/(n1-1)) +((var2/n2)^2/(n2-1)))

tval <- (x1avg - x2avg - d0) / (sqrt(var1/n1 + var2/n2))

ltail <- qt(p=alpha, df = round(v), lower.tail = FALSE)

if(tval > ltail) {
  print("Reject H0, (Hipotesis Benar)")
} else {
  print("Accept H0, (Hipotesis Salah)")
}