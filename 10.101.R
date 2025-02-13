data <- read.table("10.101.txt", header = TRUE, sep = "\t", dec = ",")
summary(data)

# Method 6 (Uji Pooled-Variance t (σi tidak diketahui, dan sama))
# Hipotesis: (left-tail)
# H0 : µ1 − µ2 ≥ 0 
# H1 : µ1 − µ2 < 0

x1avg <- mean(data$Group.1)
n1 <- length(data$Group.1)
var1 <- var(data$Group.1)
s1 <- sqrt(var1)

x2avg <- mean(data$Group.2)
n2 <- length(data$Group.2)
var2 <- var(data$Group.2)
s2 <- sqrt(var2)

sp <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / ((n1 - 1) + (n2 - 1)))

tval <- (x1avg - x2avg) / (sqrt(sp^2 * (1/n1 + 1/n2)))

tp <- pt(q = tval, df = 8, lower.tail = FALSE)
