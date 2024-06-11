data <- read.table("10.43.txt", header = TRUE, sep = "\t", dec = ",")
summary(data)

# Uji Hipotesis method 9 (Uji t pada 2 Sampel untuk Perbedaan Sampel Mean (σi tidak diketahui))
# Hipotesis: (p-value (no lvl of significance))
# H0 : µ1 ≥ µ2
# H1 : µ1 < µ2

Davg <- mean(data$Prefatigue - data$Postfatigue)
Dvar <- var((data$Prefatigue - data$Postfatigue))
Sd <- sqrt(Dvar)
n <- length(data$Subject)

tval <- (Davg) / (Sd/sqrt(n))

pval <- pt(tval, df = n - 1, lower.tail = TRUE)

print("0.01 < P-value < 0.015")
print("Decision: Reject H0 (Hipotesis Benar)")