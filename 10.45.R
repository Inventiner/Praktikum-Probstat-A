data <- read.table("10.45.txt", header = TRUE, sep = "\t", dec = ",")
summary(data)

# Uji Hipotesis method 9 (Uji t pada 2 Sampel untuk Perbedaan Sampel Mean (σi tidak diketahui))
# Hipotesis: (right tail) (p-value (no lvl of significance))
# H0 : µ1 ≤ µ2
# H1 : µ1 > µ2

Davg <- mean(data$Radial.Tires - data$Belted.Tires)
Dvar <- var((data$Radial.Tires - data$Belted.Tires))
Sd <- sqrt(Dvar)
n <- length(data$Car)

tval <- (Davg) / (Sd/sqrt(n))

pval <- pt(tval, df = n - 1, lower.tail = FALSE)

print("0.15 < P-value < 0.02")
print("Decision: Reject H0 (Hipotesis Benar) jika significance lvl diatas 0.02")