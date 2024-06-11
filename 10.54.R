data <- read.table("10.54.txt", header = TRUE, sep = "\t", dec = ",")
summary(data)

# Uji Hipotesis method 9 (Uji t pada 2 Sampel untuk Perbedaan Sampel Mean (σi tidak diketahui))
# Hipotesis: (two tail)
# H0 : µ1 - µ2 = 0 
# H1 : µ1 - µ2 ≠ 0 

Davg <- mean(data$With.CO - data$Without.CO)
Dvar <- var((data$With.CO - data$Without.CO))
Sd <- sqrt(Dvar)
n <- length(data$Subject)

tval <- (Davg) / (Sd/sqrt(n))

pval <- pt(tval, df = n - 1, lower.tail = FALSE)

print("0.02 < P-value < 0.025")
print("Decision: Reject H0 (CO memiliki efek pada frekuensi bernafas)")