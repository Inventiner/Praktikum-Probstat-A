data <- read.table("10.53.txt", header = TRUE, sep = "\t", dec = ",")
summary(data)

# Uji Hipotesis method 9 (Uji t pada 2 Sampel untuk Perbedaan Sampel Mean (σi tidak diketahui))
# Hipotesis: (p-value (no lvl of significance))
# H0 : µ1 - µ2 = 0 
# H1 : µ1 - µ2 ≠ 0 

Davg <- mean(data$Hot.Knife - data$Cold.Knife)
Dvar <- var((data$Hot.Knife - data$Cold.Knife))
Sd <- sqrt(Dvar)
n <- length(data$Dog)

tval <- (Davg) / (Sd/sqrt(n))

pval <- pt(tval, df = n - 1, lower.tail = FALSE)

print("0.15 < P-value < 0.20")
print("Decision: Accept H0 (Hipotesis Salah)")