data <- read.table("10.41.txt", header = TRUE, sep = "\t", dec = ",")
filtered_data <- na.omit(data)
summary(data)

# Uji Hipotesis method 7 (Uji Pooled-Variance t (σi tidak diketahui, tidak sama)) 
# Hipotesis: (two tail)
# H0 : µ1 = µ2
# H1 : µ1 ≠ µ2

x1avg <- mean(data$Station.1)
n1 <- length(data$Station.1)
var1 <- var(data$Station.1)

x2avg <- mean(filtered_data$Station.2)
n2 <- length(filtered_data$Station.2)
var2 <- var(filtered_data$Station.2)

d0 <- 0
alpha <- 0.05

v <- (var1/n1 + var2/n2)^2/(((var1/n1)^2/(n1-1)) +((var2/n2)^2/(n2-1)))

tval <- (x1avg - x2avg - d0) / (sqrt(var1/n1 + var2/n2))

rtail <- qt(p=alpha/2, df = round(v), lower.tail = FALSE)
ltail <- rtail * -1

message <- sprintf("tval(%f) < ltail(%f) | tval(%f) > rtail(%f)", tval, ltail, tval, rtail)
print(message)
if(tval < ltail | tval > rtail) {
  print("Reject H0, (Hipotesis Benar)")
} else {
  print("Accept H0, (Hipotesis Salah)")
}