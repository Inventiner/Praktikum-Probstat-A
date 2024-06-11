data <- read.table("10.40.txt", header = TRUE, sep = "\t", dec = ",")
filtered_data <- na.omit(data)
summary(data)

# Uji Hipotesis method 7 (Uji Pooled-Variance t (σi tidak diketahui, tidak sama))
# Hipotesis: (two-tail)(p-value)
# H0 : µ1 = µ2  
# H1 : µ1 ≠ µ2
x1avg <- mean(filtered_data$Smokers)
n1 <- length(filtered_data$Smokers)
var1 <- var(filtered_data$Smokers)

x2avg <- mean(data$Nonsmokers)
n2 <- length(data$Nonsmokers)
var2 <- var(data$Nonsmokers)

d0 <- 0

v <- (var1/n1 + var2/n2)^2/(((var1/n1)^2/(n1-1)) +((var2/n2)^2/(n2-1)))

tval <- (x1avg - x2avg - d0) / (sqrt(var1/n1 + var2/n2))

ltail <- floor(pt(tval, df = round(v), lower.tail = FALSE)*10)/10
rtail <- ceiling(pt(tval, df = round(v), lower.tail = FALSE)*10)/10

message1 <- sprintf("%f < P(T < %f) < %f", ltail, tval, rtail)
message2 <- sprintf("%f < P-value < %f", ltail * 2, rtail * 2)
print(message1)
print(message2)