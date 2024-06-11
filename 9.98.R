data <- read.table("9.98.txt", header = TRUE, sep = "\t", dec = ",")
summary(data)

# Uji Parameter Metode 6 (Estimasi untuk μ1 - μ2 (σ1, σ2 tidak diketahui) (σ1 = σ2)
x1avg <- mean(data$Wire.A)
n1 <- length(data$Wire.A)
var1 <- var(data$Wire.A)
s1 <- sqrt(var1)

x2avg <- mean(data$Wire.B)
n2 <- length(data$Wire.B)
var2 <- var(data$Wire.B)
s2 <- sqrt(var2)

sp <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))

CI <- 0.95
alpha <- 1 - CI

df <- n1 + n2 - 2
tval <- qt(p = (1 - alpha/2), df = df)

output1 = abs(x1avg - x2avg) - tval * sp * sqrt(1/n1 + 1/n2)
output2 = abs(x1avg - x2avg) + tval * sp * sqrt(1/n1 + 1/n2)

if(output1 > output2){
  temp <- output1
  output1 = output2
  output2 = temp
}

message <- sprintf("%f < μ1 - μ2 < %f", output1, output2)
print(message)