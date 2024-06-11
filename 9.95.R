data <- read.table("9.95.txt", header = TRUE, sep = "\t", dec = ",")
summary(data)

# Uji Parameter Metode 5 (Estimasi untuk μ1 - μ2 (σ1, σ2 diketahui)
x1avg <- mean(data$Polished)
n1 <- length(data$Polished)
var1 <- var(data$Polished)
s1 <- 4000

x2avg <- mean(data$Unpolished)
n2 <- length(data$Unpolished)
s2 <- 4000

CI <- 0.95
alpha <- 1 - CI

zval <- qnorm(1 - alpha/2)

output1 = abs(x1avg - x2avg) - zval * sqrt(s1^2/n1 + s2^2/n2)
output2 = abs(x1avg - x2avg) + zval * sqrt(s1^2/n1 + s2^2/n2)

if(output1 > output2){
  temp <- output1
  output1 = output2
  output2 = temp
}

message <- sprintf("%f < μ1 - μ2 < %f", output1, output2)
print(message)