data <- read.table("9.46.txt", header = TRUE, sep = "\t", dec = ",")
filtered_data <- na.omit(data)
summary(filtered_data)

# Uji Parameter Metode 7 (Estimasi untuk μ1 - μ2 (σ1, σ2 tidak diketahui) (σ1 ≠ σ2)
x1avg <- mean(filtered_data$Company.I)
n1 <- length(filtered_data$Company.I)
var1 <- var(filtered_data$Company.I)
s1 <- sqrt(var1)

x2avg <- mean(data$Company.II)
n2 <- length(data$Company.II)
var2 <- var(data$Company.II)
s2 <- sqrt(var2)

CI <- 0.90
alpha <- 1 - CI

df <- (var1/n1 + var2/n2)^2 / (((var1/n1)^2/(n1 - 1)) + ((var2/n2)^2/(n2 - 1)))
tval <- qt(p = (alpha/2), df = df, lower.tail = FALSE)

output1 = abs(x1avg - x2avg) - tval * sqrt(var1/n1 + var2/n2)
output2 = abs(x1avg - x2avg) + tval * sqrt(var1/n1 + var2/n2)

if(output1 > output2){
  temp <- output1
  output1 = output2
  output2 = temp
}

message <- sprintf("%f < μ1 - μ2 < %f", output1, output2)
print(message)