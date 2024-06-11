data <- read.table("9.97.txt", header = TRUE, sep = "\t", dec = ",")
summary(data)

# Uji Parameter Metode 10 (CI untuk ratio σ (σ1/σ2))
n1 <- length(data$New.Supplier)
v1 <- n1 - 1
var1 <- var(data$New.Supplier)
s1 <- sqrt(var1)

n2 <- length(data$Old.Supplier)
v2 <- n2 - 1
var2 <- var(data$Old.Supplier)
s2 <- sqrt(var2)

CI <- 0.95
alpha <- 1 - CI

fval <- qf(alpha/2, v1, v2, lower.tail=FALSE)

output1 = (var1/var2) * (1/fval)
output2 = (var1/var2) * fval

if(output1 > output2){
  temp <- output1
  output1 = output2
  output2 = temp
}


output3 = sqrt(output1)
output4 = sqrt(output2)

message <- sprintf("%f < σ1^2/σ2^2 < %f", output1, output2)
message2 <- sprintf("%f < σ1/σ2 < %f", output3, output4)
print(message)
print("Or")
print(message2)