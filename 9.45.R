data <- read.table("9.45.txt", header = TRUE, sep = "\t", dec = ",")
summary(data)

# Uji Parameter Metode 8 (CI Observasi Berpasangan (µd))
davg <- abs(mean(data$Variety.1 - data$Variety.2))
n <- length(data$University)
sd <- sd(data$Variety.1 - data$Variety.2)

CI <- 0.95
alpha <- 1 - CI

df <- n - 1
tval <- qt(p = (1 - alpha/2), df = df)

output1 = davg - tval * (sd/sqrt(n))
output2 = davg + tval * (sd/sqrt(n))

if(output1 > output2){
  temp <- output1
  output1 = output2
  output2 = temp
}

message <- sprintf("%f < μd < %f", output1, output2)
print(message)