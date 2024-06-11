data <- read.table("9.93.txt", header = TRUE, sep = "\t", dec = ",")
summary(data)

# Uji Parameter Metode 8 (CI Observasi Berpasangan (µd))
davg <- abs(mean(data$Waist.Size.Before - data$Waist.Size.After))
n <- length(data$Man)
sd <- sd(data$Waist.Size.Before - data$Waist.Size.After)

CI <- 0.95
alpha <- 1 - CI

df <- n - 1
tval <- qt(p = (1 - alpha/2), df = df)

output1 = (davg - tval * (sd/sqrt(n)))
output2 = (davg + tval * (sd/sqrt(n)))

if(output1 > output2){
  temp <- output1
  output1 = output2
  output2 = temp
}

message <- sprintf("%f < μd < %f", output1, output2)
print(message)