data <- read.table("9.94.txt", header = TRUE, sep = "\t", dec = ",")
summary(data)

# Uji Parameter Metode 8 (CI Observasi Berpasangan (µd))
davg <- abs(mean(data$MPN.Count - data$M.5.hr.Count))
n <- length(data$Sample)
sd <- sd(data$MPN.Count - data$M.5.hr.Count)

CI <- 0.90
alpha <- 1 - CI

df <- n - 1
tval <- qt(p = (alpha/2), df = df, lower.tail = FALSE)

output1 = (davg - tval * (sd/sqrt(n)))
output2 = (davg + tval * (sd/sqrt(n)))

if(output1 > output2){
  temp <- output1
  output1 = output2
  output2 = temp
}

message <- sprintf("%f < μd < %f", output1, output2)
print(message)