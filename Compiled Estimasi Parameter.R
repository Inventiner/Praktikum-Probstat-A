# Confidence Interval Satu Populasi
# Uji Parameter Metode 1 (Estimasi Confidence Interval untuk μ (σ diketahui))
  xavg <- mean(data$Polished)
  n <- length(data$Polished)
  sd <- 0.3
  
  CI <- 0.95
  alpha <- 1 - CI
  
  zval <- qnorm(1 - alpha/2)
  
  output1 = xavg - zval * sd / sqrt(n)
  output2 = xavg + zval * sd / sqrt(n)
  
  if(output1 > output2){
    temp <- output1
    output1 = output2
    output2 = temp
  }
  
  message <- sprintf("%f ≤ μ ≤ %f", output1, output2)
  print(message)

  # Find n sample
  # result <- ((zval*sd)/alpha)^2
  # message <- sprintf("n: %f", result)
  # print(message)
  
# Uji Parameter Metode 2 (Estimasi Confidence Interval untuk μ (σ tidak diketahui))
  data <- c(9.8, 10.2, 10.4, 9.8, 10.0, 10.2, 9.6)
  xavg <- mean(data)
  n <- length(data)
  S2 <- var(data)
  S <- sqrt(S2)
  
  CI <- 0.95
  alpha <- 1 - CI
  
  tval <- qt(p = (alpha/2), df = n - 1, lower.tail = FALSE)
  
  output1 = xavg - tval * S / sqrt(n)
  output2 = xavg + tval * S / sqrt(n)
  
  if(output1 > output2){
    temp <- output1
    output1 = output2
    output2 = temp
  }
  
  message <- sprintf("%f ≤ μ ≤ %f", output1, output2)
  print(message)
  
# Uji Parameter Metode 3 (Estimasi Confidence Interval untuk p)
  n <- 400
  p <- 32/n
  
  CI <- 0.95
  alpha <- 1 - CI
  
  zval <- qnorm(1 - alpha/2)
  
  output1 = p - zval * sqrt((p*(1-p)/n))
  output2 = p + zval * sqrt((p*(1-p)/n))
  
  if(output1 > output2){
    temp <- output1
    output1 = output2
    output2 = temp
  }
  
  message <- sprintf("%f ≤ p ≤ %f", output1, output2)
  print(message)
  
# Uji Parameter Metode 4 (Estimasi Confidence Interval untuk σ)
  data <- c(46.4, 46.1, 45.8, 47.0, 46.1, 45.9, 45.8, 46.9, 45.2, 46.0)
  n <- length(data)
  s <- var(data)
  
  CI <- 0.95
  alpha <- 1 - CI
  
  chisqval1 <- qchisq(p = alpha/2, df = n - 1, lower.tail = FALSE)
  chisqval2 <- qchisq(p = 1-alpha/2, df = n - 1, lower.tail = FALSE)
  
  output1 = ((n - 1)*s/chisqval1)
  output2 = ((n - 1)*s/chisqval2)
  
  if(output1 > output2){
    temp <- output1
    output1 = output2
    output2 = temp
  }
  
  message <- sprintf("%f ≤ p ≤ %f", output1, output2)
  print(message)

# Confidence Interval Dua Populasi
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

# Uji Parameter Metode 6 (Estimasi untuk μ1 - μ2 (σ1, σ2 tidak diketahui) (σ1 = σ2)
  x1avg <- mean(data$No.Nitrogen)
  n1 <- length(data$No.Nitrogen)
  var1 <- var(data$No.Nitrogen)
  s1 <- sqrt(var1)
  
  x2avg <- mean(data$Nitrogen)
  n2 <- length(data$Nitrogen)
  var2 <- var(data$Nitrogen)
  s2 <- sqrt(var2)
  
  sp <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))
  
  CI <- 0.95
  alpha <- 1 - CI
  
  df <- n1 + n2 - 2
  tval <- qt(p = (alpha/2), df = df, lower.tail = FALSE)
  
  output1 = abs(x1avg - x2avg) - tval * sp * sqrt(1/n1 + 1/n2)
  output2 = abs(x1avg - x2avg) + tval * sp * sqrt(1/n1 + 1/n2)
  
  if(output1 > output2){
    temp <- output1
    output1 = output2
    output2 = temp
  }
  
  message <- sprintf("%f < μ1 - μ2 < %f", output1, output2)
  print(message)

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

# Uji Parameter Metode 9
  n1 <- 250
  p1 <- 80/n1
  q1 <- 1 - p1
  
  n2 <- 175
  p2 <- 40/n2
  q2 <- 1 - p2
  
  CI <- 0.90
  alpha <- 1 - CI
  
  zval <- qnorm(1 - alpha/2)
  
  output1 = (p1 - p2) - zval * sqrt((p1*q1/n1)+(p2*q2/n2))
  output2 = (p1 - p2) + zval * sqrt((p1*q1/n1)+(p2*q2/n2))
  
  if(output1 > output2){
    temp <- output1
    output1 = output2
    output2 = temp
  }
  
  message <- sprintf("%f < p1 - p2 < %f", output1, output2)
  print(message)
  
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
