# Uji Hipotesis 1 Populasi
# Method 1 (Uji Z (one-tail) untuk Mean (σ diketahui))
  # Hipotesis: (Right-Tail)
  # H0: μ ≤ 368
  # H1: μ > 368
  xavg <- 372.5
  n <-  25
  diff <- 368
  sd <- 15
  
  zval <- (xavg-diff) / (sd/sqrt(n))
  
  alpha <- 0.05
  
  critval <- qnorm(p=alpha, lower.tail=FALSE)
  if(zval > critval){
    print("Reject H0 (Hipotesis Benar)")
  } else {
    print("Fail to reject H0 (Hipotesis Salah)")
  }

# Method 2 (Uji t (one-tail) untuk Mean (σ tidak diketahui))
  # Hipotesis: (Right-Tail)
  # H0: μ ≤ 368
  # H1: μ > 368
  xavg <- 372.5
  n <-  36
  diff <- 368
  sd <- 15
  alpha <- 0.01
  
  tval <- (xavg - diff) / (sd / sqrt(n))
  critval  <- qt(p = alpha, df = n - 1, lower.tail = FALSE)
  if(tval > critval){
    print("Reject H0 (Hipotesis Benar)")
  } else {
    print("Fail to reject H0 (Hipotesis Salah)")
  }

# Method 3 (Uji Proporsi Populasi)
  # np = 500(.04) = 20 ≥ 5
  # n(1 - p) = 500 (1 - .04) = 480 ≥ 5
  # Hipotesis: (Two Tail)
  # H0: p = .04
  # H1: p ≠ .04
  n = 500
  ps = 25/500
  alpha = 0.05
  p = 0.04
  
  zval = (ps-p) / sqrt((p * (1 - p))/n)
  critvalL = qnorm(p=alpha/2, lower.tail = TRUE)
  critvalR = qnorm(p=alpha/2, lower.tail = FALSE)
  if(zval > critvalR | zval < critvalL){
    print("Reject H0 (Hipotesis Benar)")
  } else {
    print("Fail to reject H0 (Hipotesis Salah)")
  }

# Method 5 (Uji Z pada 2 Sampel untuk Sampel Mean (σi diketahui))
  x1avg <- mean()
  var1 <- 
  n1 <-
    
  x2avg <- mean()
  var2 <-
  n2 <-
    
  d <- 0
  alpha <- 0.05
  
  zval <- (x1avg - x2avg - d)/sqrt(var1/n1+ var2/n2)
  critval <- qnorm(p=alpha, lower.tail = 1)
  
  if(zval < critval){
    print("Reject H0 (Hipotesis Benar)")
  } else {
    print("Fail to reject H0 (Hipotesis Salah)")
  }
# Method 6 (Uji Pooled-Variance t (σi tidak diketahui, dan sama))
  # Hipotesis: (left-tail)
  # H0 : µ1 − µ2 ≥ 0 
  # H1 : µ1 − µ2 < 0
  
  x1avg <- mean(filtered_data$No.Treatment)
  n1 <- length(filtered_data$No.Treatment)
  var1 <- var(filtered_data$No.Treatment)
  s1 <- sqrt(var1)
  
  x2avg <- mean(data$Treatment)
  n2 <- length(data$Treatment)
  var2 <- var(data$Treatment)
  s2 <- sqrt(var2)
  
  alpha <- 0.05
  
  sp <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / ((n1 - 1) + (n2 - 1)))
  
  tval <- (x1avg - x2avg) / (sqrt(sp^2 * (1/n1 + 1/n2)))
  
  ltail <- qt(p=alpha, df = n1 + n2 - 2)
  
  if(tval < ltail) {
    print("Reject H0, (Hipotesis Benar)")
  } else {
    print("Accept H0, (Hipotesis Salah)")
  }

# Method 7 (Uji Pooled-Variance t (σi tidak diketahui, tidak sama)) 
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
  
  if(tval < ltail | tval > rtail) {
    print("Reject H0, (Hipotesis Benar)")
  } else {
    print("Accept H0, (Hipotesis Salah)")
  }
  
# Method 8 (Uji F untuk Perbedaan 2 Populasi Variance)
  # Hipotesis: (two-tail)
  # H0: σ1^2 = σ2^2
  # H1: σ1^2 ≠ σ2^2
  n1 <- 21
  s1 <- 1.30
  var1 <- s1^2
  df1 <- n1 - 1
      
  n2 <- 25
  s2 <- 1.16
  var2 <- s2^2
  df2 <- n2 - 1
  
  alpha <- 0.05
  
  fval <- var1/var2
  critvalL <- qf(p = alpha/2, df1 = df1, df2=df2, lower.tail = TRUE)
  critvalR <- qf(p = alpha/2, df1 = df1, df2=df2, lower.tail = FALSE)
  if(zval > critvalR | zval < critvalL){
    print("Reject H0 (Hipotesis Benar)")
  } else {
    print("Fail to reject H0 (Hipotesis Salah)")
  }
  
# Method 9 (Uji t pada 2 Sampel untuk Perbedaan Sampel Mean (σi tidak diketahui))
  # Hipotesis: (p-value (no lvl of significance))
  # H0 : µ1 ≥ µ2
  # H1 : µ1 < µ2
  
  Davg <- mean(data$Prefatigue - data$Postfatigue)
  Dvar <- var((data$Prefatigue - data$Postfatigue))
  Sd <- sqrt(Dvar)
  n <- length(data$Subject)
  
  tval <- (Davg) / (Sd/sqrt(n))
  
  pval <- pt(tval, df = n - 1, lower.tail = TRUE)
  
  print("0.01 < P-value < 0.015")
  print("Decision: Reject H0 (Hipotesis Benar)")