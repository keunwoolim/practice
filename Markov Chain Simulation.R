rm(list=ls())

library(ggplot2)
library(expm)   
library(pracma)

T <-  c(50, 500, 5000)
tau <-  linspace(0, 20, n = 101)
r <-  0.01
k <- T/r
alpha <-  5
b <- 0.3
G <-  alpha*matrix(c(-1-b, 1, b, b, -1-b, 1, 1, b, -1-b), nrow = 3, byrow = TRUE)
P <-  expm(r*G)

state <- c(-1,0,1)
value <-  matrix(0, 101,4)

for (l in 1:3) {
  u <- as.integer(k[l])
  i <- as.integer(u+20/r)
  X <- numeric(i+1)
  X[1] <- sample(state, size = 1, prob = c(1/3, 1/3, 1/3))
  
  for (j in 2:i+1) {
    v <- as.integer(X[j-1]+2)
    X[j] <- sample(state, size = 1, prob = P[v, ])
  }
  for (q in 1:101) {
    t <- tau[q]
    integral <-  0
    compt <- as.integer(t/r)
    for (h in 1:u) {
      integral <-  integral + X[h+compt] * X[h]
    }
    value[q, l] <-  integral * r / T[l]
  }
}

value[ , 4] <- 2/3 * exp(-alpha * 3 / 2 * tau * (1+b)) * cos(alpha * sqrt(3) / 2 * tau * (1-b))

#matplot(tau, value, xlab = "Time", ylab = "Value", type = "l", lwd = 2, lty = 1)

data <- data.frame (cbind(tau, value))
colnames(data) <- c("Time", "Function1", "Function2", "Function3", "Target")

final_plot <- ggplot(data, aes(x = Time, y = Value)) + geom_line(aes(y = Target, color = "Target")) + geom_line(aes(y = Function1, color = "T = 50")) + geom_line(aes(y = Function2, color = "T = 500")) + geom_line(aes(y = Function3, color = "T = 5000"))+scale_color_manual(name = "Functions", values = c("Target"="black", "T = 50"="blue", "T = 500" = "green", "T = 5000" = "red"))
final_plot
