rm(list=ls())

library(parma)
library(xts)
library(CVXR)

data("etfdata")
Data <- etfdata/lag(etfdata) - 1
Data <- na.omit(Data)

#CVXR code

weight <- Variable(15)
abs_weight <- Variable(15)

objective <- Minimize(quad_form(weight, cov(Data)))
constraints <- list(weight <= abs_weight, -abs_weight <= weight, sum(abs_weight) <= 1, t(colMeans(Data))%*%weight == mean(colMeans(Data)))
problem <- Problem(objective, constraints = constraints)

weight_cvxr <- solve(problem)
weight_cvxr <- weight_cvxr[[1]]

#answer

answer <-  data.frame(matrix(c(weight_cvxr), nrow = 1, byrow = TRUE))

colnames(answer) <-  c(colnames(Data))
rownames(answer) <-  c("CVXR")

print(answer)
