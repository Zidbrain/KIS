# Задание 1

M_T <- function(X, P) { return(sum(X * P)) }
D_T <- function(X, P) { M <- M_T(X, P); return(sum(P * (X - M)^2)) }

getValue <- function(X, P) {
  y <- runif(1)
  sum <- 0
  res <- 0
  for (i in 1:length(P)) {
    sum <- sum + P[i]
    if (y <= sum) {
      res <- X[i]
      break
    }
  }
  return(res)
}

Variant<-14
set.seed(Variant) 
X1<-sample(c(1:20),5)
X2<-sample(c(3:100),5)
X3<-sample(c(0:40),5)
pp1<-runif(5)
p1<-pp1/sum(pp1)
pp2<-runif(5)
p2<-pp2/sum(pp2)
pp3<-runif(5)
p3<-pp3/sum(pp3)

M <- M_T(X1, p1) + M_T(X2, p2) + M_T(X3, p3)
D <- D_T(X1, p1) + D_T(X2, p2) + D_T(X3, p3)
sd <- sqrt(D)
k <- sd / M * 100

res <- data.frame(M, D, sd, k)
View(res)

actual <- c()
for (i in 1:100000) {
  actual <- c(actual, getValue(X1, p1) + getValue(X2, p2) + getValue(X3, p3))
}

M <- mean(actual)
sd <- sd(actual)
D <- sd^2
k <- sd / M * 100
res <- rbind(res, c(M, D, sd, k))

#install.packages("polynom")
library(polynom)
pgf <- function(X, P) {
  res <- sample(0, max(X) + 1, replace = TRUE)
  for (i in 1:length(P)) {
    res[X[i] + 1] <- P[i]
  }
  return(polynomial(coef = res))
}

pgf_X = pgf(X1, p1) * pgf(X2, p2) * pgf(X3, p3)

M <- as.function(deriv(pgf_X))(1)
D <- as.function(deriv(deriv(pgf_X)))(1) + M - M^2
sd <- sqrt(D)
k <- sd / M * 100
res <- rbind(res, c(M, D, sd, k))

rownames(res) <- c("theory", "actual", "generating function")