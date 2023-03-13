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

Variant<-12
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

rownames(res) <- c("theory", "experiment", "generating function")

# Задание 2

Variant <- 12
set.seed(Variant)
a <- runif(3)
n <- 10000

M <- sum(a)
D <- sum(a)
sd <- sqrt(D)
k <- sd / M

res <- data.frame(M, D, sd, k)

X <- rpois(n, a[1]) + rpois(n, a[2]) + rpois(n, a[3])
M <- mean(X)
sd <- sd(X)
res <- rbind(res, c(M, sd^2, sd, sd / M))
rownames(res) <- c("theory", "experiment")
View(res)

# Задание 3

Variant<-12
set.seed(Variant)
alpha<-runif(7)[7]
alpha

# Преобразование Лапласа - Стилтьеса для экспоненциального распределения F{X < x} = 1 - e^-ax:
# b(s) = a/(s + a)
# По свойству 9: B(f1(t) + f2(t)) = B(f1(t)) * B(f2(t)) =>
# Преобразование Лапласа - Стилтьеса для суммы k экспоненциально распределенных случайных велечин:
# b(s) = a^k/(s + a)^k
# Математическое ожидание такой суммы:
# M(F) = -b'(0) = ka^k/(s+a)^(k+1)|s=0 = k/a
# Дисперсия:
# D(F) = b''(0) - (b'(0))^2 = k(k+1)a^k/(a+s)^(k+2)|s=0 - (k/a)^2 = (k^2+k)/a^2 - k^2/a^2 = k/a^2

k <- 10
M <- k / alpha
D <- k / alpha^2

res <- data.frame(M, D)

n <- 100000
X <- 0
for (i in 1:k) { X <- X + rexp(n, rate=alpha) }

M <- mean(X)
D <- sd(X)^2

res <- rbind(res, c(M, D))
rownames(res) <- c("theory", "experiment")
View(res)

# Задание 4

# Пусть есть две случайные велечины u1, u2 распределенные по закону Пуассона с параметрами a1, a2
# Тогда производящая функция суммы двух этих велечин имеет вид:
# Pu(z) = Pu1(z)*Pu2(z) = e^(-a1*(1-z))*e^(-a2*(1-z))=e^((a1+a2)(1-z))
# Данная функция является производящей функцией для случайного распределения распределенного по закону Пуассона
# С параметром a1 + a2, используя свойство ассоциативности сложения получаем что сумма произвольного количества
# Случайных величин распределенных по закону Пуассона также будет распределена по закону Пуассона

Variant <- 12
set.seed(Variant)
k <- 10
a <- runif(k)

M <- sum(a)
D <- sum(a)

res <- data.frame(M, D)

n <- 100000
x <- rpois(n, a[1])
for (i in 2:k){
  x <- x + rpois(n, a[i])
}

M <- mean(x)
D <- sd(x)^2
res <- rbind(res, c(M, D))
rownames(res) <- c("theory", "experiment")
View(res)

# Задание 5

# Плотность вероятности: t(x) = a^2*x*e^(-ax)
# Мат ожидание: M(t) = 1 / a
# Дисперсия: D(t) = 1 / a^2

Variant<-12
set.seed(Variant)
alpha<-runif(100)[77]
alpha

M <- 2 / alpha
D <- 2 / alpha^2

res <- data.frame(M, D)

n <- 100000
x <- rexp(n, alpha) + rexp(n, alpha)

M <- mean(x)
D <- sd(x)^2
res <- rbind(res, c(M, D))
rownames(res) <- c("theory", "experiment")
View(res)