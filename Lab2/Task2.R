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