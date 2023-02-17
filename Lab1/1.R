x <- runif(1000, 1, 1000000)
y <- sample(1:500, 500)

hmean <- function(x) {
  return(sum(x^(-1))/length(x))^(-1)
}

median(x)
mean(y) - hmean(y)
x[x >= 5 & x < 7000]
y[y %% 2 == 0]
save(x, y, file = "xxx")