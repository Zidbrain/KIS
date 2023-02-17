
# Первый блок
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

# Второй блок
# 1
commission_count <- 0
one_man_count <- 0
count <- 100000

for (i in 1:count) {
  commission <- runif(4) <= 0.9
  commission[5] <- runif(1) <= 0.5
  if (sum(commission) >= 3) {
    commission_count <- commission_count + 1
  }
  
  if (runif(1) <= 0.5) {
    one_man_count <- one_man_count + 1
  }
}

commission_count / count
one_man_count / count

# 2
count <- 10000
two_shot <- 0

for (i in 1:count) {
  shots <- c(runif(1) <= 0.9, runif(1) <= 0.8, runif(1) <= 0.85)
  if (sum(shots) == 2) two_shot <- two_shot + 1
}

two_shot / count

# 3
count <- 10000
hit <- 0

for (j in 1:count) {
  shots <- c()
  for (i in 1:10) {
    shots[i] <- runif(1) <= 1 / 3
  }
  
  if (sum(shots) == 3) hit <- hit + 1
}

hit / count

# 4
count <- 10000
hits <- 0

for (i in 1:count) {
  girls <- NULL
  for (j in 1:5) {
    girls[j] <- runif(1) <= 0.51
  }
  
  if (sum(girls) == 3) hits <- hits + 1
}

hits / count
