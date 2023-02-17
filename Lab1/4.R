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
