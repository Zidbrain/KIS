count <- 10000
two_shot <- 0

for (i in 1:count) {
  shots <- c(runif(1) <= 0.9, runif(1) <= 0.8, runif(1) <= 0.85)
  if (sum(shots) == 2) two_shot <- two_shot + 1
}

two_shot / count
