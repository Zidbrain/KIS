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
