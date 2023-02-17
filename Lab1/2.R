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
