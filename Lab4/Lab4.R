Variant<-12
set.seed(Variant) 
k<-sample(c(10:25),1)
m<-sample(c(3:6),1)
t1<-sample(c(14:20),1)
t2<-sample(c(2:5),1)
View(data.frame(k,m,t1,t2))

results <- data.frame(theory = numeric(3))
rownames(results) <- c("вероятность того, что программа не будет выполнена сразу же",
                       "среднее время до получения пользователем результатов реализации",
                       "среднее количество программ, ожидающих выполнения на сервере")

get_P <- function(j, m, k) {
  if (j <= m) {
    return(choose(k,j) * a^j / mu^j);
  } 
  return(choose(k,m) * prod(c((k-m):(k-j+1))) * a^(j) / mu^(j) / m ^(j-m))
} 

P_0 <- 0
for (j in c(0:k)) {
  P_0 <- P_0 + get_P(j, m, k)
}

P_0 <- 1 / P_0



results[1] <- 