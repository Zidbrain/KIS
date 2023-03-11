Variant<-12
set.seed(Variant) 
k<-sample(c(10:25),1)
t1<-sample(c(14:20),1)
t2<-sample(c(2:5),1)

k <- 2
a1 <- t1^-1
a2 <- t2^-1

p <- k * a1 / a2

results <- data.frame(theory = numeric(3), experiment = numeric(3))
rownames(results) <- c("Вероятность что программа не будет выполнена сразу",
                       "Среднее время ожидания до реализации",
                       "Среднее количество программ ожидающих реализации")
results[, 1] = c(p, p^2/(k * a1*(1-p)) + t2, p^2/(1-p))

events <- matrix(nrow = 3)
events <- events[, -1]
N <- 100000
currentTime <- 0
queue <- c()
serverBusy <- FALSE
programmersWaiting <- sample(c(FALSE), k, replace = TRUE)

queueStat <- c()
JITAddStat <- c()
waitTime <- c()

createEvent <- function(events, currentTime, rate, index, programTime) {
  time <- rexp(1, rate)
  events <- cbind(events, c(index, currentTime + time, programTime))
  return(events)
} 

for (i in 1:N) {
  for (j in 1:length(programmersWaiting)) {
    if (!programmersWaiting[j]) {
      events <- createEvent(events, currentTime, a1, j, currentTime)
      programmersWaiting[j] <- TRUE
    }
  }
  events <- events[,order(events[2, ])]
  
  event <- events[,1]
  events <- events[,-1]
  currentTime <- event[2]
  
  if (event[1] == 0) {
    if (length(queue) == 0)
      serverBusy <- FALSE 
    else {
      programTime <- queue[1]
      queue <- queue[-c(1)]
      events <- createEvent(events, currentTime, a2, 0, programTime)
    }
    waitTime <- rbind(waitTime, currentTime - event[3])
    queueStat <- c(queueStat, length(queue))
  } else {
    programmersWaiting[event[1]] <- FALSE
    if (serverBusy) {
      queue <- c(queue, currentTime)
      JITAddStat <- c(JITAddStat, 1)
    }
    else {
      serverBusy <- TRUE
      events <- createEvent(events, currentTime, a2, 0, currentTime)
      JITAddStat <- c(JITAddStat, 0)
    }
  }
}

results[1, 2] = mean(JITAddStat)
results[2, 2] = mean(waitTime)
results[3, 2] = mean(queueStat)
View(results)