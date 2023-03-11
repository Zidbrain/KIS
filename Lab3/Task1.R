#install.packages("matrixcalc")
library(matrixcalc)

Variant<-12
set.seed(Variant) 
k<-sample(c(4:9),1)
pp1<-runif(4)
pp2<-runif(3)
pp3<-runif(2)
p1<-pp1/sum(pp1)
p2<-c(c(0),pp2/sum(pp2))
p3<-c(c(0,0),pp3/sum(pp3))
p4<-c(0,0,0,1)
P<-data.frame()
P<-rbind(P,p1)
P<-rbind(P,p2)
P<-rbind(P,p3)
P<-rbind(P,p4)
rownames(P)<-c("p1","p2","p3","p4")
colnames(P)<-c("","","","")
View(P)
print(paste("k=",as.character(k)))

getNextState <- function(pVector) {
  r <- runif(1)
  sum <- 0
  for (i in 1:length(pVector)) {
    sum <- sum + pVector[i]
    if (r <= sum) return(i)
  }
}

resultExperiment <- data.frame(p1 = c(0, 0, 0), p2 = c(0, 0, 0), p3 = c(0, 0, 0), p4 = c(0, 0, 0))
n <- 10000
for (i in 1:n) {
  state <- 1
  for (j in 1:k) {
    state <- getNextState(P[state, ])
    if (j == k - 2) resultExperiment[1, state] <- resultExperiment[1, state] + 1
    else if (j == k - 1) resultExperiment[2, state] <- resultExperiment[2, state] + 1
    else if (j == k) resultExperiment[3, state] <- resultExperiment[3, state] + 1
  }
}

resultExperiment <- resultExperiment / n
rownames(resultExperiment) <- c("k - 2", "k - 1", "k")

resultTheory <- data.frame(p1 = c(0,0,0), p2 = c(0,0,0), p3 = c(0,0,0), p4 = c(0,0,0))

mat <- as.matrix(P)
resultTheory[1, ] <- c(1, 0, 0, 0) %*% matrix.power(mat, k - 2)
resultTheory[2, ] <- c(1, 0, 0, 0) %*% matrix.power(mat, k - 1)
resultTheory[3, ] <- c(1, 0, 0, 0) %*% matrix.power(mat, k)
rownames(resultTheory) <- c("k - 2", "k - 1", "k")

View(resultTheory)
View(resultExperiment)