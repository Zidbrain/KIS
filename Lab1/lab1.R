data <- read.csv("https://raw.githubusercontent.com/junaart/ForStudents/master/KIS/Lab_1/Computers.csv", sep = ",")
c_min <- c(4, 15, 7, 2)
c_max <- c(9, 33, 11, 5)

View(data)

all_cost_max <- 180000
cost_max <- c(30000, 120000, 60000, 30000)
min_speed <- c(250, 300) # N1 * speed1 + N1 * ram1 >= 100 & N2 * speed2 + N2 * ram2 >= 80

res<-lm(price~speed+ram+hd, data)
cf <- res$coefficients
x_min <- c(mean(data$speed), mean(data$ram), mean(data$hd))

W1 <- function(x2, x3) { return(0.6 * x2 + 0.4 * x3) }
W2 <- function(x6, x7) { return(0.75 * x6 + 0.25 * x7) }
H1 <- function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) {
  return(x1 + cf[2] * x2 + cf[3] * x3 + cf[4] * x4 + 
           x5 + cf[2] * x6 + cf[3] * x7 + cf[4] * x8 +
           x9 + cf[2] * x10 + cf[3] * x11 + cf[4] * x12 +
           x13 + x14 * cf[2] + x15 * cf[3] + x16 * cf[4])
}
H2 <- function(x2, x3, x7, x8) { return(0.5 * W1(x2,x3) + 0.5 * W2(x7, x8)) }

#install.packages("lpSolve")
library(lpSolve)
objective.in <- c(1, cf[2], cf[3], cf[4],
                  1, cf[2], cf[3], cf[4],
                  1, cf[2], cf[3], cf[4],
                  1, cf[2], cf[3], cf[4])
const.mat <- matrix(c(0, 0.3, 0.2, 0, 0, 0.375, 0.125, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      1, cf[2], cf[3], cf[4], 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 1, cf[2], cf[3], cf[4], 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 1, cf[2], cf[3], cf[4], 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, cf[2], cf[3], cf[4],
                      1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                      0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                      nrow = 39, byrow=TRUE)
const.dir <- c(">=", "<=", "<=", "<=", "<=", 
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", "<=",
               ">=", ">=")
const.rhs <- c(100, cost_max[1], cost_max[2], cost_max[3], cost_max[4],
               c_min[1] * cf[1], c_max[1] * cf[1],
               c_min[1] * x_min[1], c_max[1] * x_min[1],
               c_min[1] * x_min[2], c_max[1] * x_min[2],
               c_min[1] * x_min[3], c_max[1] * x_min[3],
               c_min[2] * cf[1], c_max[2] * cf[1],
               c_min[2] * x_min[1] + 10, c_max[2] * x_min[1] + 10,
               c_min[2] * x_min[2], c_max[2] * x_min[2],
               c_min[2] * x_min[3] + 300, c_max[2] * x_min[3] + 300,
               c_min[3] * cf[1], c_max[3] * cf[1],
               c_min[3] * x_min[1] - 15, c_max[3] * x_min[1] - 15,
               c_min[3] * x_min[2], c_max[3] * x_min[2],
               c_min[3] * x_min[3] + 500, c_max[3] * x_min[3] + 500,
               c_min[4] * cf[1], c_max[4] * cf[1],
               c_min[4] * x_min[1] - 20, c_max[4] * x_min[1] - 20,
               c_min[4] * x_min[2], c_max[4] * x_min[2],
               c_min[4] * x_min[3], c_max[4] * x_min[3],
               min_speed[1], min_speed[2]
               )
Res <- lp("min", objective.in, const.mat, const.dir, const.rhs)
x <- Res$solution

# Здесь ошибка в числе с плавающей запятой - floor выдает 6 (хотя должен 7), поэтому + 1
N <- c(floor(x[1] / cf[1]), floor(x[5] / cf[1]), floor(x[9] / cf[1]) + 1, floor(x[13] / cf[1]))
speed <- c(x[2] / N[1], x[6] / N[2], x[10] / N[3], x[14] / N[4])
ram <- c(x[3] / N[1], x[7] / N[2], x[11] / N[3], x[15] / N[4])
hd <- c(x[4] / N[1], x[8] / N[2], x[12] / N[3], x[16] / N[4])

DPareto<-function(X,Y){
  p<-TRUE; l<-FALSE; i<-1;
  while (p & (i<=length(X))){
    if (X[i] < Y[i]) p<-FALSE
    if (X[i] > Y[i]) l<-TRUE
    i<-i+1
  }
  if (!p | !l) return(FALSE)
  else return(TRUE)
} 

NewW<-data.frame()
for (i in c(1:length(rownames(data))))
{
  if (data[i, 7] == "yes" & data[i, 4] >= 100 & data[i, 2] <= 2700 & data[i, 6] >= 15 & data[i, 8] == 'yes')
    NewW<-rbind(NewW,data[i,])
}
View(NewW)

mmax <- max(NewW$price); mmin <- min(NewW$price); norm_price <- (mmax - NewW$price) * 100 / (mmax - mmin)
mmax <- max(NewW$speed); mmin <- min(NewW$speed); norm_speed <- (NewW$speed - mmin) * 100 / (mmax - mmin)
mmax <- max(NewW$ram); mmin <- min(NewW$ram); norm_ram <- (NewW$ram - mmin) * 100 / (mmax - mmin)
mmax <- max(NewW$hd); mmin <- min(NewW$hd); norm_hd <- (NewW$hd - mmin) * 100 / (mmax - mmin)
WW <- data.frame(norm_price, norm_speed, norm_ram, norm_hd)
rownames(WW) <- rownames(NewW)

result<-c()
for (i in c(1:length(rownames(WW)))){
  p<-TRUE
  for (j in c(1:length(rownames(WW)))){
    if (DPareto(WW[j,],WW[i,]))
      p<-FALSE}
  if (p) result<-c(result, rownames(WW)[i])
}
result

ideal<-data.frame(min(NewW$price), speed, ram, hd)
ideal

distance<-function(A, B){
  return(sqrt(sum((A-B)^2)))}

for (i in 1:4) {
  cat(paste("Для С",  i, ": количество компьютеров -", N[i]))
  results <- data.frame(index = numeric(), distance = numeric())
  for (j in result) {
    results <- rbind(results, c(j, distance(ideal[i, ], WW[j, ])))
  }
  best_result <- min(results[, 2])
  print(paste(", лучший комьютер:", results[results[, 2] == best_result, 1], "с расстоянием до идеального -", best_result))
}

# Получаем: 
# С1: 4 компьютера - 5827 или 5881
# С2: 15 компьютеров - 5827 или 5881
# С3: 7 компьютеров - 5827 или 5881
# C4: 2 компьютера - 6168