rm(list = ls())


library(e1071)
source('R/bayesian.R', encoding = "UTF-8")
load("data/houses_30000.RData")


shoot <- function(trainsize, testsize, data, times = 5) {
  num.basket <- 1:nrow(data)
  ratio <- 0
  ratio <- rep(0, times)
  ratio2 <- 0
  ratio2 <- rep(0, times)
  for (i in seq_len(times)) {
    sel <- sample(num.basket, trainsize)
    sel2 <- sample(num.basket, testsize)
    
    training.set <- data[sel, ]
    test.set <- data[sel2, ]
    
    b <- bayesian(training.set)
    result <- forecast(test.set, b)
    b2 <- naiveBayes(training.set[, 1:ncol(training.set)-1], training.set[, ncol(training.set)])
    result2 <- predict(b2, test.set[, 1:ncol(test.set)-1])
    hit <- test.set[, ncol(test.set)] == result
    ratio[i] <- sum(hit) / testsize
    hit2 <- test.set[, ncol(test.set)] == result2
    ratio2[i] <- sum(hit2) / testsize
  }
  c(mean(ratio), mean(ratio2))
}


test <- function(minshots, maxshots, len, data) {
  shots <- seq.int(minshots, maxshots, length.out = len)
  ratio <- rep(0, len)
  ratio2 <- rep(0, len)
  for (i in seq_along(shots)) {
    cat("=")
    r <- shoot(shots[i], 30, data, 3)
    ratio[i] <- r[1]
    ratio2[i] <- r[2]
  }
  plot(shots, ratio, type = "l", ylim = c(0, 1), xlab = "训练集样本数", ylab = "正确率")
  lines(shots, ratio2, lty = 3, col = "red")
  legend("bottomright",legend = c("mine", "e1071"), 
         col = c("black", "red"), lty = c(1,3), 
         cex = 0.8)
}

