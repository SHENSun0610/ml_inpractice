require(plyr)


multicols <- function(df) {
  n <- ncol(df)
  
  if (is.null(n)) {
    df
  } else if (n == 1) {
    df[, 1]
  } else {
    df[, 1] * multicols(df[, 2:n])
  }
}


pluscols <- function(df) {
  n <- ncol(df)
  
  if (is.null(n)) {
    df
  } else if (n == 1) {
    df[, 1]
  } else {
    df[, 1] + pluscols(df[, 2:n])
  }
}


# 计算xcol的条件概率，xcol为离散值
#
# Args:
#   xcol: 特征列
#   ycol: 类别列
#   trainset: 训练集
#
# Returns:
#   xcol的条件概率
get.discrete.xtable <- function(xcol, ycol, trainset) {
  xcount <- ddply(trainset, c(ycol, xcol), each(count = nrow))
  xcount <- ddply(xcount, ycol, transform, ratio = count / sum(count))
  xtable <- xcount[["ratio"]]
  names(xtable) <- paste(xcount[[ycol]], xcount[[xcol]], sep = "")
  xtable
}


# 计算xcol的概率密度，xcol为连续值
#
# Args:
#   xcol: 特征列
#   ycol: 类别列
#   trainset: 训练集
#
# Returns:
#   xcol的高斯分布概率密度
get.continout.xdensity <- function(xcol, ycol, trainset) {
  xmeansd <- ddply(trainset, ycol, function(x) c(mean = mean(x[[xcol]]), sd = sd(x[[xcol]])))
  mean <- xmeansd[["mean"]]
  names(mean) <- xmeansd[[ycol]]
  sd <- xmeansd[["sd"]]
  # 当某个分类下所有特征值相同时，付给sd一个极小值，因为高斯分布sd为0，密度值为无穷
  sd[sd <= 0] <- 0.0001
  names(sd) <- xmeansd[[ycol]]
  xtable <- list(mean = mean, sd = sd)
  xtable
}


# 计算各个类别的概率
#
# Args:
#   ycol: 类别列
#   trainset: 训练集
#
# Returns:
#   各个类别的概率
get.ytable <- function(ycol, trainset) {
  ycount <- ddply(trainset, ycol, each(count = nrow))
  ycount$ratio <- ycount$count / nrow(trainset)
  ytable <- ycount[["ratio"]]
  names(ytable) <- ycount[[ycol]]
  ytable
}


# Laplace校准，当某个类别下某个条件值概率为0时为其设置默认值
#
# Args:
#   ycol: 类别列
#   trainset: 训练集
#
# Returns:
#   各个类别的Laplace校准概率值
get.defaultx <- function(ycol, trainset, laplace) {
  ycount <- ddply(trainset, ycol, each(count = nrow))
  defaultx <- laplace / ycount[["count"]]
  names(defaultx) <- ycount[[ycol]]
  defaultx
}


# 将训练集的值转换为相应的条件概率
#
# Args:
#   testset: 测试集
#   discrete.xtable: 离散值得条件概率
#   continous.xdensity: 连续值得高斯分布概率密度
#   defaultx: Laplace校准概率
#
# Returns:
#   长度等于类别数的列表，列表的每个元素都是一个条件概率组成的矩阵
testset2plist <- function(testset, discrete.xtable, continous.xdensity, defaultx) {
  n <- ncol(testset)
  cnames <- names(testset)
  names(cnames) <- cnames
  xcols <- cnames[1:n-1]
  
  ylevels <- names(defaultx)
  names(ylevels) <- ylevels
  
  plist <- lapply(ylevels, function(y) {
    pm <- do.call("cbind",lapply(xcols, function(xn) {
      if (is.numeric(testset[[xn]])) {
        mean <- continous.xdensity[[xn]][["mean"]][[y]]
        sd <- continous.xdensity[[xn]][["sd"]][[y]]
        p <- dnorm(testset[[xn]], mean, sd)
        p[is.na(p)] <- 0
        p
      } else {
        index <- paste(y, testset[[xn]], sep = "")
        p <- discrete.xtable[[xn]][index]
        p[is.na(names(p))] <- defaultx[[y]]
        p 
      }

    }))
    unname(pm)
  })
  plist

}


# 对上面各个方法的封装
bayesian <- function(trainset, laplace = 1) {
  colname <- names(trainset)
  n <- ncol(trainset)
  xcols <- colname[1:n-1]
  ycol <- colname[n]
  if (is.numeric(trainset[ycol])) stop("ycol必须为离散值！")
  continous.xcols <- xcols[sapply(xcols, function(c) is.numeric(trainset[, c]))]
  discrete.xcols <- setdiff(xcols, continous.xcols)
  
  discrete.xtable <- lapply(discrete.xcols, get.discrete.xtable, ycol, trainset)
  names(discrete.xtable) <- discrete.xcols
  continous.xdensity <- lapply(continous.xcols, get.continout.xdensity, ycol, trainset)
  names(continous.xdensity) <- continous.xcols
 
  ytable <- get.ytable(ycol, trainset)
  defaultx <- get.defaultx(ycol, trainset, laplace)
  
  list(cxtable = continous.xdensity, dxtable = discrete.xtable, ytable = ytable, defaultx = defaultx)
}


# 根据测试集的已知条件预测类别值
#
# Args:
#   testset: 测试集
#   bayesian: bayesian函数返回的结果
#
# Returns:
#   测试集的预测类别值
forecast <- function(testset, bayesian) {
  plist <- testset2plist(testset, bayesian[["dxtable"]], 
                         bayesian[["cxtable"]], bayesian[["defaultx"]])
  
  pmatrix <- do.call("rbind", lapply(plist, multicols)) * bayesian[["ytable"]]
  pmatrix <- t(pmatrix)
  
  forecast <- colnames(pmatrix)[max.col(pmatrix)]
  forecast

}



