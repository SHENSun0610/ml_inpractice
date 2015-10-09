require(plyr)


multicols <- function(df) {
  n <- ncol(df)
  
  if (is.null(n)) {
    df
  }
  else if (n == 1) {
    df[, 1]
  } else {
    df[, 1] * multicols(df[, 2:n])
  }
}


pluscols <- function(df) {
  n <- ncol(df)
  
  if (is.null(n)) {
    df
  }
  else if (n == 1) {
    df[, 1]
  } else {
    df[, 1] + pluscols(df[, 2:n])
  }
}


# 1.计算离散x的条件概率表
get.discrete.xtable <- function(xcol, ycol, trainset) {
  xcount <- ddply(trainset, c(ycol, xcol), each(count = nrow))
  xcount <- ddply(xcount, ycol, transform, ratio = count / sum(count))
  xtable <- xcount[["ratio"]]
  # xratio<- as.vector(xratio)
  names(xtable) <- paste(xcount[[ycol]], xcount[[xcol]], sep = "")
  xtable
}

get.continout.xdensity <- function(xcol, ycol, trainset) {
  xmeansd <- ddply(trainset, ycol, function(x) c(mean = mean(x[[xcol]]), sd = sd(x[[xcol]])))
  mean <- xmeansd[["mean"]]
  names(mean) <- xmeansd[[ycol]]
  sd <- xmeansd[["sd"]]
  sd[is.na(sd)] <- Inf
  names(sd) <- xmeansd[[ycol]]
  xtable <- list(mean = mean, sd = sd)
  
  xtable
}

# 2.计算各个y的概率表
get.ytable <- function(ycol, trainset) {
  ycount <- ddply(trainset, ycol, each(count = nrow))
  ycount$ratio <- ycount$count / nrow(trainset)
  ytable <- ycount[["ratio"]]
  names(ytable) <- ycount[[ycol]]
  ytable
}

# 3.当某个分类中没有相应值时设置默认值
get.defaultx <- function(ycol, trainset) {
  ycount <- ddply(trainset, ycol, each(count = nrow))
  defaultx <- 1 / ycount[["count"]]
  names(defaultx) <- ycount[[ycol]]
  defaultx
}


# 4.将测试集数据转换为相应的条件概率
# 查找xcol列值分别为x和y的条件概率值
testset2plist <- function(testset, discrete_xtable, continous.xdensity, defaultx) {
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
        p
      } else {
        index <- paste(y, testset[[xn]], sep = "")
        p <- discrete_xtable[[xn]][index]
        p[is.na(names(p))] <- defaultx[[y]]
        p 
      }
      
    }))
    unname(pm)
  })
  plist

}


bayesian <- function(trainset) {
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
  defaultx <- get.defaultx(ycol, trainset)
  
  list(cxtable = continous.xdensity, dxtable = discrete.xtable, ytable = ytable, defaultx = defaultx)
}


prodict <- function(testset, bayesian) {
  plist <- testset2plist(testset, bayesian[["dxtable"]], 
                         bayesian[["cxtable"]], bayesian[["defaultx"]])
  
  pmatrix <- do.call("rbind", lapply(plist, multicols)) * bayesian[["ytable"]]
  # pmatrix <- do.call("rbind", lapply(plist, function(x)pluscols(log10(x)))) +
    # log10(bayesian[["ytable"]])
  pmatrix <- t(pmatrix)
  
  forecast <- colnames(pmatrix)[max.col(pmatrix)]
  forecast

}



