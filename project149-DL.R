train = read.csv("/Users/davidliang/Documents/Semester6/Stat_149/Project/train.csv")
test = read.csv("/Users/davidliang/Documents/Semester6/Stat_149/Project/test.csv")
sample = read.csv("/Users/davidliang/Documents/Semester6/Stat_149/Project/sample-submission.csv")

na.convert.mean = function (frame) {
  vars <- names(frame)
  if (!is.null(resp <- attr(attr(frame, "terms"), "response"))) {
    vars <- vars[-resp]
    x <- frame[[resp]]
    pos <- is.na(x)
    if (any(pos)) {
      frame <- frame[!pos, , drop = FALSE]
      warning(paste(sum(pos), "observations omitted due to missing values in the response"))
    }
  }
  for (j in vars) {  #j is variable names
    x <- frame[[j]]
    pos <- is.na(x)
    if (any(pos)) {
      if (length(levels(x))) {   # factors
        xx <- as.character(x)
        xx[pos] <- "NA"
        x <- factor(xx, exclude = NULL)
      }
      else if (is.matrix(x)) {   # matrices
        ats <- attributes(x)
        x.na <- 1*pos
        #               x[pos] <- 0
        w <- !pos
        n <- nrow(x)
        TT <- array(1, c(1, n))
        xbar <- (TT %*% x)/(TT %*% w)
        xbar <- t(TT) %*% xbar
        x[pos] <- xbar[pos]
        attributes(x) <- ats
        attributes(x.na) <- ats
        dimnames(x.na)[[2]]=paste(dimnames(x)[[2]],".na",sep='')
        frame[[paste(j,".na",sep='')]] <- x.na 
      } else {   # ordinary numerical vector
        ats <- attributes(x)
        x[pos] <- mean(x[!pos])
        #               x[pos] <- 0
        x.na <- 1*pos
        frame[[paste(j,".na",sep='')]] <- x.na 
        attributes(x) <- ats
      }
      frame[[j]] <- x
    }
  }
  frame
}

mytrain = na.convert.mean(train)
mytest = na.convert.mean(test)

#baseline model
baseline = glm(voted ~ gender + cd + hd + age + party + racename + hsonly + mrrg + 
               chldprsnt + cath+ evang + nonchrst + otherchrst + days.since.reg, 
               family=binomial, data=mytrain)

# inverse of logit function
inverselogit = function (x){
  return (1 / (1 + exp(-x)))
}

# converts Y/N to 1/0
to_numeric = function(vec){
  return (as.numeric(vec) - 1)
}

# loss function to compute kaggle score
loss = function(act, pred){
  er = act * log(pred) + (1-act)*(log(1-pred))
  ll = -1 * mean(er)
  return (ll)
}

mytrain.1 = mytrain[1:95000,]
mytrain.2 = mytrain[95001:105000,]
mytrain.3 = mytrain[105001:n,]
n = length(train[,1])
n*0.8



baseline_predict = predict(baseline)
loss(to_numeric(train$voted), inverselogit(baseline_predict))
