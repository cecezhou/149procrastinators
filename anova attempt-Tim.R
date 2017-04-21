train_set = "/Users/TimothyKang/Desktop/train.csv"
data = read.csv(train_set, header=T)

summary(data)
# cd,hd, dbdistance, vccdistance   has na
# only two values in cd and hd are N/A - maybe just encode them as that?

# convert relevant things to factors
data$gender = as.factor(data$gender)
data$cd = as.factor(data$cd)
data$hd = as.factor(data$hd)
data$party = as.factor(data$party)
data$racename = as.factor(data$racename)

dat = na.convert.mean(data)

vars <- colnames(dat)
indices <- list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)

# creates every possible combination of variables
combinations <- sapply(indices, function(x) {combn(y=vars[2:19], x)})
formulas <- list()
k=0
# creates all the formulas that we can just use
# THIS PART TAKES FUCKING FOREVER TO RUN BECAUSE MANY COMBINATIONS.
for(i in 1){
  tmp <- combinations[[i]]
  for(j in seq(ncol(tmp))){
    k <- k + 1
    formulas[[k]] <- formula(paste("voted", "~", paste(tmp[,j], collapse=" + ")))
  }
}

sset = dat[sample(nrow(dat), ceiling(nrow(dat) * .60)), ]
base = glm(voted ~ 1, data = sset, family=binomial)

# applies glm to all the formulas
res <- vector(mode="list", length(COMB2))
for(i in seq(formulas)){
  res[[i]] <- glm(formulas[[i]], data=sset, family=binomial)
}

anova(base, res[[1]])
do.call(Curry(anova, object = base), res)

# hd  and mrrg have most significant initial 
