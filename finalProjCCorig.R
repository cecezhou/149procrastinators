loss = function(act, pred){
  er = act * log(pred) + (1-act)*(log(1-pred))
  ll = -1 * mean(er)
  return (ll)
}

train = read.csv("~/Documents/Stat149/train.csv")
test = read.csv("~/Documents/Stat149/test.csv")
test = na.convert.mean(test)
train = na.convert.mean(train)
train$hd = as.factor(train$hd)
train$party = as.factor(train$party)
train$racename = as.factor(train$racename)

mytrain = na.convert.mean(mytrain)
mytrain = train[1:80000, ]

model = glm(voted ~ gender + cd + hd + age + party 
            + racename + hsonly + mrrg + chldprsnt 
            + cath+ evang + nonchrst + otherchrst
            + days.since.reg, family=binomial, 
            data = mytrain)
summary(model)


inverselogit = function (x){
  return (1 / (1 + exp(-x)))
}
mytest = train[80001:118529, ]

pred = inverselogit(predict(model, mytest))
min(pred)
actual = as.numeric(mytest$voted) - 1
loss(actual, pred)

# hd + party*racename + mrrg + mrrg:gender
mytrain$hd = as.factor(mytrain$hd)
mytrain$party = as.factor(mytrain$party)
mytrain$racename = as.factor(mytrain$racename)

mytest$hd = as.factor(mytest$hd)
mytest$party = as.factor(mytest$party)
mytest$racename = as.factor(mytest$racename)
# worse with log of days.since.reg
model2 = glm(voted ~  hd + mrrg*gender + party*racename + days.since.reg,
            family=binomial, data = mytrain, 
            control = list(maxit = 10000))


pred2 = inverselogit(predict(model2, mytest))

loss(actual, pred2)

# all predictors + 2 interaction
model4valid = glm(voted ~ gender + cd + hd + age + party 
             + racename + hsonly + mrrg + chldprsnt 
             + cath+ evang + nonchrst + otherchrst
             + dbdistance + vccdistance + dbdistance.na
             + days.since.reg + mrrg:gender + party:racename,
             family=binomial, data = mytrain)
pred4valid = inverselogit(predict(model4valid, mytest))
loss(actual, pred4valid)


# Kaggle Submission 1
convertedtest = na.convert.mean(test)
convertedtrain = na.convert.mean(train)
model1 = glm(voted ~ gender + cd + hd + age + party 
            + racename + hsonly + mrrg + chldprsnt 
            + cath+ evang + nonchrst + otherchrst
            + dbdistance + vccdistance + dbdistance.na
            + days.since.reg, family=binomial, 
            data = convertedtrain)
pred1 = inverselogit(predict(model1, convertedtest))
write.csv(pred1, "~/Documents/Stat149/file1.csv")

# Kaggle Submission 2
train$gender = as.factor(train$gender)
train$cd = as.factor(train$cd)
train$hd = as.factor(train$hd)
train$party = as.factor(train$party)
train$racename = as.factor(train$racename)

test$gender = as.factor(test$gender)
test$cd = as.factor(test$cd)
test$hd = as.factor(test$hd)
test$party = as.factor(test$party)
test$racename = as.factor(test$racename)

convertedtest = na.convert.mean(test)
convertedtrain = na.convert.mean(train)
model1a = glm(voted ~ gender + cd + hd + age + party 
             + racename + hsonly + mrrg + chldprsnt 
             + cath+ evang + nonchrst + otherchrst
             + dbdistance + dbdistance.na
             + days.since.reg, family=binomial, 
             data = convertedtrain)
pred2 = inverselogit(predict(model1a, convertedtest))
write.csv(pred2, "~/Documents/Stat149/file2.csv")


# Kaggle Submission 3

model3 = glm(voted ~  hd + mrrg*gender + party*racename + days.since.reg,
             family=binomial, data = convertedtrain, 
             control = list(maxit = 10000))
summary(model3)
pred3 = inverselogit(predict(model3, convertedtest))
write.csv(pred3, "~/Documents/Stat149/file3.csv")

# Kaggle Submission 4/5 (5 is without outlier for G Native American)
convertedtrain 
newtrain = convertedtrain[-96253,]
length(newtrain[,1])

model4 = glm(voted ~ gender + cd + hd + age + party 
             + racename + hsonly + mrrg + chldprsnt 
             + cath+ evang + nonchrst + otherchrst
             + dbdistance + vccdistance + dbdistance.na
             + days.since.reg + mrrg:gender + party:racename,
             family=binomial, data = newtrain)
summary(model4)
pred4 = inverselogit(predict(model4, convertedtest))
write.csv(pred4, "~/Documents/Stat149/file5.csv")
dists = cooks.distance(model4)

plot(dists)
