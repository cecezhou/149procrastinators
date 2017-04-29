train = read.csv("~/Desktop/train.csv")
test = read.csv("~/Desktop/test.csv")
mytrain = train[1:80000, ]
mytrain = na.convert.mean(mytrain)
model = glm(voted ~ gender + cd + hd + age + party 
            + racename + hsonly + mrrg + chldprsnt 
            + cath+ evang + nonchrst + otherchrst
            + days.since.reg, family=binomial, 
            data = mytrain)
model = randomForest(voted ~ gender + cd + hd + age + party 
                     + racename + hsonly + mrrg + chldprsnt 
                     + cath+ otherchrst
                     + days.since.reg, family=binomial, 
                     data = mytrain)
loss = function(act, pred){
  er = act * log(pred) + (1-act)*(log(1-pred))
  ll = -1 * mean(er)
  return (ll)
}

inverselogit = function (x){
  return (1 / (1 + exp(-x)))
}
mytest = train[80001:118529, ]

pred = inverselogit(predict(model, mytest))
min(pred)
actual = as.numeric(mytest$voted) - 1
loss(actual, pred)


summary(train)
summary(test)


# Kaggle Submission 1
convertedtest = na.convert.mean(test)
convertedtrain = na.convert.mean(train)
model1 = randomForest(voted ~ gender + cd + hd + age + party 
                      + racename + hsonly + mrrg + chldprsnt 
                      + cath + otherchrst
                      + dbdistance + vccdistance + dbdistance.na
                      + days.since.reg, family=binomial, 
                      data = convertedtrain, type)

pred1 = predict(model1, convertedtest, type='prob')
pred = pred1[,"Y"]
write.csv(pred, "~/Desktop/file.csv")
