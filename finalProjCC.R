train = read.csv("~/Documents/Stat149/train.csv")
test = read.csv("~/Documents/Stat149/test.csv")
convertedtest = na.convert.mean(test)
convertedtrain = na.convert.mean(train)
convertedtrain$voted
loss = function(act, pred){
  er = act * log(pred) + (1-act)*(log(1-pred))
  ll = -1 * mean(er)
  return (ll)
}

inverselogit = function (x){
  return (1 / (1 + exp(-x)))
}
mytrain = convertedtrain[30001:118529, ]
mytest = convertedtrain[1:30000,]
actual = as.numeric(mytest$voted) - 1
########################
#### GLM
library(MASS)
modelbase = glm(voted ~ gender + cd + hd + age + party 
            + racename + hsonly + mrrg + chldprsnt 
            + cath + otherchrst
            + days.since.reg, family=binomial, 
            data = mytrain)
summary(modelbase)
pred = inverselogit(predict(modelbase, mytest))
loss(actual, pred)

trainmodel1 = glm(voted ~ gender + cd + hd + age + party *racename
                  + hsonly + mrrg + chldprsnt 
                  + cath + otherchrst
                  + dbdistance + vccdistance + dbdistance.na
                  + days.since.reg, family=binomial, 
                  data = mytrain)
pred = inverselogit(predict(trainmodel1, mytest))
loss(actual, pred)
###### GAM
library(mgcv)
library(faraway)
library(rpart)
library(rpart.plot)
gam1 = gam(voted ~ gender + cd + hd + s(age) + party *racename
           + s(hsonly) + s(mrrg) + s(chldprsnt) 
           + s(cath) + s(otherchrst)
           + s(dbdistance) + s(vccdistance) + dbdistance.na
           + days.since.reg, family=binomial, 
           data = mytrain)
predictions2 = predict(gam1, test_set, type = "response")
binpred2 = ifelse(predictions2 < 0.5,0,1)


### Tree -- completely doesn't work

tree1 = rpart(voted ~ gender + hd
              + dbdistance + vccdistance + dbdistance.na
              + days.since.reg, data=mytrain)
prp(tree1, main="Default Tree Model")
treepred1 = predict(tree1, newdata = mytest)
bintreepred1 = ifelse(treepred1 < 0.5,0,1)
results4 = (bintreepred1 == test_set$test)
length(results4[results4==TRUE])

printcp(tree1)
tree2 = prune(tree1, cp=0.01000)
prp(tree2, type=0,digits=0,
    main="Pruned Tree Model")


# Kaggle Submission 1

model1 = glm(voted ~ gender + cd + hd + age + party 
             + racename + hsonly + mrrg + chldprsnt 
             + cath + otherchrst
             + dbdistance + vccdistance + dbdistance.na
             + days.since.reg, family=binomial, 
             data = convertedtrain)
pred1 = inverselogit(predict(model1, convertedtest))
write.csv(pred1, "~/Documents/Stat149/file.csv")

### RF
library(randomForest)

cvr = rfcv(train, train$voted,step=3)
cbind(nvars=cvr$n.var, error.rate=cvr$error.cv)
rf1 = randomForest(voted ~ gender + cd + hd + age + party 
                      + racename + hsonly + mrrg + chldprsnt 
                      + cath + otherchrst
                      + dbdistance + vccdistance 
                      + days.since.reg, 
                      data = train, mtry = 4, na.action = na.roughfix)

pred1 = predict(rf1, convertedtest, type='prob')
pred = pred1[,"Y"]
write.csv(pred, "~/Documents/Stat149/predrf.csv")

