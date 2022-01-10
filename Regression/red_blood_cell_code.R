
#################################  ##########################################

athlete = read.csv("~/LocalDocuments/UCDavis/Year2/STA108/datasets/athlete.csv")
names(athlete)
#rcc = Y
names(athlete) = c("Y","X1","X2","X3","X4","X5","X6")
View(athlete)
athlete$X5 = as.factor(athlete$X5)
athlete$X6 = as.factor(athlete$X6)
summary(athlete)
################################# plot Y vs Xs ##########################################
library(ggplot2)
# rcc vs lbm
ggplot(athlete,aes(x= X1, y=Y))+
  geom_point(shape = 19)+
  geom_smooth(method='lm',se= FALSE)+
  ylab("Red Blood Cell (L)") + 
  xlab("lean body mass (kg)")

#Red blood cell count vs body mass index
ggplot(athlete,aes(x= X2, y=Y))+
  geom_point(shape = 19)+
  geom_smooth(method='lm',se= FALSE)+
  ylab("Red Blood Cell (L)") + 
  xlab("BMI (kg)")

ggplot(athlete, aes(y=Y, x = factor("X2")))+ 
  geom_boxplot()+ 
  ylab("Red blood cell count (per liter)") + 
  xlab("bmi (kg)")+ coord_flip() + ggtitle("Y vs X2")

#### rcc vs pbf
ggplot(athlete,aes(x= X3, y=Y))+
  geom_point(shape = 19)+
  geom_smooth(method='lm',se= FALSE)+
  ylab("Red Blood Cell (L)") + 
  xlab("Percent Body Fat")

ggplot(new.athlete, aes(y=Y, x = factor("X3")))+ 
  geom_boxplot() + 
  ylab("Red blood cell count(per liter)") + 
  xlab("pcBfat")+ coord_flip() + ggtitle(" Y vs X3")


#### rcc vs plasma fer
ggplot(athlete,aes(x= X4, y=Y))+
  geom_point(shape = 19)+
  geom_smooth(method='lm',se= FALSE)+
  ylab("Red Blood Cell (L)") + 
  xlab("Palsma ferritins")

ggplot(new.athlete, aes(y=Y, x = factor("X4")))+ 
  geom_boxplot() + 
  ylab("Red blood cell (L)") + 
  xlab("plasma ferritins")+ coord_flip() + ggtitle(" Y vs X3")


##### rcc vs sex & lean body mass
ggplot(athlete,aes(y = Y, x =X1, colour=X5)) + 
  geom_point() + geom_smooth(method="lm",fill = NA)+ 
  scale_color_discrete(name = "Gender", labels = c("Male", "Female"))+
  ylab("Red blood cell (L)") + 
  xlab("Lean Body Mass (kg)")+ ggtitle("Y vs X5")

ggplot(athlete,aes(y = Y, x = X5)) + 
  geom_boxplot()

##### rcc vs sport & lean body mass
ggplot(athlete,aes(y = Y, x =X1, colour=X6)) + 
  geom_point() + geom_smooth(method="lm",fill = NA)+ 
  scale_color_discrete(name = "Sport", labels = c("Net", "Run", "Swim"))+
  ylab("Red blood cell (L)") + 
  xlab("Lean Body Mass (kg)")+ ggtitle("Y vs X5")

ggplot(athlete,aes(y = Y, x = X6)) + xlab("Sport")+ylab("red blood cell count")+
  geom_boxplot()+coord_flip()
################################# regression model##########################################

athlete.model = lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data =athlete)
athlete.model

################################# model selection##########################################
library(MPV)


All.Models = c("Y~X1","Y~X1+X2","Y~X1+X3","Y~X1+X4","Y~X1+X5","Y~X1+X6", 
               "Y~X2","Y~X2+X3","Y~X2+X4","Y~X2+X5","Y~X2+X6",
               "Y~X3","Y~X3+X4","Y~X3+X5","Y~X3+X6",
               "Y~X4","Y~X4+X5","Y~X4+X6","Y~X5","Y~X5+X6","Y~X6",
               "Y~X1+X2+X3+X4+X5+X6","Y~X1+ X2 + X3", "Y~X1+ X2 + X4",
               "Y~X1+ X2 + X5","Y~X1+ X2 + X6", "Y~X2+X3+X4","Y~X2+X3+X4",
               "Y~X2+X3+X5", "Y~X2+X3+X6", "Y~X3+X4+X5", "Y~X3+X4+X6", 
               "Y~X4+X5+X6")

all.model.crit = t(sapply(All.Models,function(M){
  current.model = lm(M,data = athlete)
  All.Criteria(current.model)
}))
criterions = data.frame(round(all.model.crit,4))
which.min(criterions$BIC)
criterions[20,]

which.min(criterions$AIC)
criterions[which.min(criterions$AIC),]

library(leaps)
all.models =regsubsets(Y ~X1 + X2 + X3+X4+X5+X6 , data =athlete)

some.stuff = summary(all.models)
names.of.data = c("Y",colnames(some.stuff$which)[-1])
n= nrow(new.athlete) 
K = nrow(some.stuff$which)
nicer = lapply(1:K,function(i){
  model = paste(names.of.data[some.stuff$which[i,]],collapse = ",")
  p = sum(some.stuff$which[i,])
  BIC = some.stuff$bic[i]
  AIC = some.stuff$aic[i]
  CP = some.stuff$cp[i]
  results = data.frame(model,p,CP,BIC)
  return(results)
})
nicer = Reduce(rbind,nicer)
nicer

#################################stepwise  ##########################################
full.model = lm(Y ~X1+X2+X3+X4+X5+X6, data =athlete) 
empty.model = lm(Y ~ 1, data =athlete)

#  Forward step-wise regression
forward.model.AIC = stepAIC(empty.model, scope = list(lower = empty.model, upper= full.model), k = 2,direction = "forward",trace = FALSE)
forward.model.BIC = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), k = log(n),trace=FALSE,direction = "forward")
forward.model.AIC$coefficients
forward.model.BIC$coefficients

# backwards
backward.model.AIC = stepAIC(full.model, scope = list(lower = empty.model, upper= full.model), k = 2,direction = "backward",trace = FALSE)
backward.model.BIC = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), k = log(n),trace=FALSE,direction = "backward")

##Forward-Backward or Backward-Forward step-wise selection
FB.model.AIC = stepAIC(empty.model, scope = list(lower = empty.model, upper= full.model), k = 2,direction = "both",trace = FALSE)
FB.model.BIC = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), k = log(n),trace=FALSE,direction = "both")
FB.model.AIC
BF.model.AIC = stepAIC(full.model, scope = list(lower = empty.model, upper= full.model), k = 2,direction = "both",trace = FALSE)
BF.model.BIC = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), k = log(n),direction = "both", trace = FALSE)
BF.model.BIC
################################# outliers ##########################################

cutoff = 6.70
outliers = which(athlete$Y > cutoff |  athlete$Y < -cutoff)
new.removed.outliers.data = athlete[-outliers,]
new.removed.outliers.data

newfinaldata.model = lm(Y ~ X5 + X6, data = new.removed.outliers.data)
new.removed.outliers.data$ei = newfinaldata.model$residuals
new.removed.outliers.data$yhat = newfinaldata.model$fitted.values
new.removed.outliers.data$ei
new.removed.outliers.data$yhat

newfinaldata.model


get_outliers = function(m, data, alpha, n, p){
  ei.s = m$residuals/sqrt(sum(m$residuals^2)/(nrow(data) - length(m$coefficients)))
  ri = rstandard(m)
  ti = rstudent(m)
  
  cutoff = qt(1-alpha/(2*n), n -p )
  cutoff.deleted = qt(1-alpha/(2*n), n -p -1 )
  
  outliers = which(abs(ei.s)> cutoff | abs(ri) > cutoff | abs(ti) > cutoff.deleted)
  return(list(ei.s = ei.s, ri = ri, ti = ti,cutoff = cutoff, cutoff.deleted = cutoff.deleted, outliers = outliers))
}
outliers = get_outliers(best.model, athlete, 0.1, nrow(athlete), length(best.model$coefficients))$outliers
ei.s = get_outliers(best.model, athlete, 0.1, nrow(athlete), length(best.model$coefficients))$ei.s
cutoff = get_outliers(best.model, athlete, 0.1, nrow(athlete), length(best.model$coefficients))$cutoff
athlete[outliers,]

################################# leverage points  ##########################################
all.values = influence.measures(newfinaldata.model)$infmat
colnames(all.values)

p = length(newfinaldata.model$coefficients)
n = nrow(athlete)
lev.hat = which(all.values[,"hat"] >2*p/n)
athlete[lev.hat,]

lev.DF = which(all.values[,"dffit"] >1)
athlete[lev.DF,]

lev.DF = which(all.values[,"cook.d"] >qf(0.50,p,n-p))
athlete[lev.DF,]

################################# test normality  ##########################################

qqnorm(newfinaldata.model$residuals)
qqline(newfinaldata.model$residuals)


## sw test 
shapiro.test(newfinaldata.model$residuals)

################################# histogram of errors ##########################################

hist(newfinaldata.model$residuals)

################################# constant variance #################################

plot(newfinaldata.model$fitted.values, newfinaldata.model$residuals, xlab = "fitted values", ylab = "errors")
abline(h=0, lwd = 2, col = "purple")

## fk test 

Group = rep("Lower", nrow(new.removed.outliers.data))
Group[new.removed.outliers.data$Y > median(new.removed.outliers.data$Y)] = "Upper" 
Group = as.factor(Group)
new.removed.outliers.data$group = Group
fligner.test(new.removed.outliers.data$ei, new.removed.outliers.data$group)


################################# general linear test  #################################

small.model = lm(Y~1, data = new.removed.outliers.data)
anova(small.model, newfinaldata.model)


################################# partial R2 & coeff of determination #################################

Partial.R2 = function(small.model,big.model){
  SSE1 = sum(small.model$residuals^2)
  SSE2 = sum(big.model$residuals^2)
  PR2 = (SSE1 - SSE2)/SSE1
  return(PR2)
}

x6.model=lm(Y ~ X6, data = new.removed.outliers.data)
full.model = lm(Y~X5 + X6, data = new.removed.outliers.data )
Partial.R2(x6.model, full.model)

x5_model=lm(Y ~ X5, data = new.removed.outliers.data)
full_model = lm(Y~X5 + X6, data = new.removed.outliers.data )
Partial.R2(x5_model, full.model)

summary(best.model)

summary(newfinaldata.model)

################################# test stats #################################

summary(newfinaldata.model)


################################# drop X5 or X6? #################################

#drop x5
anova(x6.model, full_model)
#drop x6
anova(x5_model, full_model)


################################# simultaneous CIs ################################
alpha =0.05
SCI =confint(newfinaldata.model,level = 1-alpha/4)
SCI

################################# prediction interval ###########################
mult.fun = function(n,p,g,alpha){
  bon = qt(1-alpha/(2*g), n-p)
  WH = sqrt(p*qf(1-alpha,p,n-p))
  Sch = sqrt(g*qf(1-alpha,g,n-p))
  all.mul = c(bon,WH,Sch)
  all.mul = round(all.mul,3)
  names(all.mul) = c("Bon","WH","Sch")
  return(all.mul)
}

mult.CI = function(C.star,x.stars,the.model,alpha,the.type = "confidence"){
  all.preds = predict(the.model,x.stars)
  if(the.type == "confidence"){
    all.se = predict(the.model,x.stars,interval = the.type,se.fit = TRUE)$se.fit
  } else if(the.type == "prediction"){
    all.se = predict(the.model,x.stars,interval = the.type,se.fit = TRUE)$se.fit
    MSE = sum(the.model$residuals^2)/(length(the.model$residuals) - length(the.model$coefficients))
    all.se = sqrt(all.se^2 + MSE)
  }
  LB = all.preds - C.star*all.se
  UB = all.preds + C.star*all.se
  all.CIs = cbind(LB,UB)
  colnames(all.CIs) = paste((1-alpha)*100, "%",c(" Lower"," Upper"), sep = "")
  results = cbind(all.preds,all.CIs)
  colnames(results)[1] = "Estimate"
  return(results)
}


all.of.them = mult.fun(nrow(new.removed.outliers.data), length(newfinaldata.model$coefficients), 3, 0.05)
all.of.them
xs = data.frame(X2=c(19,24,31),X5 = c("m","f","f"), X6=c("Run","Swim", "Net"))
Mul=mult.fun(nrow(new.removed.outliers.data),length(newfinaldata.model$coefficients), 3, 0.05)
Mul
keep = Mul[1]
all.the.CIs = mult.CI(all.of.them[1], xs,newfinaldata.model,0.05,"prediction")
cbind(xs,all.the.CIs)


