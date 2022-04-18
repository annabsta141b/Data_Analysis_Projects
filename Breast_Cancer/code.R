

################### read in data ##############################
bcancer <- read.csv("Documents/Winter22/STA223/proj1/data-2.csv", header=TRUE)
bcancer$X=NULL
bcancer$id = NULL
bcancer$diagnosis = as.factor(bcancer$diagnosis)
View(bcancer)

######################## correlations ##################
library(ggcorrplot)
correlationData = cor(bcancer[,2:31])
ggcorrplot(correlationData,type = "lower")

## get corelation matrix in data frame
corrVals = data.frame(correlationData)
corrVals$Xname = rownames(corrVals)
corrVals = corrVals[,c(31,1:30)]
View(corrVals)
View(corrVals[,c(2:11, 22:31)])

corrVals = sort(corrVals, decreasing = TRUE)

bigCorr = which(corrVals[,2:31] > 0.5 | corrVals[,2:31] < -0.5)


#### correlated
# radius mean --> perimeter_mean, area_mean, perim_se, area_se, radius_worst
### keep radius
library(dplyr)
bcancer_mean_only_measure= bcancer %>% select(-contains(c("perimeter", "area","worst","se")))
names(bcancer_mean_only_measure)
View(cor(bcancer_mean_only_measure[,c(2,5:8,3,4,9)]))

################# boxplots/eda####################################
par(mfrow = c(1,1))
boxplot(bcancer$radius_mean, main = "a")
boxplot(bcancer$area_mean, main = "a")

sapply(bcancer[,2:31], boxplot)

good = subset(bcancer, diagnosis == "B")
mean(good$radius_mean)

bad = subset(bcancer, diagnosis == "M")
mean(bad$radius_mean)

## checking for balanced groups in Y variable
library(ggplot2)
ggplot(bcancer)+
  geom_bar(aes(x= diagnosis, fill = diagnosis),position = "dodge")+
   ylab("Count") + xlab("Tumor Diagnosis")+theme_bw()
  
########## compare 2 models & more eda ##############################
### standard deviation of gray-scale values = texture
### smoothness: local variation in radius lengths

m1_compact = glm(diagnosis~ radius_mean  + smoothness_mean + 
                   fractal_dimension_mean + symmetry_mean + compactness_mean,
                 data = bcancer_mean_only_measure, family ="binomial")

m2_concav = glm(diagnosis~ radius_mean  + smoothness_mean + 
                  symmetry_mean + fractal_dimension_mean + concavity_mean,
                data = bcancer_mean_only_measure, family ="binomial" )

m1_compact$deviance
m2_concav$deviance

# why I chose BIC, why use logistic regression with logit link 
# why we removed standard errors, 

# the higher hte concave pt increase probability of B,
corrplot(bcancer[,c(2,4,5,12,14,15, 22,24,25)],color = T, details = T)

plot(bcancer[,c(2,4,5,12,14,15, 22,24,25)])
plot(bcancer[,c(2,4,5)])
plot(bcancer[,c(12,14,15)])
plot(bcancer[,c(22,24,25)])

########################### model selection ###################################
init_model = m2_concav

empty_model = glm(diagnosis~1, family = "binomial", data = bcancer_mean_only_measure)
#regression effect, can we perform regression, yes we can
anova(empty_model, init_model, test = "Chi")


fbBIC = step(empty_model, scope = list(lower = empty_model, upper = init_model), 
             direction= "both", k=log(nrow(bcancer_mean_only_measure)))
fbBIC$coefficient 

anova(empty_model, fbBIC, test = "Chi")

########### residuals ##############################
par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
par(mfrow = c(1,2))

res.P = residuals(fbBIC, type="pearson")
res.D = residuals(fbBIC, type="deviance") #or residuals(fit), by default
boxplot(cbind(res.P, res.D), names = c("Pearson", "Deviance"), 
        main = "Pearson and Deviance Residuals")

plot(fbBIC$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals Plot', 
     xlab='Fitted Values', main = "Pearson Residual Plot")
lines(smooth.spline(fbBIC$fitted.values, res.D, spar=1.485), col=2)
abline(h=0, lty=2, col='grey')

plot(fbBIC$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', 
     xlab='Fitted Values', main = "Residuals vs Fitted Values")
lines(smooth.spline(fbBIC$fitted.values, res.D, spar=0.9), col=2)
abline(h=0, lty=2, col='grey')


################## outliers and leverage pts ########################################
# Leverage points are suspected if hii > 2p/n.

a = influence.measures(fbBIC, infl = influence(fbBIC))$infmat
a = data.frame(a)
a$cook.d

plot(a$hat,ylab="Leverage", xlab="Index", type="h", main = "Leverage Plot")
points(a$hat)
abline(h=2*length(fbBIC$coefficients)/nrow(bcancer_mean_only_measure),col=2,lwd=2,lty=2)

plot(a$cook.d,ylab="Cook's Distance", cex=0.9, main = "Cooks Distance Plot")
susPts <- as.numeric(names(sort(cooks[infPts], decreasing=TRUE)[1:2]))
text(susPts, cooks[susPts], susPts, adj=c(-0.1,-0.1), cex=0.7, col=4)

leverage = hatvalues(fbBIC)
plot(names(leverage), leverage, ylab="Leverage", xlab="Index", type="h")
points(names(leverage), leverage)
abline(h=2*length(fbBIC$coefficients)/nrow(bcancer_mean_only_measure),col=2,lwd=2,lty=2)

bcancer_mean_only_measure[c(69,153,505),]
#get the leverage pts
# plot(names(leverage), leverage, xlab="Index", type="h")
# points(names(leverage), leverage, pch=16, cex=0.6)
# p <- length(coef(bfBIC))
# n <- nrow(bcancer_mean_only_measure)
# abline(h=2*p/n,col=2,lwd=2,lty=2)
# infPts <- which(leverage>2*p/n)

sort(leverage, decreasing = T)

######################## coocks distance ##############################

cooks = cooks.distance(fbBIC)

plot(cooks, ylab="Cook's Distance", pch=16, cex=0.6)

#which(cooks > 0.2)
which(leverage > 0.10)

par(mfrow = c(1,2))
leverage = hatvalues(fbBIC)
plot(names(leverage), leverage, ylab="Leverage", xlab="Index", 
     type="h", main =" Leverage Plot")
points(names(leverage), leverage)
susPts <- as.numeric(names(sort(leverage, decreasing=TRUE)[1:3]))
text(susPts, leverage[susPts], susPts, adj=c(-0.2,-0.3), cex=0.7, col=4)
abline(h=2*length(fbBIC$coefficients)/nrow(bcancer_mean_only_measure),col=2,lwd=2,lty=2)

plot(cooks, ylab="Cook's Distance", cex=0.9,main = "Cooks Distance Plot")
susPts <- as.numeric(names(sort(cooks[infPts], decreasing=TRUE)[1:3]))
text(susPts, cooks[susPts], susPts, adj=c(-0.1,-0.1), cex=0.7, col=4)
# plot(a$cook.d,ylab="Cook's Distance", cex=0.9, main = "Cooks Distance Plot")
# susPts <- as.numeric(names(sort(cooks[infPts], decreasing=TRUE)[1:2]))
# text(susPts, cooks[susPts], susPts, adj=c(-0.1,-0.1), cex=0.7, col=4)

plot(names(leverage), leverage, xlab="Index", type="h")
points(names(leverage), leverage, pch=16, cex=0.6)
p <- length(coef(fbBIC))
n <- nrow(bcancer)
abline(h=2*p/n,col=2,lwd=2,lty=2)
infPts <- which(leverage>2*p/n)

plot(cooks, ylab="Cook's Distance", pch=16, cex=0.6)
points(infPts, cooks[infPts], pch=17, cex=0.8, col=2)
susPts <- as.numeric(names(sort(cooks[infPts], decreasing=TRUE)[1:3]))
text(susPts, cooks[susPts], susPts, adj=c(-0.1,-0.1), cex=0.7, col=4)



newData = bcancer_mean_only_measure[-which(cooks ),]
newDataL = bcancer_mean_only_measure[-which(leverage > 0.1),]

newDataAll = bcancer_mean_only_measure[-unique(infPts),]
#### refit the data 
### want to test if radius has an effect on tumor diagnosis
## original data no pts removed, also take out readius mean
m_orig1 = glm(formula = diagnosis ~ radius_mean + concavity_mean + smoothness_mean, 
             family = "binomial", data = newDataAll)

m_orig2 = glm(diagnosis ~ 1, family = "binomial", data = newDataAll)

anova(m_orig2, m_orig1, test = "Chi")
 

###### test again with the leverage removed

fullMLev = glm(formula = diagnosis ~ radius_mean + concavity_mean + smoothness_mean, 
                    family = "binomial", data = newDataL)
  
emptyLev = glm(diagnosis ~ 1, family = "binomial", data = newDataL)


anova(emptyLev, fullMLev, test = "Chi")
###cooks distance ####
newDataC = bcancer_mean_only_measure[-which(cooks > 0.2),]

fullMCook = glm(formula = diagnosis ~ radius_mean + concavity_mean + smoothness_mean, 
               family = "binomial", data = newDataAll)

emptyCook = glm(diagnosis ~ 1, family = "binomial", data = newDataAll)

anova(emptyCook,fullMCook,test = "Chi")

############################## interaction terms ########################
#only relevant if there is biological reason 
fbBIC
interactModel = glm(diagnosis ~ radius_mean + concavity_mean + smoothness_mean +
                      radius_mean*smoothness_mean , 
                    family = "binomial", data = bcancer_mean_only_measure)

anova(fbBIC,interactModel, test = "Chi")
