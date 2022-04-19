
########################### condition on area ####################

## note: we want to partition between 14-15 in such a way that we have relatively balanced groups
### based on the histogram we have kama = green, rosa = blue, canadian = pink 
# a = subset(seed3, kernalType=="Rosa")
# range(a$area)

kamaRosa = subset(seed3, area >14.3)
table(kamaRosa$kernalType)
table(kamaRosa$kernalType)[2]/sum(table(kamaRosa$kernalType))
table(kamaRosa$kernalType)[3]/sum(table(kamaRosa$kernalType))
kamaRosa$kernalType = droplevels(kamaRosa$kernalType)
kamaRosa$area >14.3

kamaCanada = subset(seed3, area <= 14.3)
table(kamaCanada$kernalType)
table(kamaCanada$kernalType)[1]/sum(table(kamaCanada$kernalType))
table(kamaCanada$kernalType)[2]/sum(table(kamaCanada$kernalType))
rownames(kamaCanada) = NULL

### area <=14.3

################ model selection for area <= 14.3 ###############
pinkGreenArea = glm(kernalType ~area + compactness + asymCoef , 
                    family = binomial(link = "logit"), data = kamaCanada)

pinkGreenArea$deviance

emptyPG = glm(kernalType ~1 , family = binomial(link = "logit"), 
              data = kamaCanada)

fbs_pg = step(emptyPG, scope = list(lower = emptyPG, upper= pinkGreenArea),
     direction = "both", k=log(nrow(kamaCanada)), trace = FALSE)

summary(fbs_pg)

###reggression effect
anova(emptyPG,fbs_pg, test = "Chisq")
anova(emptyPG,pinkGreenArea, test = "Chisq")

##################### diagnostic plots ################
library(latex2exp)
res.D = residuals(fbs_pg, type="deviance") #or residuals(fit), by default


plot(fbs_pg$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', 
     xlab='Fitted Values', cex.main=1.3, cex.axis=1.3,cex.lab = 1.3,
     main = "Deviance Residual Plot:\n Kama and Canadian Wheat Kernels ")
lines(smooth.spline(fbs_pg$fitted.values, res.D, spar=1.8), col=2)
abline(h=0, lty=2, col='grey')

########## outliers and leverage pts 


leveragePG = hatvalues(fbs_pg)
plot(names(leveragePG), leveragePG, xlab="Index", type="h", ylab = "Leverage",
     main = "Leverage Plot:\n Kama and Canadian Wheat Kernels",
     cex.main=1.3, cex.axis=1.3,cex.lab = 1.3)
points(names(leveragePG), leveragePG, pch=16, cex=0.6)
susPtsPG <- as.numeric(names(sort(leveragePG, decreasing=TRUE)[1:3]))
text(susPtsPG, leveragePG[susPtsPG], susPtsPG, adj=c(-0.2,-0.3), cex=0.9, col=4)
p <- length(coef(fbs_pg))
n <- nrow(kamaCanada)
abline(h=2*p/n,col=2,lwd=2,lty=2)

infPtsPG <- which(leveragePG>2*p/n)
cooksPG = cooks.distance(fbs_pg)
plot(cooksPG, ylab="Cook's Distance", pch=16, cex=0.6, 
     main = "Cook's Distance Plot:\n Kama and Canadian Wheat Kernels",
     cex.main=1.3, cex.axis=1.3,cex.lab = 1.3)
susPtsPG <- as.numeric(names(sort(cooksPG, decreasing=TRUE)[1:3]))
text(susPtsPG,cooksPG[susPtsPG] ,susPtsPG, adj=c(-0.1,-0.1), cex=0.9, col=4)

### remove influential pts as well as high cooks dist to see if these affect regression line. 
## if so rm them 
seed4 = kamaCanada[-c(12,20,25,26,72),]
a = glm(kernalType ~1 , family = binomial(link = "logit"), 
              data = seed4)

b = glm(formula = kernalType ~ area + asymCoef, family = binomial(link = "logit"), 
        data = seed4)

###reggression effect
anova(a,b, test = "Chisq")
##regression effect and interaction

fbs_pg
q = glm(formula = kernalType ~ area + asymCoef+area*asymCoef , family = binomial(link = "logit"), 
        data = kamaCanada)

anova(fbs_pg, q, test = "Chisq")




