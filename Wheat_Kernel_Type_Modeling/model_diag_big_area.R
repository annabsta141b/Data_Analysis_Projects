
### area > 14.3

kamaRosaBG = subset(seed3, area >14.3)
table(kamaRosaBG$kernalType)
kamaRosaBG$area >14.3
kamaRosaBG$kernalType = droplevels(kamaRosaBG$kernalType)
rownames(kamaRosaBG) = NULL

############################ model selection area >14.3 ##############################
blueGreenArea = glm(kernalType ~area + compactness + asymCoef , 
                    family = binomial(link = "logit"), 
                    data = kamaRosaBG) 

blueGreenArea$deviance


emptyBG = glm(kernalType ~1 , family = binomial(link = "logit"), 
              data = kamaRosaBG)

#stepwise selection
fbs_BG = step(emptyBG, scope = list(lower = emptyBG, upper= blueGreenArea),
              direction = "both", k=log(nrow(kamaRosaBG)), trace = FALSE)

finalGreenBlue = fbs_BG

summary(finalGreenBlue)
anova(emptyBG,finalGreenBlue, test = "Chisq")
anova(emptyBG,blueGreenArea, test = "Chisq")



######################## diagostic plots ###########################

res.DBG = residuals(finalGreenBlue, type="deviance")
#### resid vs fitted vals
plot(finalGreenBlue$fitted.values, res.DBG, pch=16, cex=0.6, 
     ylab='Deviance Residuals', 
     xlab='Fitted Values',
     main = "Deviance Residual Plot for Kernels with Large Area")
lines(smooth.spline(finalGreenBlue$fitted.values, res.DBG, spar=1.9), col=2)
abline(h=0, lty=2, col='grey')

par(mfrow = c(1,2))
### leverage and cooks dist
leverageBG = hatvalues(finalGreenBlue)
plot(names(leverageBG), leverageBG, xlab="Index", type="h", 
     main = "Leverage Plot:\n Kernels with Large Area")
points(names(leverageBG), leverageBG, pch=16, cex=0.6)
susPtsBG <- as.numeric(names(sort(leverageBG, decreasing=TRUE)[1:3]))
text(susPtsBG, leverageBG[susPtsBG], susPtsBG, adj=c(-0.2,-0.3), cex=0.7, col=4)
p <- length(coef(finalGreenBlue))
n <- nrow(kamaRosaBG)
abline(h=2*p/n,col=2,lwd=2,lty=2)
infPts <- which(leverageBG>2*p/n)
cooksBG = cooks.distance(finalGreenBlue)
plot(cooksBG, ylab="Cook's Distance", pch=16, cex=0.6, 
     main = "Cook's Distance Plot:\n Kernels with Large Area")
susPts <- as.numeric(names(sort(cooksBG[infPts], decreasing=TRUE)[1:3]))
text(susPts, cooksBG[susPts], susPts, adj=c(-0.1,-0.1), cex=0.7, col=4)

### check influential pts
seed5 = kamaRosaBG[-c(6,21,102,28,19,105),]
c=  glm(kernalType ~1 , family = binomial(link = "logit"), 
        data = seed5)
d = glm(formula = kernalType ~ area + asymCoef, family = binomial(link = "logit"), 
        data = seed5)
anova(c,d,test = "Chisq")


