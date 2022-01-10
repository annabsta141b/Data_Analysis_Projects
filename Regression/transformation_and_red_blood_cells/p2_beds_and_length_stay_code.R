############################## Hospital Beds & Length of Stay ####################

hospital_beds <- read.csv("~/LocalDocuments/UCDavis/Year2/STA108/projects/Project_2/Transform2.csv")
View(hospital_beds)
names(hospital_beds)
######################## Plot of Original Data & Regression Line ####################
# X = beds, Y= length
plot(hospital_beds$Bed, hospital_beds$Length, xlab = "Bed",ylab = "Length" )

hosp_reg_line = lm(Length~Bed, data = hospital_beds)
hosp_reg_line = lm(Y~X, data = hospital_beds)


######################## testing normality ####################

qqnorm(hosp_reg_line$residuals)
qqline(hosp_reg_line$residuals)

######################## histogram of errors ####################

hist(hosp_reg_line$residuals, main = "Histogram of Errors", xlab = "ei",
     col = "white")


######################## shapiro -wilks test ####################

shapiro.test(hosp_reg_line$residuals)


######################## test constant variance ####################

## errors vs fitted vals
plot(hosp_reg_line$fitted.values,hosp_reg_line$residuals, xlab = "Fitted Values",
     ylab = "Residuals", pch = 20,font = 2,font.lab = 2,cex = 1.25)
abline(h=0, col = "purple", lwd = 2)

######################## FK test ####################
Group = rep("Lower",nrow(hospital_beds)) 
Group[hospital_beds$Length > median(hospital_beds$Length)] = "Upper" 
Group = as.factor(Group) 
hospital_beds$Group = Group
hospital_beds$ei = hosp_reg_line$residuals
the.FKtest= fligner.test(hospital_beds$ei, hospital_beds$Group)
the.FKtest


######################## general linear test ####################
small_model = lm(Length~1, data = hospital_beds)
big_model = lm(Length ~Bed, data = hospital_beds)


anova(small_model,big_model)

######################## Tukey Transformations (X,Y variable) ####################

#before transformation
par(mfrow = c(1,2))
plot(hospital_beds$Length, hospital_beds$Bed, xlab = "Length", ylab = "Bed")
qqnorm(hosp_reg_line$residuals)
qqline(hosp_reg_line$residuals)

#transform  X
library(rcompanion) #To load in the library needed
tukeyX = transformTukey(hospital_beds$Bed, plotit = FALSE)
par(mfrow = c(1,2))
T.DataX = data.frame(Y = hospital_beds$Length,X1 = tukeyX)
T.modelX = lm(Y ~ X1, data = T.DataX)
plot(T.DataX$X1, T.DataX$Y, xlab = "X", ylab = "Y")
qqnorm(T.modelX$residuals)
qqline(T.modelX$residuals)

#transform Y
tukeyY = transformTukey(hospital_beds$Length,plotit = FALSE)
par(mfrow = c(1,2))
T.DataY = data.frame(Y = tukeyY,X1 = hospital_beds$Bed)
T.modelY = lm(Y ~ X1, data = T.DataY)
plot(T.DataY$X1, T.DataY$Y, xlab = "X", ylab = "Y")
qqnorm(T.modelY$residuals)
qqline(T.modelY$residuals)

#transformation of both X and Y
par(mfrow = c(1,2))
T.Data = data.frame(Y = tukeyY,X1 = tukeyX)
T.model = lm(Y ~ X1, data = T.Data)
plot(T.Data$X1, T.Data$Y, xlab = "X", ylab = "Y")
qqnorm(T.model$residuals)
qqline(T.model$residuals)

######################## Box Cox (Y variable) ####################
library(MASS)
orig_model = lm(Length ~ Bed, data = hospital_beds)

BC = boxcox(orig_model,lambda = seq(-6,6,0.1),plotit = TRUE)
lambda = BC$x[which.max(BC$y)]
lambda

BC.Y = (hospital_beds$Length^lambda - 1)/lambda
BC.data = data.frame(Y = BC.Y, X1 = hospital_beds$Bed)

## after Y trans
par(mfrow = c(1,2))
BC.model = lm(Y ~ X1, data = BC.data)
plot(BC.data$X1, BC.data$Y)
qqnorm(BC.model$residuals)
qqline(BC.model$residuals)


######################## outliers ####################
install.packages("leaps")
install.packages("MPV")
library(leaps)
library(MPV)


best.model = T.model
ei.s = best.model$residuals/sqrt(sum(best.model$residuals^2)/(nrow(hospital_beds) - length(best.model$coefficients)))
ei.s
ri = rstandard(best.model)
ti = rstudent(best.model)

alpha = 0.1 
n = nrow(hospital_beds)
p = length(best.model$coefficients)
cutoff = qt(1-alpha/(2*n), n -p )
cutoff.deleted = qt(1-alpha/(2*n), n -p -1 )

outliers = which(abs(ei.s)> cutoff | abs(ri) > cutoff | abs(ti) > cutoff.deleted)
hospital_beds[outliers,]

alpha = 0.05
n = nrow(hospital_beds)
p = length(best.model$coefficients)
cutoff = qt(1-alpha/(2*n), n -p )
cutoff.deleted = qt(1-alpha/(2*n), n -p -1 )

outliers = which(abs(ei.s)> cutoff | abs(ri) > cutoff | abs(ti) > cutoff.deleted)
hospital_beds[outliers,]


