library(MASS)
library(VGAM)
library(vcd)
library(pROC)
library(dplyr)
library(Epi)
library(lattice)
library(DAAG)
library(boot)

data1=read.csv("Cancer_Data.csv",header=T,na.strings = "")
data1=data1[,-c(1,33)]
head(data1)
names(data1)
data1$y=data1$diagnosis
data1$y=ifelse(data1$y=="B",0,1)
data1=data1[,-1]
data1=data1[complete.cases(data1), ]
row.has.na <- apply(data1, 1, function(x){any(is.na(x))})
sum(row.has.na)
data1.fix <- data1[!row.has.na,]

#### null model
mod0=glm(y~1,data=data1.fix,family = binomial(link="logit"))
summary(mod0)

#### full model
modf=glm(y~.,data=data1.fix,family = binomial(link="logit"))
summary(modf)

#### select model
data2=data1.fix[,-7]
mod1=glm(y~.,data=data2,family = binomial(link="logit"))
summary(mod1)


#### comparsion
anova(mod0, mod1, test="LRT")
anova(modf, mod1, test="LRT")


#leave one out cross-validation
prop <- sum(data2$y)/nrow(data2) 
prop
predicted <- as.numeric(fitted(mod1) > prop)
xtabs(~ data2$y + predicted)
pihat <- vector(length=569)
for (i in 1:569) {
  pihat[i] <-
    predict(update(mod1, subset=-i),
            newdata=data1[i,], type="response")
}

yy <- as.numeric(data1$y > 0)
yhat <- as.numeric(pihat >prop)
confusion <- table(yy, yhat)
confusion

acc = (confusion[1,1]+confusion[2,2])/sum(confusion)
acc


#### K-fold cross validation
cv.binary(mod0)
cv.binary(modf)
cv.binary(mod1)
cost<-function(r,pi=0) mean(abs(r-pi)>0.8)
out0=cv.glm(data2,mod1,cost,K=10)
names(out0)
out0$delta
out1=cv.glm(data2,mod1,cost,K=10)
out2=cv.glm(data2,mod1,cost,K=10)
out3=cv.glm(data2,mod1,cost,K=10)

out0$delta
out1$delta
out2$delta
out3$delta


#### roc curve for each model
rocplot0 <- roc(y ~ fitted(mod0), data=data1.fix)
plot.roc(rocplot0, legacy.axes=TRUE) 
auc(rocplot0)

rocplot1 <- roc(y ~ fitted(modf), data=data1.fix)
plot.roc(rocplot1, legacy.axes=TRUE) 
auc(rocplot1) 

rocplot2 <- roc(y ~ fitted(mod1), data=data2)
plot.roc(rocplot2, legacy.axes=TRUE)
auc(rocplot2) 

ROC(form=data2$y ~ data2$radius_mean + data2$texture_mean+
      data2$perimeter_mean+data2$area_mean+data2$smoothness_mean+
      data2$compactness_mean+data2$concave.points_mean+
      data2$symmetry_mean+data2$fractal_dimension_mean+data2$radius_se+
      data2$texture_worst+data2$perimeter_worst+data2$area_worst+
      data2$smoothness_worst+data2$compactness_worst+
      data2$concavity_worst+data2$concave.points_worst+
      data2$symmetry_worst+data2$fractal_dimension_worst,plot="ROC")

#### correlation
cor(data1$y, fitted(modf))
cor(data2$y, fitted(mod1))

