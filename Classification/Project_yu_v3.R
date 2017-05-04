#-----------------------------------------------------------------------------------#
#               Import Data
#-----------------------------------------------------------------------------------##
myData = read.csv("CleanData.csv",header=TRUE)
myX = myData[,9:41]
myY = myData[,4]


#-----------------------------------------------------------------------------------#
#               Best Subsets Regression
#-----------------------------------------------------------------------------------##
library(leaps)
attach(myX)
leaps = regsubsets(myY~.,data=myX,method = "exhaustive", nvmax=33)
# view results 
lp=summary(leaps)
lp$adjr2

plot(leaps,scale=c("Cp"), main = "Cp")
plot(leaps,scale=c("bic"), main = "bic")
plot(leaps,scale=c("adjr2"), main = "adjr2")


write.csv2(lp$which,file = "bestsubset2")
write.csv2(lp$bic,file = "bic_redo")

# plot statistic by subset size 
testleap = leaps(myX, myY,method=c("Cp", "adjr2", "r2"),nbest = 8)

# best subset test case on 2way anova---Abandoned due to linear dependence
newX<-myX
counter = 1 
for(i in 1:33){
  for(j in 1:33){
    newX = cbind(newX,myX[,i]*myX[,j])
    colnames(newX)[33+counter] = toString(c(colnames(newX)[i],colnames(newX)[j]))
    counter = counter + 1  
  }
}

attach(newX)
leaps_2way = regsubsets(myY~.,data=newX,method = "exhaustive", nvmax=100)
ob = summary(leaps_2way)



#-----------------------------------------------------------------------------------#
#               Principle Conponent Regression
#-----------------------------------------------------------------------------------#
library(pls)
set.seed (1000)
myData = read.csv("CleanData.csv",header=TRUE)
Attr = data.matrix(myData[,9:41])
myX = myData[,9:41]
myY = myData[,4]

#Checkout the correlation of data
Correlation = cor(myX)
write.csv2(Correlation,file = "Correlation")
#Get the eigen value and eigen vectors
MyEigen<-eigen(t(Attr)%*%Attr)

#RUN PCR
pcr_model <- pcr(myY~., data = myX, scale = TRUE, validation = "CV")
summary(pcr_model)

# Train-test split
trainX <- myX[1:17112,]
trainY <- myY[1:17112]
testX <- myX[17112:21390, ]
testY <- myY[17112:21390]

pcr_model_TRAIN <- pcr(trainY~., data = trainX,scale =TRUE, validation = "CV")
pcr_pred <- predict(pcr_model_TRAIN, testX, ncomp = 22)
rmse = sqrt(mean((pcr_pred - testY)^2))
print(rmse)

