data=read.csv("CleanData.csv",header=TRUE)
myattri=data.matrix(data[,-c(1:8)])
myrate=data.matrix(data[,4])
bc.myrate=as.vector(myrate)^1.273
nobs=length(myrate) #number of observations

Attr=myattri

for(i in 1:33){
  for(j in 1:33){
    Attr<-cbind(Attr,Attr[,i]*Attr[,j])
  }
}
fit.full=lm(myrate~.,data=data.frame(Attr)
          
##############least sqr estimator##############
fit=lm(myrate~.,data=data.frame(myattri))
bcfit=lm(bc.myrate~.,data=data.frame(myattri))
summary(fit)
myresult=data.matrix(fit$fitted.values)
accuracy=t(myresult-myrate)%*%(myresult-myrate)/nobs
sqrt(accuracy)


qqnorm(fit$residuals)
qqline(fit$residuals)


par(mfrow=c(2,2))
plot(fit)

vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

########box - cox####################
library(MASS)

bc <- boxcox(myrate~.,data=data.frame(myattri))
trans <- bc$x[which.max(bc$y)]
mnew <- lm(myrate^trans~.,data=data.frame(myattri))

myresult=data.matrix(mnew$fitted.values)
myrate=myrate^trans
accuracy=t(myresult-myrate)%*%(myresult-myrate)/nobs
RMSE=sqrt(accuracy)
RMSE

qqnorm(mnew$residuals)
qqline(mnew$residuals)

#####stepwise model selection#############
step.fit=step(mnew,direction = "backward")
step.fit=step(mnew,direction="both")
stepfit=lm(formula = myrate ~ crossing + finishing + heading_accuracy + 
             short_passing + free_kick_accuracy + long_passing + ball_control + 
             acceleration + sprint_speed + agility + reactions + shot_power + 
             stamina + strength + long_shots + aggression + interceptions + 
             positioning + vision + penalties + marking + standing_tackle + 
             sliding_tackle + gk_diving + gk_handling + gk_kicking + gk_positioning + 
             gk_reflexes,data = data.frame(myattri))

myresult=data.matrix(stepfit$fitted.values)
accuracy=t(myresult-myrate)%*%(myresult-myrate)/nobs
R2 = 1 - (sum((bc.myrate-myresult)^2)/sum((bc.myrate-mean(bc.myrate))^2))
k=33-5
R2.adj=1-((1-R2)*(nobs-1)/(nobs-k-1))


######non-negative least sqr estimation#########
library(nnls)
dummyx=matrix(1,nobs,1)
x=cbind(dummyx,myattri)
fit.nn=nnls(x, myrate)
coef.nn=as.matrix(fit.nn$x)

myresult=t(t(coef.nn)%*%t(x))
accuracy=(t(myresult-myrate)%*%(myresult-myrate))/nobs
RMSE=sqrt(accuracy)
# Computing Rsquared value
R2 = 1 - (sum((myrate-myresult)^2)/sum((myrate-mean(myrate))^2))
k=33-14
R2.adj=1-((1-R2)*(nobs-1)/(nobs-k-1))


RMSE#3.329
R2#
R2.adj #0.784


