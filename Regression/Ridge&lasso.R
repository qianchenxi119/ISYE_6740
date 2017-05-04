######################Lasso Regression##########################
library(glmnet)
dummyx=matrix(1,nobs,1)
x=cbind(dummyx,myattri)


fit.lasso = glmnet(x=myattri,y=myrate,family="gaussian", alpha=1)
cv.lasso=cv.glmnet(x=myattri,y=myrate,alpha=1,parallel=TRUE, standardize=TRUE, type.measure='auc')
#extract variables and coefficient from lasso regression model
c=coef(cv.lasso,s='lambda.min')
mybeta=c@x
#form reduced attribute
lasattri=x[,c@i+1]

# Evaluate reduced model
myresult=t(mybeta%*%t(lasattri))
# Computing accuracy via MSE
accuracy=t(myresult-myrate)%*%(myresult-myrate)/nobs
RMSE=sqrt(accuracy)
# Computing Rsquared value
R2 = 1 - (sum((myrate-myresult)^2)/sum((myrate-mean(myrate))^2))
k=33-3
R2.adj=1-((1-R2)*(nobs-1)/(nobs-k-1))

R2.adj
RMSE


#######################Ridge regression#################################
fit.ridge=glmnet(x=myattri,y=myrate,family="gaussian", alpha=0)
cv.ridge=cv.glmnet(myattri,myrate,family='gaussian',alpha=0, parallel=TRUE, standardize=TRUE, type.measure='auc')
plot(cv.ridge)
#extraxt coefficents
c=coef(cv.ridge, s=cv.ridge$lambda.min)@x

myresult=t(c%*%t(x))
accuracy=(t(myresult-myrate)%*%(myresult-myrate))/nobs
# Computing Rsquared value
R2 = 1 - (sum((myrate-myresult)^2)/sum((myrate-mean(myrate))^2))
k=33
R2.adj=1-((1-R2)*(nobs-1)/(nobs-k-1))
RMSE=sqrt(accuracy)

accuracy#
R2.adj
R2
RMSE

##############plots####################
par(mfrow=c(2,1))
plot(fit.lasso, xvar="lambda")
plot(cv.lasso, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(cv.ridge, main="Ridge")