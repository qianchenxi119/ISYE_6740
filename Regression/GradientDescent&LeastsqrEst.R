#---------Use gradient descent to do multiple linear regression--------
niter=10000
mybeta=matrix(0,34,1)

gdt=matrix(0,niter,34)#the gradient
fb=matrix(0,niter)#objective function
nobs=length(myrate) #number of observations

#adding a column vector of 1 at the begining of attributes
#in order to yield the intercept
dummyx=matrix(1,nobs,1)
x=cbind(dummyx,myattri)
y=myrate

L=max(eigen(t(x)%*%x/nobs)$values)
ss=1/L

for(i in 1:niter){
  #gradient
  gdt[i,]=t(x%*%mybeta-y)%*%x/nobs
  #f(beta)
  fb[i]=t(y-x%*%mybeta)%*%(y-x%*%mybeta)/(2*nobs)
  #update beta
  mybeta=mybeta-ss*(gdt[i,])
  stopiter=i # the number of iteration when gradient desent stops
  if(fb[i]<6.5)
    break #stopping criteria
}

for (k in 1:stopiter){
plot(fb,type="line", lwd=2, xlab="# of iteration",ylab="objective function",main="f(beta) versus
     #of iteration")
}

yhat=x%*%mybeta
MSE=t(yhat-myrate)%*%(yhat-myrate)/nobs
RMSE=sqrt(MSE)
R2 = 1 - (sum((myrate-yhat)^2)/sum((myrate-mean(myrate))^2))
k=33
R2.adj=1-((1-R2)*(nobs-1)/(nobs-k-1))

RMSE
R2
R2.adj

#---use least sqr estimator--------
dummyx=matrix(1,nobs,1)
x=cbind(dummyx,myattri)

mybeta=solve(t(x)%*%x)%*%t(x)%*%bc.myrate
yhat=x%*%mybeta
MSE=t(yhat-bc.myrate)%*%(yhat-bc.myrate)/nobs
RMSE=sqrt(MSE)
R2 = 1-(sum((bc.myrate-yhat)^2)/sum((bc.myrate-mean(bc.myrate))^2))

RMSE
R2



