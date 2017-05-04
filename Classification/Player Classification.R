###ISyE 6740 Project European Player Rating System
##PCA Analysis and Position Labeling
setwd('D:/Acedemic GT/ISyE 6740/Project')
MyData<-read.csv('CleanData.csv')

#libraries
#library(plot3D)
#library(scatterplot3d)
library("e1071")
#library(mclust)
library(EMCluster)
Rate<-as.matrix(MyData[,4])
Attr<-as.matrix(MyData[,c(9:41)])
#####Classfication#########
###K-Means classification
K = 4 #4 classes, expecting GK, AT, MF, DF 
KMClass<-kmeans(Attr, K, nstart = 1) #call kmeans() fn
KMAttr<-cbind(Attr,KMClass$cluster)
KMMyData<-cbind(MyData,KMClass$cluster)
KM1<-KMMyData[KMMyData$`KMClass$cluster`==1,]
write.csv(KMMyData, file = "ClassficationData.csv")
# PC1<-subset(KMAttr, KMAttr[,34] == 1)[,-34]
# PC2<-subset(KMAttr, KMAttr[,34] == 2)[,-34]
# PC3<-subset(KMAttr, KMAttr[,34] == 3)[,-34]
# PC4<-subset(KMAttr, KMAttr[,34] == 4)[,-34]

###Mixture Model

#initialization of EM
initEM<-init.EM(Attr, nclass = 4, lab = NULL, EMC = .EMC,
        stable.solution = TRUE, min.n = NULL, min.n.iter = 5,
        method = c("em.EM", "Rnd.EM"))

emobj <- simple.init(Attr, nclass = 4)

EMC<-emcluster(Attr, emobj = emobj, assign.class = TRUE)



#######PCA Analysis############

MyEigen<-eigen(t(Attr)%*%Attr)
PC1<-subset(KMAttr, KMAttr[,34] == 1)[,-34]
PC2<-subset(KMAttr, KMAttr[,34] == 2)[,-34]
PC3<-subset(KMAttr, KMAttr[,34] == 3)[,-34]
PC4<-subset(KMAttr, KMAttr[,34] == 4)[,-34]

###Classification Identification###

KMcolmean<-cbind(colMeans(PC1),colMeans(PC2),colMeans(PC3),colMeans(PC4))
write.csv(KMcolmean, file = "ClassficationData1.csv")


# Plot first vs. second principle component for each KKKKKKM class

plot(Attr%*%MyEigen$vectors[,1],Attr%*%MyEigen$vectors[,2],type="p",pch=19,axes=FALSE,xlab="",ylab="",main="")

points(x=PC1%*%MyEigen$vectors[,1],y=PC1%*%MyEigen$vectors[,2],col="red",pch=19)
points(x=PC2%*%MyEigen$vectors[,1],y=PC2%*%MyEigen$vectors[,2],col="green",pch=19)
points(x=PC3%*%MyEigen$vectors[,1],y=PC3%*%MyEigen$vectors[,2],col="blue",pch=19)
points(x=PC4%*%MyEigen$vectors[,1],y=PC4%*%MyEigen$vectors[,2],col="yellow",pch=19)
#points(x=PC5%*%MyEigen$vectors[,1],y=PC5%*%MyEigen$vectors[,2],col="pink",pch=19)
box()
axis(1,padj=-0.5)
axis(2,padj=0.5)
title(main="First Two Principle Components-4 Classes K-Means")
title(xlab="PC 1",line=1.65)
title(ylab="PC 2",line=1.65)

# Plot first vs. second principle component for each EEEEEEM class
EMAttr<-cbind(Attr,EMC$class)
PC1<-subset(EMAttr, EMAttr[,34] == 1)[,-34]
PC2<-subset(EMAttr, EMAttr[,34] == 2)[,-34]
PC3<-subset(EMAttr, EMAttr[,34] == 3)[,-34]
PC4<-subset(EMAttr, EMAttr[,34] == 4)[,-34]

EMcolmean<-cbind(colMeans(PC1),colMeans(PC2),colMeans(PC3),colMeans(PC4))
write.csv(EMcolmean, file = "ClassficationData2.csv")

plot(Attr%*%MyEigen$vectors[,1],Attr%*%MyEigen$vectors[,2],type="p",pch=19,axes=FALSE,xlab="",ylab="",main="")

points(x=PC1%*%MyEigen$vectors[,1],y=PC1%*%MyEigen$vectors[,2],col="red",pch=19)
points(x=PC2%*%MyEigen$vectors[,1],y=PC2%*%MyEigen$vectors[,2],col="green",pch=19)
points(x=PC3%*%MyEigen$vectors[,1],y=PC3%*%MyEigen$vectors[,2],col="blue",pch=19)
points(x=PC4%*%MyEigen$vectors[,1],y=PC4%*%MyEigen$vectors[,2],col="yellow",pch=19)
box()
axis(1,padj=-0.5)
axis(2,padj=0.5)
title(main="First Two Principle Components-4 Classes EM")
title(xlab="PC 1",line=1.65)
title(ylab="PC 2",line=1.65)

write.csv(KMcolmean, file = "ClassficationData1.csv")

###write labeled data
write.csv(cbind(MyData,EMC$class),file = "labeleddata.csv")


