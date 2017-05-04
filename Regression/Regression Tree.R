###ISyE 6740 Project European Player Rating System
## Regression Tree
setwd('D:/Acedemic GT/ISyE 6740/Project')
MyData<-read.csv('CleanData.csv')
Rate<-as.matrix(MyData[,4])
Attr<-as.matrix(MyData[,c(9:41)])
RAttr<-as.matrix(MyData[,c(4,9:41)])
# df <- as.data.frame(MyData)
# for(i in c(4,9:41)){
#   MyData[,i]<-as.factor(MyData[,i])
# }

library(rpart)
library(party)

# grow tree 
fit <- rpart(overall_rating~crossing	+finishing	+heading_accuracy
             +short_passing	+volleys	+dribbling	+curve	
             +free_kick_accuracy	+long_passing	+ball_control	
             +acceleration	+sprint_speed	+agility	+reactions	
             +balance	+shot_power	+jumping	+stamina	+strength	
             +long_shots	+aggression	+interceptions	+positioning	
             +vision	+penalties	+marking	+standing_tackle	
             +sliding_tackle	+gk_diving	+gk_handling	+gk_kicking	
             +gk_positioning	+gk_reflexes, 
             method="anova", data=MyData)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", 
     title = "Regression Tree for Mileage ")

# Random Forest prediction of Kyphosis data
library(randomForest)
fit <- randomForest(overall_rating~crossing	+finishing	+heading_accuracy
                    +short_passing	+volleys	+dribbling	+curve	
                    +free_kick_accuracy	+long_passing	+ball_control	
                    +acceleration	+sprint_speed	+agility	+reactions	
                    +balance	+shot_power	+jumping	+stamina	+strength	
                    +long_shots	+aggression	+interceptions	+positioning	
                    +vision	+penalties	+marking	+standing_tackle	
                    +sliding_tackle	+gk_diving	+gk_handling	+gk_kicking	
                    +gk_positioning	+gk_reflexes, 
                    data=MyData)
print(fit) # view results 
importance(fit) # importance of each predictor

