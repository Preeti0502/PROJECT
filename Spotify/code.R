##importing data
library(readr)
data <- read_csv("C:/Users/k/Desktop/project/songdata.csv")
head(data,10)

##Data Pre-Processing
data1=data[,c(-1,-16,-17)]
names(data1)
is.null("data1")
str(data1)
head(data1,10)
summary(data1)

##DATA VISUALIZATION
#Boxplot
par(mfrow=c(2,7))
for(i in 1:13){
  boxplot(data1[,i],main=names(data1)[i])
}
  
#Examining Correlation between variables
library(psych)
pairs.panels(data1[,1:13],method="pearson",hist.col=7,
             density=TRUE,ellipses=TRUE)


##Data Normalization
library(caret)
preproc=preProcess(data1[,c(3,6,8,11,12)], method=c("center","scale"))
norm=predict(preproc, data1)
summary(norm)

##splitting dataset in train and test set
library(caTools)
data_sample=sample(1:nrow(norm),size = ceiling(0.75*nrow(norm)),replace = FALSE)
train_data=norm[data_sample,]
test_data=norm[-data_sample,]


##Logistic Regression
full=glm(target~., family=binomial,data=train_data)
summary(full)

library(MASS)
step=stepAIC(full,trace=FALSE)
summary(step)
step$anova
res_step=predict(step,test_data,target="response")
(table(actualvalue=test_data$target,predictedvalue=res_step>0.5))

res=predict(step,train_data,target="response")
library(ROCR)
ROCPred=prediction(res,train_data$target)
ROCPref=performance(ROCPred,"tpr","fpr")
plot(ROCPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

res_step=predict(step,test_data,target="response")
(table(actualvalue=test_data$target,predictedvalue=res_step>0.3))

##NEURAL NETWORKS
library(neuralnet)
library(tictoc)
tic()
nn=neuralnet(target~.-c("key","time_signature","energy","mode"),data=train_data,
             hidden=5,act.fct="logistic",linear.output=FALSE)
plot(nn)
toc()
pred=predict(nn,test_data)
table(test_data$target,pred[,1]>0.5)

tic()
nn=neuralnet(target~.-c("key","time_signature","energy","mode"),data=train_data,
             hidden=4,act.fct="logistic",linear.output=FALSE)
plot(nn)
toc()
pred=predict(nn,test_data)
table(test_data$target,pred[,1]>0.5)

##Random Forest
train_data$target <- as.character(train_data$target)
train_data$target <- as.factor(train_data$target)
library(randomForest)
tic()
rf=randomForest(target~acousticness + danceability + duration_ms + energy + 
                  instrumentalness + liveness + loudness + mode + speechiness + 
                  valence,data=train_data)
toc()
rf

 
