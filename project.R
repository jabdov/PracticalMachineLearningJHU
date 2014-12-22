library(caret)
set.seed(1991)
training<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
training<-training[,-1]
testing<-testing[,-1]
inTrain<-createDataPartition(training$classe,p=0.7,list=FALSE)
train<-training[inTrain,]
test<-training[-inTrain,]

'Preprocess:
table(train$num_window,train$classe) shows noise-free relationship
'

'1) Use dictionary num_window --> classe (, as testing data seem to be a sample of overall data and classe is the same for the same num_window)'
predictions=rep(factor(c("A","B","C","D","E")),dim(testing)[1]%/%5)
for (i in 1:dim(testing)[1])
{
  predictions[i]<-train$classe[which(train$num_window==testing$num_window[i])[1]]
}
predictions

'2) Use FR model with only num_window as predictor'
modFit<-train(classe ~ num_window,method="rf",data=train)
modFit$finalModel
confusionMatrix(predict(modFit,newdata=test),test$classe)
predict(modFit,newdata=testing)

'3) Preprocess PCA to keep 80% of variance and apply RF model'
trainred<-train[,-c(grep("^max|^min|^amplitude|^avg|^var|^stddev|^kurtosis|^skewness",colnames(train)))]
testred<-test[,-c(grep("^max|^min|^amplitude|^avg|^var|^stddev|^kurtosis|^skewness",colnames(test)))]
testingred<-testing[,-c(grep("^max|^min|^amplitude|^avg|^var|^stddev|^kurtosis|^skewness",colnames(testing)))]

preProc<-preProcess(trainred[,-c(1,2,3,4,5,6,59)],method="pca",thresh=0.80)
trainredPC<-predict(preProc,trainred[,-c(1,2,3,4,5,6,59)])
testredPC<-predict(preProc,testred[,-c(1,2,3,4,5,6,59)])
testingredPC<-predict(preProc,testingred[,-c(1,2,3,4,5,6,59)])

g<-train(trainred$classe ~ ., method="rf",data=trainredPC)
confusionMatrix(testred$classe,predict(g,testredPC))
mean(testred$classe==predict(g,testredPC))

mean(predictions==predict(g,testingredPC))
predictions==predict(g,testingredPC)

'4) Selected features'
g2<-train(classe ~ gyros_arm_x + gyros_arm_y + magnet_forearm_y + gyros_forearm_y + total_accel_arm + gyros_dumbbell_y, method="rf",data=trainred)
confusionMatrix(testred$classe,predict(g2,testred))
mean(testred$classe==predict(g2,testred))

mean(predictions==predict(g2,testingred))
predictions==predict(g2,testingred)

