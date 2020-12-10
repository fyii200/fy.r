#Machine Learning Exercise using the iris flowers dataset
rm(list=ls())
setwd("/Users/fabianyii/Desktop/fy.r/Machine Learning")
install.packages("caret")
install.packages('e1071', dependencies=TRUE)

#caret is a versatile package for machine learning in R
library(caret)
data(iris)
dataset <- iris

#80% of data goes to training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)

#20% of data goes to validation
validation <- dataset[-validation_index,]

#80% of data for TRAINING
dataset <- dataset[validation_index,]
dim(dataset)
#check types of the attributes
sapply(dataset, class)

#distribution of species in percentage
prop.table(table(dataset$Species))*100
summary(dataset)

#split dataset into x=input and y=output
x <- dataset[,1:4]
y <- dataset[,5]

#individual box and whisker plots for independent variables
par(mfrow=c(1,4))
for (i in 1:4){
  boxplot(x[,i], main=names(x)[i])
}

#multivariate plots
featurePlot(x=x, y=y, plot='box')
featurePlot(x=x, y=y, plot='density')

#run algorithms using 10-fold cross-validation

control <- trainControl(method='cv', number=10)
metric <- 'Accuracy'

##### BUILDING MODELS #######
# linear model: linear discriminant analysis (LDA)
set.seed(7)
fit.lda <- train(Species~., data=dataset, method='lda', metric=metric, trControl=control)

# non linear model: classification and regression trees (CART)
set.seed(7)
fit.cart <- train(Species~., data=dataset, method='rpart', metric=metric, trControl=control)

# non linear model: K-nearest neighbors (kNN)
set.seed(7)
fit.knn <- train(Species~., data=dataset, method='knn' ,metric=metric, trControl=control)

# complex non linear method: support vector machines (SVM)
set.seed(7)
fit.svm <- train(Species~., data=dataset, method='svmRadial', metric=metric, trControl=control)

# complex non linear method: random forest (RF)
set.seed(7)
fit.rf <- train(Species~., data=dataset, method='rf', metric=metric, trControl=control)

results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
#graphically compare the performance metrics between different models
dotplot(results)

#summarize best model: lda
print(fit.lda)

#validate the accuracy of the trained model (lda) on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)





























