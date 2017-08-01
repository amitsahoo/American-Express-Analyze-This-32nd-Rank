# American-Express-Analze-This
American Express Data Analytics Competition
Train <- read.csv("~/Desktop/American_Express/Analyze_This/Training_Dataset.csv")
View(Train)
Test <- read.csv("~/Desktop/American_Express/Analyze_This/Leaderboard_Dataset.csv")
View(Test)

#"Centaur=1,Cosmos=2,Ebony=3,Odyssey=4,Tokugawa=5" // party_voted_past,actual_vote-->numeric
#"Degree=0,Primary=1,Diploma=2,Masters=3" 

##Creating dummy varaiables
mvar32.train <- factor(Train$mvar32)
dummies_train = model.matrix(~mvar32.train)
View(dummies)

##Checking for missing pattern data
install.packages("VIM")
library(VIM)
mice_plot <- aggr(Train_2, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(Train_2), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))


##svm
library(e1071)
svm_model <- svm(actual_vote~.,Train_3)
Temp_Test <- subset(Test_3,select = c(mvar4,mvar5,mvar2,mvar1,mvar9,mvar32,mvar33,mvar10,mvar3,mvar7,mvar6,mvar23,mvar24,mvar25,
                                        +                                         mvar21,mvar8,mvar22,mvar17,mvar16,mvar32,mvar20,mvar19,mvar27.46.55.))
pred_svm <- predict(svm_model,Temp_Test)
View(pred_svm)
write.csv(pred_svm,file = "Team_datadragos_IITKGP_45.csv")

##NOT WORKING
svm_tune <- tune(svm, actual_vote~mvar4+mvar5+mvar2+mvar1+mvar9+mvar32+mvar33+mvar10+mvar3+mvar7+mvar6+mvar23+mvar24+mvar25+
                   mvar21+mvar8+mvar22+mvar17+mvar16+mvar32+mvar20+mvar19+mvar27.46.55., data=Train_3 ,kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)
svm_model_after_tune <- svm(actual_vote~mvar4+mvar5+mvar2+mvar1+mvar9+mvar32+mvar33+mvar10+mvar3+mvar7+mvar6+mvar23+mvar24+mvar25+
                              mvar21+mvar8+mvar22+mvar17+mvar16+mvar32+mvar20+mvar19+mvar27.46.55., data=Train_3, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)

##Converting into factor variabls for train
Train_2$actual_vote <- as.factor(Train_2$actual_vote)
Train_2$mvar27.25.35. <- as.factor(Train_2$mvar27.25.35.)
Train_2$mvar27.36.45. <- as.factor(Train_2$mvar27.36.45.)
Train_2$mvar27.46.55. <- as.factor(Train_2$mvar27.46.55.)
Train_2$mvar27.55.. <- as.factor(Train_2$mvar27.55..)
Train_2$mvar30 <- as.factor(Train_2$mvar30)
##Converting into factor variabls for test
Test_2$mvar27.25.35. <- as.factor(Test_2$mvar27.25.35.)
Test_2$mvar27.36.45. <- as.factor(Test_2$mvar27.36.45.)
Test_2$mvar27.46.55. <- as.factor(Test_2$mvar27.46.55.)
Test_2$mvar27.55.. <- as.factor(Test_2$mvar27.55..)
Test_2$mvar30 <- as.factor(Test_2$mvar30)
#Converting to factor
convert = c(1:24)
`Temp_Test`[,convert] = sapply(`Temp_Test`[,convert],as.numeric)


##Making subset of variabales which have missing values and imputationn
sample_Train_2 <- Train_2[c("mvar28","mvar29","mvar30")] 
imputed_Train_2 <- complete(mice(sample_Train_2))
summary(imputed_Train_2)
Train_2$mvar28 <- imputed_Train_2$mvar28
Train_2$mvar29 <- imputed_Train_2$mvar29
Train_2$mvar30 <- imputed_Train_2$mvar30

#Test imputation
sample_Test_2 <- Test_2[c("mvar28","mvar29","mvar30")] 
imputed_Test_2 <- complete(mice(sample_Test_2))
summary(imputed_Test_2)
Test_2$mvar28 <- imputed_Test_2$mvar28
Test_2$mvar29 <- imputed_Test_2$mvar29
Test_2$mvar30 <- imputed_Test_2$mvar30

##Naive Bayes on Train_2
#naive Bayes
m2 = naiveBayes(actual_vote~., data = new_train)
p2 = predict(m2, newdata = new_test)

##XGB
# xgboost parameters
library(xgboost)
param <- list("objective" = "multi:softmax",
              "num_class" = 13,
              "eval_metric" = "merror",    # evaluation metric 
              "nthread" = 8,   # number of threads to be used 
              "max_depth" = 16,    # maximum depth of tree 
              "eta" = 0.2,    # step size shrinkage 
              "gamma" = 0.01,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12)  # minimum sum of instance weight needed in a child 

#Convert labels to numeric:
num.class = length(levels(labels$country_destination))
levels(labels$country_destination) = 1:num.class

#Convert the data to matrix form:
#Convert the train_xg:
train.matrix = as.matrix(Train_2)
mode(train.matrix) = "numeric"

#Convert the test_xg:
test.matrix = as.matrix(Test_2)
mode(test.matrix) = "numeric"

#Convert the labels data:
x <- Train_2$actual_vote
labels.matrix <- as.matrix(labels$coun)
mode(labels.matrix) = "numeric"

# k-fold cross validation, with timing
nround.cv = 50
dtrain <- xgb.DMatrix(train.matrix, label=data.matrix(x))
xgboost.cv <- xgb.cv(param=param, data=dtrain,nfold=10, nrounds=nround.cv, prediction=TRUE, verbose=0)

# index of maximum auc:
min.merr.idx = which.min(xgboost.cv$dt[, test.merror.mean]) 
min.merr.idx
## [1] 13
# minimum merror
xgboost.cv$dt[min.merr.idx,]

# real model fit training, with full data
xgb.bst <- xgboost(param=param, data=train.matrix, label=data.matrix(x), 
                   nrounds=min.merr.idx, verbose=1)
pred <- predict(xgb.bst,test.matrix)

##XGB NEW
require(xgboost)
require(methods)

Temp_Train <- subset(Train_3,select = c(actual_vote,mvar4,mvar5,mvar2,mvar1,mvar9,mvar32,mvar33,mvar10,mvar3,mvar7,mvar6,mvar23,mvar24,mvar25,mvar21,mvar8,mvar22,mvar17,mvar16,mvar20,mvar19,mvar27.46.55.,Loc_change))

Temp_Test <- subset(Test_3,select = c(mvar4,mvar5,mvar2,mvar1,mvar9,mvar32,mvar33,mvar10,mvar3,mvar7,mvar6,mvar23,mvar24,mvar25,
                                        mvar21,mvar8,mvar22,mvar17,mvar16,mvar20,mvar19,mvar27.46.55.,Loc_change))

y = Temp_Train[,1]
y = gsub('Class_','',y)
y = as.integer(y)-1 #xgboost take features in [0,numOfClass)

x = rbind(Temp_Train[,-1],Temp_Test)
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))
trind = 1:length(y)
teind = (nrow(Temp_Train)+1):nrow(x)

# Set necessary parameter
param <- list("objective" = "multi:softmax",
              "num_class" = 13,
              "eval_metric" = "merror",    # evaluation metric 
              "nthread" = 8,   # number of threads to be used 
              "max_depth" = 10,    # maximum depth of tree 
              "eta" = 0.2,    # step size shrinkage 
              "gamma" = 0.01,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12)
              
# Run Cross Valication
cv.nround = 50
bst.cv = xgb.cv(param=param, data = x[trind,], label = y, 
                nfold = 5, nrounds=cv.nround)

# Train the model
nround = 8
bst = xgboost(param=param, data = x[trind,], label = y, nrounds=nround)

# Make prediction
pred_xgb = predict(bst,x[teind,])
pred = matrix(pred,9,length(pred)/9)
pred = t(pred)

# Output submission
pred = format(pred, digits=2,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),pred)
names(pred) = c('id', paste0('Class_',1:9))
write.csv(pred,file='submission.csv', quote=FALSE,row.names=FALSE)

##Random Forest
forest_model <- randomForest(actual_vote~mvar4+mvar5+mvar2+mvar1+mvar9+mvar32+mvar33+mvar10+mvar3+mvar7+mvar6+mvar23+mvar24+mvar25
                               +mvar21+mvar8+mvar22+mvar17+mvar16+mvar32+mvar20+mvar19+mvar27.46.55., data = Train_3,ntrees=150)
print(forest_model)
varImpPlot(forest_model)


##GBM
library(gbm)
library(caret)
fitControl <- trainControl(method="repeatedcv",
                           number=5,
                           repeats=1,
                           verboseIter=TRUE)
set.seed(825)
gbmFit <- train(actual_vote ~ ., data=Train_3,
                method="gbm",
                trControl=fitControl,
                verbose=FALSE)

importance = summary(gbmFit, plotit=TRUE)

##KNN Algo
library(class)
knn_model <- knn(Temp_Train,Temp_Test,ytrain,k=5)
summary(knn_model)
# Example of Stacking algorithms
# create submodels
set.seed(1234)
Temp_Train <- Temp_Train[sample(nrow(Temp_Train)),]
split <- floor(nrow(Temp_Train)/3)
ensembleData <- Temp_Train[0:split,]
blenderData <- Temp_Train[(split+1):(split*2),]
testingData <- Temp_Train[(split*2+1):nrow(Temp_Train),]

labelName <- 'actual_vote'
predictors <- names(ensembleData)[names(ensembleData) != labelName]
library(caret)
myControl <- trainControl(method="repeatedcv",
                          number=5,
                          repeats=1,
                          verboseIter=TRUE
                         
)

model_gbm <- train(actual_vote~., method='gbm', Train_3,trControl=myControl)
model_multi<-train(ensembleData[,predictors], ensembleData[,labelName], method='multinom', trControl=myControl)
model_nnet <- nnet(actual_vote~.,ensembleData, size=3,rang=0.07,Hess=FALSE,decay=15e-4,maxit=250,trControl=myControl)
model_svm <- train(actual_vote~.,Train_3, method='svmRadial', trControl=myControl)

blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,predictors])
blenderData$svm_PROB <- predict(object=model_svm, blenderData[,predictors])
blenderData$multinom_PROB <- predict(object=model_multi, blenderData[,predictors])
blenderData$nnet_PROB <- predict(object=model_nnet, blenderData[,predictors])


Temp_Test$gbm_PROB <- predict(object=model_gbm, Temp_Test[,predictors])
Temp_Test$svm_PROB <- predict(object=model_svm, Temp_Test[,predictors])
Temp_Test$nnet_PROB <- predict(object=model_nnet, Temp_Test[,predictors])
Temp_Test$multinom_PROB <- predict(object=model_multi, Temp_Test[,predictors])

predictors <- names(blenderData)[names(blenderData) != labelName]
final_blender_model <- train(actual_vote~., blenderData, method='svmRadial', trControl=myControl)

preds <- predict(object=final_blender_model, Temp_Test[,predictors])
auc <- roc(Temp_Test[,labelName], preds)
print(auc$auc)
## CLASSIFICATION USING NEURAL NETWORK

library(nnet)

## 1. Fit a Single Hidden Layer Neural Network using Least Squares
train.nnet<-nnet(actual_vote~.,Train_3,size=3,rang=0.07,Hess=FALSE,decay=15e-4,maxit=250)
## Use TEST data for testing the trained model
test.nnet<-predict(train.nnet,Test_3,type=("class"))
## MisClassification Confusion Matrix
table(Temp_Test$actual_vote,test.nnet)
## One can maximize the Accuracy by changing the "size" while training the neural network. SIZE refers to the number of nodes in the hidden layer.
which.is.max(test.nnet)  ## To Fine which row break ties at random (Maximum position in vector)

##2. Use Multinomial Log Linear models using Neural Networks
train.mlln<-multinom(actual_vote~.,new_train)
##USe TEST data for testing the trained model
test.mlln<-predict(train.mlln,new_test)
##Misclassification or Confusion Matrix
table(test$subscribed,test.mlln)
##3. Training Neural Network Using BACK PROPOGATION
install.packages("neuralnet")
library(neuralnet)
## Check for all Input Independent Variables to be Integer or Numeric or complex matrix or vector arguments. If they are not any one of these, then tranform them accordingly
str(train)
str(test)

library(caret)
set.seed(1234567)
train2<-createDataPartition(Train_3$actual_vote,p=0.7,list=FALSE)
trainnew<-Train_3[train2,]
testnew<-Train_3[-train2,]
str(trainnew)
str(testnew)
## Now lets run the neuralnet model on Train dataset
trainnew.nnbp<-neuralnet(actual_vote~.,data=Temp_Train,hidden=5,threshold=0.01,err.fct="sse",linear.output=FALSE,likelihood=TRUE,stepmax=1e+05,rep=1,startweights=NULL,learningrate.limit=list(0.1,1.5),learningrate.factor=list(minus=0.5,plus=1.5),learningrate=0.5,lifesign="minimal",lifesign.step=1000,algorithm="backprop",exclude=NULL,constant.weights=NULL)
## Here, Back Propogation Algorithm has been used. One can also use rprop+ (resilient BP with weight backtracking),rprop- (resilient BP without weight backtracking), "sag and "slr" as modified global convergent algorithm
## Accordingly the accuracy can be checked for each algorithm
summary(trainnew.nnbp)
## Plot method for the genralised weights wrt specific covariate (independent variable) and response target variable
gwplot(trainnew.nnbp,selected.covariate="actual_vote")
##(Smoother the Curve- Better is the model prediction)
## Plot the trained Neural Network
plot(trainnew.nnbp,rep="best")
## To check your prediction accuracy of training model
prediction(trainnew.nnbp)
print(trainnew.nnbp)
## Now use the TEST data set to test the trained model
## Make sure that target column in removed from the test data set
columns=c("age","job","marital","education","balance","housingloan","personalloan","lastcommtype","lastday","lastmonth","lastduration","numcontacts","pdays","poutcome")
testnew2<-subset(testnew,select=columns)
testnew.nnbp<-compute(trainnew.nnbp,Temp_Test,rep=1)
## MisClassification Confusion Matrix
table(testnew$subscribed,testnew.nnbp$net.result)
cbind(testnew$subscribed,testnew.nnbp$net.result)
print(testnew.nnbp) 

library("caret")
library("mlbench")
library("pROC")

set.seed(107)
inTrain <- createDataPartition(y = Train_3$actual_vote, p = .75, list = FALSE)
training <- Train_3[ inTrain,]
testing <- Train_3[-inTrain,]
folds=5
repeats=1
myControl <- trainControl(method='cv', number=folds, repeats=repeats, 
                          returnResamp='none', 
                          returnData=FALSE, savePredictions=TRUE, 
                          verboseIter=TRUE, allowParallel=TRUE
                        
)
library("caretEnsemble")
model_list <- caretList(
  actual_vote~mvar4+mvar5+mvar2+mvar1+mvar9+mvar32+mvar33+mvar10+mvar3+mvar7+mvar6+mvar23+mvar24+mvar25+
    mvar21+mvar8+mvar22+mvar17+mvar16+mvar32+mvar20+mvar19+mvar27.46.55., data=training,
  trControl=myControl,
  methodList=c("gbm","svmRadial")
)

#Make a greedy ensemble - currently can only use RMSE
greedy <- caretEnsemble(model_list, iter=1000L)
sort(greedy$weights, decreasing=TRUE)
greedy$error

svm_ensemble <- caretStack(model_list, method='svmRadial', trControl=trainControl(method='cv'))
linear$error

##LDA
library(MASS)
model_lda <- lda(actual_vote~mvar4+mvar5+mvar2+mvar1+mvar9+mvar32+mvar33+mvar10+mvar3+mvar7+mvar6+mvar23+mvar24+mvar25+
                   mvar21+mvar8+mvar22+mvar17+mvar16+mvar32+mvar20+mvar19+mvar27.46.55.,data=Train_3)
summary(model_lda)


## xgboost
Temp_Train_target <- Temp_Train$actual_vote
Temp_Train$actual_vote<-NULL
xgtrain <- xgb.DMatrix(data = data.matrix(Temp_Train), label = data.matrix(Temp_Train_target))
xgtest <- xgb.DMatrix(data = data.matrix(Temp_Test), missing = NA)

params <- list()
params$objective <- "multi:softmax"
params$eta <- 0.1
params$max_depth <- 5
params$subsample <- 0.9
params$colsample_bytree <- 0.9
params$min_child_weight <- 2
params$eval_metric <- "mlogloss"
params$num_class <- 13
model_xgb_cv <- xgb.cv(params=params, xgtrain, nrounds = 100, nfold = 5, early.stop.round = 30, prediction = TRUE)

model_xgb <- xgb.train(params = params, xgtrain, nrounds = 100)

vimp <- xgb.importance(model = model_xgb,feature_names = names(Temp_Train))
View(vimp)
pred_xgb<- predict(model_xgb,xgtest)
