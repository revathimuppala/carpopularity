
 ## load train data set

train <- read.csv("file:///C:/Users/manoj/Desktop/Goldman sacks/train.csv")
test <- read.csv("file:///C:/Users/manoj/Desktop/Goldman sacks/test.csv", header=FALSE)
str(train)
summary(train)

colnames(train)

## add colnames to  test data

colnames(test) <-c("buying_price","maintainence_cost","number_of_doors",  
                   "number_of_seats","luggage_boot_size","safety_rating")

## combine train and test data sets for preprocess

## add dummy column with 0's for test set to make equal columns both in train and test set

test$popularity <-"0"

str(test)
 
data <- rbind(train,test)
str(data)


str(data)

## add feture_maintenence cost_buying

data$maintainence_Buy_price <- data$buying_price *data$maintainence_cost

## lugaguge size and number of seats ratio

data$laguage_seats_ratio <- data$luggage_boot_size/data$number_of_seats

str(data)

# rearrange columns

data <- data[,c(1,2,8,3,4,5,9,6,7)]

## split train and test from full dataset

data$popularity[data$popularity=="0"]<-NA

table(is.na(data))

train <-subset(data, (!is.na(data[,9])))
test <-subset(data, (is.na(data[,9])))
table(is.na(train))
table(is.na(test))

test <- test[,-9]

##train model
str(train)

train$popularity<-as.factor(train$popularity)
str(train)

model <- randomForest(train$popularity~.,data=train[,-9],importance=TRUE,proximity=TRUE,ntree=500)

## predict result

ypred <- predict(model,newdata = test)

ypred_train<- predict(model,newdata = train[,-9])

yactual <- train$popularity

## k-fold cross validation

library(caret)
folds = createFolds(train$popularity, k = 10)
cv = lapply(folds, function(x) {
  training_fold = train[-x, ]
  test_fold = train[x, ]
  model = randomForest(train$popularity~.,data=train[,-9],
                       importance=TRUE,proximity=TRUE,ntree=500)
  ypred_train<- predict(model,newdata = test_fold[,-9])
  
  yactual <- training_fold$popularity
  
  tpf <- 0
  tp <- 0
  for(label in c(1,2,3,4)){
    predictedPositives <-(ypred_train==label)*1
    actualPositives <- (yactual==label)*1
    truePositives <- (ypred_train==label& yactual==label)*1
    precision <- sum(truePositives)/sum(predictedPositives)
    recal<-sum(truePositives)/sum(actualPositives)
    f  <- 2*precision*recal/(precision+recal)
    tp <- tp+sum(truePositives)
    tpf<- tpf+(f*sum(truePositives))
  }
  score = 1000*tpf/tp
})

score = mean(as.numeric(cv))
score









