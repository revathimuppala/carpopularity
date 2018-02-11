
## import train data set

## Car popularity prediction challenge
## model-Random Forest Classification
## import data sets
train <- read.csv("train.csv")
test <- read.csv("test.csv", header=FALSE)
str(train)
summary(train)

colnames(train)

## add colnames to  test data to combine both train and test datasets for data preprocessing

colnames(test) <-c("buying_price","maintainence_cost","number_of_doors",  
                   "number_of_seats","luggage_boot_size","safety_rating")

## combine train and test data sets for data pre process

## add dummy column with 0's for test set to make equal columns both in train and test set
test$popularity <-"0"

str(test)

data <- rbind(train,test)

## add new features
##1) maintainence


## add feature_maintenence + cost_buying, usually we could give w1*buying price + w2*maintenance_Cost

data$maintainence_Buy_price <- data$buying_price +data$maintainence_cost



## convert all variable in to factor since there are categorised
## keep number of doors and number of seats variabled as integers since they are numbers

str(data)

data$buying_price<-as.factor(data$buying_price)
data$maintainence_cost<-as.factor(data$maintainence_cost)
data$luggage_boot_size<-as.factor(data$luggage_boot_size)
data$safety_rating<-as.factor(data$safety_rating)
data$maintainence_Buy_price<-as.factor(data$maintainence_Buy_price)
str(data)

## add 
#data$laguage_seats_ratio <- data$luggage_boot_size/data$number_of_seats

# rearrange columns

data <- data[,c(1,2,8,3,4,5,6,7)]

## split train and test from full dataset

data$popularity[data$popularity=="0"]<-NA

table(is.na(data))

train <-subset(data, (!is.na(data[,8])))
test <-subset(data, (is.na(data[,8])))
table(is.na(train))
table(is.na(test))

## remove dummy column which we added already for test set

test <- test[,-8]

## covert response variable in to factor

train$popularity<-as.factor(train$popularity)

str(train)

## train model- random Forest
instal.packages('randomForest')
library(randomForest)

model <- randomForest(train$popularity~.,data=train[,-8],importance=TRUE,proximity=TRUE,ntree=1000)

## predict result for test set

ypred <- predict(model,newdata = test)

## generate results CSV file for Random Forest model 

solution2 <- data.frame(test[,c(-3,-7)],popularity=ypred)
write.table(solution2[,6], file = "prediction.csv", 
            row.names=FALSE,col.names = FALSE,quote = FALSE)
