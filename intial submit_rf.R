
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

##
## generate results CSV file for Random Forest model 

getwd()

setwd()

test1 <-rm(colnames(test))
solution2 <- data.frame(test[,c(-3,-7)],popularity=ypred)
write.table(solution2[,7], file = "car_populary_rf.csv", 
          row.names=FALSE,col.names = FALSE,quote = FALSE)
