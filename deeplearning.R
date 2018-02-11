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

#data$maintainence_Buy_price <- data$buying_price *data$maintainence_cost

## lugaguge size and number of seats ratio

#data$laguage_seats_ratio <- data$luggage_boot_size/data$number_of_seats

str(data)

# rearrange columns

#data <- data[,c(1,2,8,3,4,5,9,6,7)]

## split train and test from full dataset

data$popularity[data$popularity=="0"]<-NA

table(is.na(data))

train <-subset(data, (!is.na(data[,7])))
test <-subset(data, (is.na(data[,7])))
table(is.na(train))
table(is.na(test))

test <- test[,-7]

##train model
str(train)

train$popularity<-as.factor(train$popularity)
str(train)

# Feature Scaling
#train[-7] = scale(train[-7])
#test[,c(1:6)] = scale(test[,c(1:6)])

##install h20

#install.packages('h2o')
library('h2o')

h2o.init(nthreads=-1)

model <- h2o.deeplearning(y='popularity',
                          training_frame = as.h2o(train),
                          activation = 'RectifierWithDropout',
                          distribution="multinomial",
                          hidden=c(8,8),
                          epochs = 100,
                          input_dropout_ratio = 0.22,
                          l1 = 1e-5,
                          train_samples_per_iteration = -2)
## prdict test set

h2o.confusionMatrix(model, as.h2o(train))

ypred <-h2o.predict(model,newdata = as.h2o(test))
ypred <-as.data.frame(ypred)['predict']

ypred_train <-h2o.predict(model,newdata = as.h2o(train[-7]))
ypred_train <-as.data.frame(ypred_train)['predict']

str(ypred_train)
 

h2o.shutdown()
TRUE
yactual <- train$popularity

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

##
## generate results CSV file for Random Forest model 

getwd()

setwd()

test1 <-rm(colnames(test))
solution2 <- data.frame(test,popularity=ypred)
write.table(solution2[,9], file = "car_populary_dt_rf.csv", 
            row.names=FALSE,col.names = FALSE,quote = FALSE)
