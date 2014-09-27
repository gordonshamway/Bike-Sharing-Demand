#open the training dataset

column.types <- c('factor', #have to correct that later
            'factor',
            'factor',
            'factor',
            'factor',
            'numeric',
            'numeric',
            'integer',
            'numeric',
            'integer',
            'integer',
            'integer')

train.raw <- read.csv("train.csv",colClasses = column.types)

#Convert the datetime into real datetime (german locale)
library(lubridate)
train.raw$datetime <- ymd_hms(train.raw$datetime)
train.raw$ger_datetime <- format(ymd_hms(train.raw$datetime), "%d.%m.%y %H:%M:%S")

#look for NAs
require(Amelia)
missmap(train.raw, main="Bike Sharing Data - Missing Values", col=c("yellow", "black"), legend=FALSE) #no NAs


#Feature Engineering

#extract the hour and weekday of the date
train.raw$hour <- hour(train.raw$datetime)
train.raw$year <- year(train.raw$datetime)
train.raw$month <- month(train.raw$datetime)
train.raw$weekday <- weekdays(train.raw$datetime)
train.raw$weekday <- as.factor(train.raw$weekday)
train.raw$combined <- paste(as.character(train.raw$weekday), "_", as.character(train.raw$hour), sep="")
train.raw$combined <- as.factor(train.raw$combined)

#Modeling
library(caret)
library(randomForest)
lm.model.casual.1 <- train(casual ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + as.factor(weekday), data=train.raw, method="lm")
rpart.model.registered.1 <- train(registered ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + year + month + weekday + hour, data=train.raw, method="rpart")
rf.model.registered.1 <- randomForest(registered ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + year + month + weekday + hour, data=train.raw, method="rpart")
#rpart.model1 <- train(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + as.factor(weekday), data=train.raw, method="rpart")
#rpart.model2 <- train(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + as.factor(combined), data=train.raw, method="rpart")
#glm.model1 <- train(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + as.factor(weekday), data=train.raw, method="glm")
#glm.model2 <- train(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + combined, data=train.raw, method="glm")
#glm.model3 <- glm(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + combined, data=train.raw, family="poisson")
#glm.model4 <- glm(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + combined, data=train.raw, family="Gamma")
#glm.model99 <- train(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + combined, data=train.raw, method = "lm", family="Gamma", preProcess=c("center", "scale"))

#library(randomForest)
#rf.model1 <- randomForest(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + weekday, data=train.raw)

gbm.casual.model <- train(casual ~ year + month + hour + weekday + season + holiday + workingday + weather + temp + atemp + humidity, data=train.raw, method="gbm")
gbm.registered.model <- train(casual ~ year + month + hour + weekday + season + holiday + workingday + weather + temp + atemp + humidity, data=train.raw, method="gbm")


#methods to test
#blackboost  RMSE = 24.8 the longest
#bstTree RMSE = 20.6 with mstop = 150, maxdepth = 3 and nu = 0.1
#gbm (RMSE = 20.1 with shrinkage = 0,1 n.trees = 150 and interaction.depth = 3)
#treebag RMSE = 24.7
#avNNet didnt converge
#bayesglm 36.1

summary(model_test) #model_test
#step(lm.model.casual.1,direction="both",test="F")

#check RMSE
library(qpcR)
RMSE(glm.model99)


#RMSE
# rf = doesnt converge
# rpart = 138 (with cp of 0.0497802)
# rpart2 = 162 (with cp of 0.03159552)
# glm = 147
# glm2 = 88,7
# glm3 = 4,75
# glm4 = 0.50 # WE FOUND OUR WINNER

#new checking basic Metrics:rmlse
library(Metrics)
rmlse(TRUTH,predicted)


# lets made everything we did above for the test-dataset
column.types2 <- c('factor', #this time its 3 columns less than in training dataset
                  'factor',
                  'factor',
                  'factor',
                  'factor',
                  'numeric',
                  'numeric',
                  'integer',
                  'numeric')

test.raw <- read.csv("test.csv",colClasses = column.types2)
test.raw$datetime <- ymd_hms(test.raw$datetime)
test.raw$ger_datetime <- format(ymd_hms(test.raw$datetime), "%d.%m.%y %H:%M:%S")
test.raw$hour <- hour(test.raw$datetime)
test.raw$weekday <- weekdays(test.raw$datetime)
test.raw$weekday <- as.factor(test.raw$weekday)
test.raw$combined <- paste(as.character(test.raw$weekday), "_", as.character(test.raw$hour), sep="")
test.raw$combined <- as.factor(test.raw$combined)

#predict the counts
lm.model.casual.predictions <- predict(gbm.casual.model, newdata = test.raw)
glm.model3.prediction <- predict(glm.model99, newdata = test.raw)
rf.model1.predicitions <- predict(rf.model1, newdata = test.raw)

#prepare the output file
submit <- data.frame(datetime = test.raw$datetime, count = rf.model1.predicitions)
write.csv(submit, file = "submission1.csv", row.names = FALSE)
