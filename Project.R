
##Classification

##The packages installed.
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(readr)

##Importing the data using diectory path.
New_mobile_data <- read_csv("/Users/joyalkemmanuel/OneDrive/Data sets/New mobile data.csv")
View(New_mobile_data)

##Checking the summary of the data
summary(New_mobile_data)
colnames(New_mobile_data)

##Structure of the data.
str(New_mobile_data)


##Checking thw missing values and treating the vaplues.
sum(is.na(New_mobile_data))
New_mobile_data<- na.omit(New_mobile_data)
sum(is.na(New_mobile_data))
colnames(New_mobile_data)
unique(New_mobile_data$price_range)
unique(New_mobile_data$int_memory)


## Creating the function for the outlier treatment.
outlier <- function(data) {
  Q1<- quantile(data)[2]
  Q3<- quantile(data)[4]
  IQR<- Q3-Q1
  LL<- Q3 - 1.5*IQR
  UL<- Q3 + 1.5*IQR
  data[data > UL ] <- UL
  data[data < LL ] <- LL
  
  return (data)
}

##Checking the outlier and treating them.
## The prices are segregated according to their values.
P0<- dplyr::filter(New_mobile_data, price_range == 0)
P1<- dplyr::filter(New_mobile_data, price_range == 1)
P2<- dplyr::filter(New_mobile_data, price_range == 2)
P3<- dplyr::filter(New_mobile_data, price_range == 3)
counts <- table(New_mobile_data$ram)


str(P0)
str(P1)
str(P2)
str(P3)
#Splitting the screen fir the visulisation
par(mfrow = c(1,4))


boxplot(P0$battery_power)
boxplot(P1$battery_power)
boxplot(P2$battery_power)
boxplot(P3$battery_power)


boxplot(P0$clock_speed)
boxplot(P1$clock_speed)
boxplot(P2$clock_speed)
boxplot(P3$clock_speed)

boxplot(P0$front_camera,col = "Blue")
P0$front_camera <- outlier(P0$front_camera)
boxplot(P1$front_camera,col = "Blue")
P1$front_camera <- outlier(P1$front_camera)
boxplot(P2$front_camera,col = "Blue")
P2$front_camera <- outlier(P2$front_camera)
boxplot(P3$front_camera,col = "Blue")
P3$front_camera <- outlier(P3$front_camera)



boxplot(P0$int_memory)
boxplot(P1$int_memory)
boxplot(P2$int_memory)
boxplot(P3$int_memory)


boxplot(P0$m_depth)
boxplot(P1$m_depth)
boxplot(P2$m_depth)
boxplot(P2$m_depth)


boxplot(P0$m_width)
boxplot(P1$m_width)
boxplot(P2$m_width)
boxplot(P2$m_width)


boxplot(P0$n_cores)
boxplot(P1$n_cores)
boxplot(P2$n_cores)
boxplot(P3$n_cores)


boxplot(P0$prim_camera)
boxplot(P1$prim_camera)
boxplot(P2$prim_camera)
boxplot(P3$prim_camera)


boxplot(P0$px_width)
boxplot(P1$px_width)
boxplot(P2$px_width)
boxplot(P3$px_width)

boxplot(P0$px_height,col = "Green")
P0$px_height<-outlier(P0$px_height)
boxplot(P1$px_height,col="Green")
P1$px_height<-outlier(P1$px_height)
boxplot(P2$px_height)
boxplot(P3$px_height)


boxplot(P0$ram,col = "red")
P0$ram<-outlier(P0$ram)
boxplot(P1$ram,col = "red")
P1$ram<-outlier(P1$ram)
boxplot(P2$ram,col = "red")
P2$ram<-outlier(P2$ram)
boxplot(P3$ram,col = "red")
P3$ram<-outlier(P3$ram)

boxplot(P0$screen_height)
boxplot(P1$screen_height)
boxplot(P2$screen_height)
boxplot(P3$screen_height)


boxplot(P0$screen_width)
boxplot(P1$screen_width,col = "violet")
P1$screen_width <- outlier(P1$screen_width)
boxplot(P2$screen_width)
boxplot(P3$screen_width)

boxplot(P0$talk_time)
boxplot(P1$talk_time)
boxplot(P2$talk_time)
boxplot(P3$talk_time)

##Combining the rows after the outlier treatment.
mobile<- rbind(P0,P1,P2,P3)
str(mobile)

## Assign random values.
set.seed(250)

##Splittong the data into train and test.
ind<- sample(1: nrow(mobile), 0.7* nrow(mobile))
ind
train<- mobile[ind,]
test<- mobile[-ind,]
colnames(test)
colnames(train)

##Building model 
install.packages("MASS")
library(MASS)
install.packages("car")
library(car)

m1<- glm(price_range~., data= train)
summary(m1)

step<- stepAIC(m1, direction = 'both')

m2<- glm(price_range ~ battery_power + m_width + px_height + px_width + 
           ram, data= train)
summary(m2)
vif(m2)

m3<-glm(price_range ~ battery_power + m_width + px_width  + ram, data= train)
summary(m3)
vif(m3)

plot(m3)
ncol(test) 
View(test[,-1:-20])

##Predicting the model
test$pred<- predict(m2, test[,-21])
test$pred

a<- test$pred[test$price_range == 0]
b<- test$pred[test$price_range ==1]
c<- test$pred[test$price_range == 2]
d<- test$pred[test$price_range == 3]


## Visualising the the information using Vioplot
library(vioplot)
vioplot(a,col="Blue")
vioplot(b,col="Red")
vioplot(c,col = "Violet")
vioplot(d,col = "skyblue")

##Asigning the new vlues according the their value.
test$pred<- ifelse(test$pred < 0.6 ,0, ifelse(test$pred < 1.5,1, ifelse(test$pred < 2.4,2,3)))
plot(test$pred,col="SKYBLUE")

##Preding the outcome using confusion matrix
library(caret)
test$price_range<- as.factor(test$price_range)
test$pred<- as.factor(test$pred)
confusionMatrix(test$price_range,test$pred)


##The model is good as the prediction has given good results.

#LINEAR REGRESSION
##Building model 
install.packages("MASS")
library(MASS)
install.packages("car")
library(car)

m1<- lm(price_range~., data= train)
summary(m1)

step<- stepAIC(m1, direction = 'both')

m2<- lm(price_range ~ battery_power + clock_speed + m_width + px_height + 
          px_width + ram, data= train)
summary(m2)
vif(m2)


m3<- lm(price_range ~ battery_power+ m_width + px_height + 
          px_width + ram, data= train)
summary(m3)
vif(m3)

m4<- lm(price_range ~ battery_power+ m_width +
          px_width + ram, data= train)
summary(m4)
vif(m4)



plot(m3)
ncol(test) 
View(test[,-1:-20])

##Predicting the model
test$predi<-predict(m4,test[,-21])
r<-cor(test$price_range,test$predi)
r2<-r^2
r2

## The model is good as it gives .90% accuracy.

##DECISION TREE

##The packges installed.
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(readr)
install.packages("lattice")
library(lattice)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("data.tree")
library(data.tree)
library(caret)
library(readr)

##Importing the data using diectory path.
New_mobile_data <- read_csv("/Users/joyalkemmanuel/OneDrive/Data sets/New mobile data.csv")
View(New_mobile_data)

##Checking the summary of the data
summary(New_mobile_data)
colnames(New_mobile_data)
##Structure of the data.
str(New_mobile_data)


##Checking thw missing values and treating the vaplues.
sum(is.na(New_mobile_data))
New_mobile_data<- na.omit(New_mobile_data)
sum(is.na(New_mobile_data))
colnames(New_mobile_data)
unique(New_mobile_data$price_range)
unique(New_mobile_data$int_memory)


## Creating the function for the outlier treatment.
outlier <- function(data) {
  Q1<- quantile(data)[2]
  Q3<- quantile(data)[4]
  IQR<- Q3-Q1
  LL<- Q1 - 1.5*IQR
  UL<- Q3 + 1.5*IQR
  data[data > UL ] <- UL
  data[data < LL ] <- LL
  
  return (data)
}

##Checking the outlier and treating them.
## The prices are segregated according to their values.
P0<- dplyr::filter(New_mobile_data, price_range == 0)
P1<- dplyr::filter(New_mobile_data, price_range == 1)
P2<- dplyr::filter(New_mobile_data, price_range == 2)
P3<- dplyr::filter(New_mobile_data, price_range == 3)
counts <- table(New_mobile_data$ram)

str(P0)
str(P1)
str(P2)
str(P3)

plot(New_mobile_data$price_range,New_mobile_data$front_camera)
plot(New_mobile_data$battery_power,New_mobile_data$touch_screen)

plot((New_mobile_data[1:4]))
#Splitting the screen fir the visulisation



boxplot(P0$battery_power)
boxplot(P1$battery_power)
boxplot(P2$battery_power)
boxplot(P3$battery_power)


boxplot(P0$clock_speed)
boxplot(P1$clock_speed)
boxplot(P2$clock_speed)
boxplot(P3$clock_speed)

boxplot(P0$front_camera,col = "Blue")
P0$front_camera <- outlier(P0$front_camera)
boxplot(P1$front_camera,col = "Blue")
P1$front_camera <- outlier(P1$front_camera)
boxplot(P2$front_camera,col = "Blue")
P2$front_camera <- outlier(P2$front_camera)
boxplot(P3$front_camera,col = "Blue")
P3$front_camera <- outlier(P3$front_camera)



boxplot(P0$int_memory)
boxplot(P1$int_memory)
boxplot(P2$int_memory)
boxplot(P3$int_memory)


boxplot(P0$m_depth)
boxplot(P1$m_depth)
boxplot(P2$m_depth)
boxplot(P2$m_depth)


boxplot(P0$m_width)
boxplot(P1$m_width)
boxplot(P2$m_width)
boxplot(P2$m_width)


boxplot(P0$n_cores)
boxplot(P1$n_cores)
boxplot(P2$n_cores)
boxplot(P3$n_cores)


boxplot(P0$prim_camera)
boxplot(P1$prim_camera)
boxplot(P2$prim_camera)
boxplot(P3$prim_camera)


boxplot(P0$px_width)
boxplot(P1$px_width)
boxplot(P2$px_width)
boxplot(P3$px_width)

boxplot(P0$px_height,col = "Green")
P0$px_height<-outlier(P0$px_height)
boxplot(P1$px_height,col="Green")
P1$px_height<-outlier(P1$px_height)
boxplot(P2$px_height)
boxplot(P3$px_height)


boxplot(P0$ram,col = "red")
P0$ram<-outlier(P0$ram)
boxplot(P1$ram,col = "red")
P1$ram<-outlier(P1$ram)
boxplot(P2$ram,col = "red")
P2$ram<-outlier(P2$ram)
boxplot(P3$ram,col = "red")
P3$ram<-outlier(P3$ram)

boxplot(P0$screen_height)
boxplot(P1$screen_height)
boxplot(P2$screen_height)
boxplot(P3$screen_height)


boxplot(P0$screen_width)
boxplot(P1$screen_width,col = "violet")
P1$screen_width <- outlier(P1$screen_width)
boxplot(P2$screen_width)
boxplot(P3$screen_width)

boxplot(P0$talk_time)
boxplot(P1$talk_time)
boxplot(P2$talk_time)
boxplot(P3$talk_time)

##Combining the rows after the outlier treatment.
mobile<- rbind(P0,P1,P2,P3)
str(mobile)

## Assign random values.
set.seed(10)

##Splittong the data into train and test.
ind<- sample(1: nrow(mobile), 0.7* nrow(mobile))
ind
train<- mobile[ind,]
test<- mobile[-ind,]
colnames(test)
colnames(train)

##Building model 
install.packages("MASS")
library(MASS)
install.packages("car")
library(car)
library(carData)
install.packages("caTools")
library(caTools)

m1<- rpart(price_range~., data= train)

predi<-predict(m1, test)
prp(m1)

install.packages("caret")
library(caret)
test$price_range<-as.factor(test$price_range)

levels(test$price_range)

predi<- ifelse(predi < 0.6 ,0, ifelse(predi < 1.5,1, ifelse(predi< 2.4,2,3)))
predi<-as.factor(predi)
confusionMatrix(predi,test$price_range)


prp(m1, box.palette = "Blue", tweak = 1.0)

#Gini Index is a metric to measure how often a randomly chosen element would be incorrectly identified. It means an attribute with lower gini index should be preferred
gini<-train(price_range~., data = train, method = "rpart",
            parms = list(split = "gini"),
            tuneLength = 10)
gini
test_pred_gini <- predict(gini, newdata = test)

## The model is satisfactory as it gives only 0.76% accuracy.

