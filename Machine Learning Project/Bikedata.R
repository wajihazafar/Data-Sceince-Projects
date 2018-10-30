install.packages("ggplot2")
install.packages("GGally")
install.packages("corrplot")
install.packages("MLmetrics")
library(ggplot2)
library(GGally)
library(corrplot)
library(scales)
library(grid)
library(RColorBrewer)
library(KernSmooth)
library(MLmetrics)

day <- read.csv("C:/Users/Wajiha/Desktop/day.csv")
bikedata <- subset(day, select = c("cnt","season","holiday","weathersit","atemp"), stringsAsFactors = F)
head(bikedata)

ggplot(bikedata, aes(x=atemp)) + geom_histogram(color = "red", fill = "pink")
ggplot(bikedata, aes(x=weathersit)) + geom_histogram(binwidth = 1, color = "green", fill = "lightgreen")
ggplot(bikedata, aes(x=season)) + geom_histogram(binwidth = 0.5, color = "blue", fill = "lightblue")
ggplot(bikedata, aes(x=cnt)) + geom_histogram(color = "orange", fill = "yellow")
ggplot(bikedata, aes(x=holiday)) + geom_histogram(binwidth = 0.5, color = "purple", fill = "lightblue")

#Mean and Median and Variance:
mean(bikedata$atemp)
mean(bikedata$cnt)
mean(bikedata$weathersit)
mean(bikedata$season)
mean(bikedata$holiday)
median(bikedata$atemp)
median(bikedata$holiday)
median(bikedata$season)
median(bikedata$cnt)
median(bikedata$weathersit)
var(bikedata$cnt)
var(bikedata$holiday)
var(bikedata$season)
var(bikedata$atemp)
var(bikedata$weathersit)
#Correlation matrix and plot
df1 <- data.frame(bikedata$atemp, bikedata$season, bikedata$cnt, bikedata$weathersit, bikedata$holiday)
colnames(df1)[1] <- "Temperature"
colnames(df1)[2] <- "season"
colnames(df1)[3] <- "Total Rental"
colnames(df1)[4] <- "Weather"
colnames(df1)[5] <- "Holiday"
cor(df1)   #Correlation of Data
corplot_data <- cor(df1)
corrplot(corplot_data, method = "circle")  #Correlation plot

#Multivariant Regression
set.seed(20)
sample <- sample.int(n = nrow(bikedata), size = floor(0.85*nrow(bikedata)), replace = F)
trainset = bikedata[sample, ]
testset = bikedata[-sample, ]

#Multiple Regression function
regressionModel <- function(trainset,testset){
  y <- trainset$cnt
  # matrix of feature variables from dataset
  x <- as.matrix(trainset[,1:5])
  int <- rep(1, length(y))
  # add an intercept to the variables
  #x <- cbind(int, x)
  # calculate the beta / Coefficent
  beta <- solve(t(x) %*% x) %*% t(x) %*% y
  
  intercept<-beta[1]
  seasoncoef<-beta[2]
  holidaycoef<-beta[3]
  weathercoef<-beta[4]
  atempcoef<-beta[5]
  
  model <- intercept + seasoncoef * testset$season + holidaycoef * testset$holiday + weathercoef * testset$weathersit + atempcoef * testset$atemp
  
  return(model)
}

Rsquare <- function(actual,predicted){
  return(1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2)))
}
# Mean Squared erorr:
rmse <- function(error){
  return(sqrt(mean(error^2)))
}

evalMetrix<-function(test_data){
  R2 <- Rsquare(testset$cnt,testset$pred)
  RMSE<-rmse(test_data$error)
  
  evaldf<-data.frame(Rsquare = R2, RMSE = RMSE)
  return(evaldf)
}

# Feature Scaling
trainset["atemp"] = scale(trainset["atemp"])
testset["atemp"] = scale(testset["atemp"])

predict <- regressionModel(trainset, testset) 
testset$predict <- predict

# error calculation:
testset$error <- testset$cnt - testset$predict
evaldf<-evalMetrix(test_data = testset)
evaldf
