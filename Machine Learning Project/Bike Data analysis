BikeData-analysis
Bike sharing is the new way of renting bike where members rent and return has become automatic. Through this system, user can easily rent bike from one point and return at other point. Right now there are 500 different bike sharing systems around the world. I downloaded the dataset Bike Sharing Dataset from the UCI machine learning database link and performed some of the Exploratory analysis and Multivariate regression model on 4 independent variable with one dependent variable which is total bike rental.

SUMMARY STATS: Attributes	Mean	Median	Variance Count	4504.35	4548	3752788 Temperature	0.4744	0.486	0.0266

CORRELATION MATRIX

According to the correlation plot and matrix, there is slightly positive correlation between the dependent variable and actual temperature of 0.63. There is also some correlation between the season and dependent variable of around 0.40. Where as weather situation and dependent variable showed negative correlation of -0.297 and also correlation between holiday and total rental is negative. The highest correlation is seen between the season and temperature. This shows that the change in temperature and season effect the bike rental frequency. ANALYSIS: MULTIVARIANT REGRESSION ANALYSIS: The choice of algorithm was to calculate/compute the multivariate regression analysis, because we have to predict the numerical outcome of the bike rental. The response variable in this case is Bike rental count. The regression is performed well in numeric and normal distributed variables. Linear regressions are often used to predict the influence of variables to the values. They have ability to easily identify outliers in data. The linear regression is also dependent on the quality of data points. The Rsquare and RMSE from the model is : 0.5724 and 1194.066, which are not too bad because the 25th percentile of the variable is around 2500. The overall dataset was not that massive have only 731 observations and after picking 4 independent variables against a response variable its not very bad.

#Data Set: df1 <- data.frame(bikedata$atemp, bikedata$season, bikedata$cnt, bikedata$weathersit, bikedata$holiday) colnames(df1)[1] <- "Temperature" colnames(df1)[2] <- "season" colnames(df1)[3] <- "Total Rental" colnames(df1)[4] <- "Weather" colnames(df1)[5] <- "Holiday"

#Multiple Regression function regressionModel <- function(trainset,testset){ y <- trainset$cnt

matrix of feature variables from dataset
x <- as.matrix(trainset[,1:5]) int <- rep(1, length(y))

add an intercept to the variables
#x <- cbind(int, x)

calculate the beta / Coefficent
beta <- solve(t(x) %% x) %% t(x) %*% y

intercept<-beta[1] seasoncoef<-beta[2] holidaycoef<-beta[3] weathercoef<-beta[4] atempcoef<-beta[5]

model <- intercept + seasoncoef * testset$season + holidaycoef * testset$holiday + weathercoef * testset$weathersit + atempcoef * testset$atemp

return(model) }
