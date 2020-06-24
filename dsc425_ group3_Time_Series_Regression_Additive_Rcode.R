## Cynthia Cho
## GROUP 3
## DSC 425
## Final Project Code
# Due: December 1, 2019



# importing libraries
library(DataCombine)
library(Metrics)
library (ggplot2)
library (moments)
library(forecast)
library(lmtest)
library(astsa)
library(tseries)
library(SciViews)


# loadig data
df=read.csv('C:/Users/Cindy/Documents/DSC 425/Project/Data/GlobalLandTemperaturesByCity.csv')
str(df)
head(df)
df$City

# Subsetting for only Baku
Baku_full <- subset(df, City == 'Baku', select=c(dt,AverageTemperature)) 
Baku_full$dt = as.Date(Baku_full$dt, format = "%Y-%m-%d")
Baku_full$AverageTemperature  = ts(Baku_full$AverageTemperature, start=c(1808, 10), frequency =12)

Baku_full[!complete.cases(Baku_full),]

frequency(Baku_full$AverageTemperature)
length(Baku_full$AverageTemperature)
plot(Baku_full$AverageTemperature)


# Subsetting the data for observations starting from 2000(yr)
Baku_2000 <- subset(Baku_full, dt >= '2000-1-1', select=c(dt, AverageTemperature))
DropNA(Baku_2000, message = TRUE)
Baku_2000 <- na.omit(Baku_2000)

# Converting into TS data
Baku_2000$AverageTemperature  = ts(Baku_2000$AverageTemperature, start=c(2000, 1), frequency =12)

str(Baku_2000)
length(Baku_2000$AverageTemperature)
frequency(Baku_2000$AverageTemperature)  # frequency is shown as 12

# Plotting the data
plot(Baku_2000$AverageTemperature, ylab="Average Temperature in Celcius", main="Baku Temperature from 2000-2012")


# ACF and PACF plots to see what the autocorrelation looks like
Baku_2000$AverageTemperature
Baku_2000$AverageTemperature  %>% autoplot()
Baku_2000$AverageTemperature  %>% acf()
Baku_2000$AverageTemperature  %>% pacf()
Baku_2000$AverageTemperature  %>% acf2()
Baku_2000$AverageTemperature  %>% eacf()


diff_Baku_2000 = Baku_2000$AverageTemperature %>% diff(12)
diff_Baku_2000%>% acf2()
kpss.test(diff_Baku_2000, null=("Level"))
kpss.test(diff_Baku_2000, null=("Trend"))

sd(training)
##########################################################################################################
#
# Splitting the data betwewen training and testing
# Train
training = window(Baku_2000$AverageTemperature, start=c(2000,1), end=c(2011,12))
str(training)
# Test
test = window(Baku_2000$AverageTemperature, start=2012, end=c(2012,12))
str(test)

#########################################################################################################
#
# Time Series Regression Modeling
lm_season_train = tslm(training~ season)  
#lm_season_trend = tslm(Baku_2000$AverageTemperature~ trend + season)
summary(lm_season_train)
coeftest(lm_season_train)  ## al but coeff 2 is significant

## Goodness of Fit Test
plot(training, main = "Goodness of Fit Test")
lines(lm_season_train$fitted, col='blue')

## Test to include trend proves to be insignificant
#lm_trend_lin_full = tslm(sub_Baku$AverageTemperature~trend + season) #Trend is insignficant

train_res = lm_season_train$residuals

AIC(lm_season_train)
########################################################################################################
#
### Residual Analysis

## ACF AND plot of residuals
acf(lm_season_train$residuals, main="ACF of Residuals")
plot(lm_season_train$residuals, main="Residuals Plot")
abline(h=0)

# QQ PLOT
qqnorm(lm_season_train$residuals, main="QQ-Plot of Residuals")
qqline(lm_season_train$residuals)

## Historgram
qplot(lm_season_train$residuals, fill=I("blue"), col=I("black"),bins = 10, main="Residuals Histogram",xlab="Residuals")
lines(density(lm_season_train$residuals))

# Residual Plot
plot_resid = as.numeric(lm_season_train$residuals)
plot(plot_resid, main="Residual Plot", ylab="residuals")
acf(lm_season_train$residuals, main="ACF of Residuals")


# Ljung Box test for residuals 1-20
# Also creates a plot with a 0.05 threshold

result <- vector("numeric", 20)
lags <- vector("numeric", 20)

for (i in 1:20) {
  lags[i] <-i
}
for (i in 1:20) {
  result[i] <-Box.test(lm_season_train$residuals, lag=i, type = 'Ljung-Box')$p.value
}
lags
result

resid_table <- data.frame(lags,result)
resid_table

# dot chart if Ljung Box p-values 
ggplot(resid_table) + geom_point(aes(x = lags, y = result),size=3) + geom_hline(yintercept=0.05, linetype="dashed",
        color = "red") +ggtitle("Plot of Ljung Box P-Values") 


# Normality Test - not normal
normalTest(plot_resid,method=c('jb'))
# Jarque-Bera Test
resid_num = as.numeric(lm_season_train$residuals)
jarque.test(resid_num)

#############################################################################################################
#
### FORECASTING FOR 2012 - 12 MONTHS
#fcast = forecast(lm_season_train, newdata=as.data.frame(test))
fcast = forecast(lm_season_train, h=12)
fcast
plot(fcast)

fcast$residuals
x = plot(fcast$fitted)     

plot.ts(fcast$residuals)     

fcast$mean ## point forecast values

# Overlay plot
autoplot(training, main="Average Monthly Temperature Prediction for 2012")  +
  autolayer(fcast) + autolayer(test)

plot(Baku_2000$AverageTemperature)

## Calculating the RMSE and MAPE for the model

rmse(fcast$mean, test)
mape(fcast$mean, test)



#############################################################################################################
## Testing out the best model using SARIMA

Baku_2000$AverageTemperature  %>% acf2()
Baku_1=Baku_2000$AverageTemperature  %>% diff(12)
Baku_1%>% acf2()
ma1 = Arima(Baku_2000$AverageTemperature, order=c(0,0,1), seasonal=c(1,1,1), include.mean=F) # 0 means take log transformation  -7.277559
sarima(Baku_2000$AverageTemperature, 1, 0,1, P = 1, D = 1, Q = 1, S=12, no.constant = FALSE)
coeftest(ma1)


M1 = auto.arima(training,ic=c("aic"), seasonal = TRUE)  # setting seasonality to False as there is no seasonality
M1
M1_resid = residuals(M1)
coeftest(M1)
M2 = Arima(Baku_2000$AverageTemperature, order=c(0,0,1), seasonal=c(0,1,2), include.mean=F) # 0 means take log transformation  -7.277559
coeftest(M2)
AIC$M1

sarima(training, 0, 0,1, P = 1, D = 1, Q = 1, S=12, no.constant = TRUE)
# Normality Test - not normal

M1_resid_num = as.numeric(M1_resid)
jarque.test(M1_resid_num)

M1_fcast = forecast(M1, h=12)
M1_fcast
plot(M1_fcast)

M1_fcast$mean

autoplot(training, main="Average Monthly Temperature Prediction for 2012")  +
  autolayer(M1_fcast) + autolayer(test)


plot(Baku_2000$AverageTemperature)

rmse(M1_fcast$mean, test)
mape(M1_fcast$mean, test)


###########################################################################################################

# NEURAL NETWORKS RESIDUALS TO INCORPORATE INTO THE PAPER
# TO ADD FOR SAKE OF COMPLETENESS
nn_residuals=read.csv('C:/Users/Cindy/Documents/DSC 425/Project/Data/residuals.csv')
nn_residuals = ts(nn_residuals)

### Residual Analysis

## ACF AND plot of residuals
acf(nn_residuals, main="ACF of Residuals")
plot(nn_residuals, ylab="value", xlab="Residuals", main="Residuals Plot")
abline(h=0)

# QQ PLOT
qqnorm(nn_residuals, main="QQ-Plot of Residuals")
qqline(nn_residuals, color='red')

## Historgram
qplot(nn_residuals, fill=I("purple"), col=I("black"),bins = 10, main="Residuals Histogram",xlab="Residuals")
lines(density(nn_residuals))

# Residual Plot
plot_resid = as.numeric(lm_season_train$residuals)
plot(nn_residuals, main="Residual Plot", ylab="residuals")
acf(nn_residuals, main="ACF of Residuals")


# Jarque-Bera Test
nn_residuals_num = as.numeric(nn_residuals)
jarque.test(nn_residuals_num)


