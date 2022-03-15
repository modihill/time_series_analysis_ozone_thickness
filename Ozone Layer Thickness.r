# Clearing the environment
rm(list=ls())

# importing the necessary libraries 
library(readr)
library(TSA)
library(tseries)

# reading the data
ozone <- read.csv("data1.csv",header=FALSE)
head(ozone)

# class of ozone
class(ozone)


# converting dataframe into time series
ozoneTS = ts(ozone$V1, start = 1927)
head(ozoneTS)

# checking the class of ozoneTS
class(ozoneTS)

#----------------------------------------------------------------------
#------------------------------ Functions -----------------------------
#----------------------------------------------------------------------

# Functions for Time Series
plot_ts <- function(ts, transformation)
{
  win.graph(width = 20, 
            height = 10,
            pointsize = 8)
  plot(ts,
       ylab = "Dobson units",
       main = c(paste0(toString(transformation),
                       " plot of Changes in Ozone layer Thickness (dobson unit)")),
       type="o")
}


# Function for ACF and PACF
autoCorrelation <- function(ts, time_series)
{
  win.graph(width = 20, 
            height = 15,
            pointsize = 8)
  
  par(mfrow=c(2,1))
  acf(ts,
      main = c(paste0("ACF plot of ",toString(time_series))))
  
  pacf(ts, 
       main = c(paste0("PACF plot of ",toString(time_series))))
  par(mfrow=c(1,1))
}


# Functions for Model and its Residual Analysis
model <- function(ts, model, modelname)
{
  win.graph(width = 20, 
            height = 15,
            pointsize = 8)
  par(mfrow=c(1,1))
  plot(ts, 
       ylim = c(min(c(fitted(model), as.vector(ts))), 
                max(c(fitted(model),as.vector(ts)))),
       ylab='Change in Dobson units',
       main = c(paste0("Time Series Plot of change in Ozone Layer Thickniess (Yearly) and ", 
                       toString(modelname)," fitted")),
       type='o')
  
  lines(ts(fitted(model),
           start = 1927,
           end = 2016,
           frequency = 1),
        col="red", 
        type="l")
  
  
  res.model = rstudent(model)
  win.graph(width = 20, 
            height = 15, 
            pointsize=8)
  
  par(mfrow=c(3,2))
  
  plot(y = res.model, 
       x = as.vector(time(ozoneTS)),
       main = c(paste0("Time Series plot of Standardised residuals of fitted ", 
                       toString(modelname))),
       xlab = 'Time', 
       ylab = 'Standardized Residuals',
       type = 'o')
  
  hist(res.model,
       main = c(paste0("Histogram of Standardised residuals of fitted ", 
                       toString(modelname))),
       xlab='Standardized Residuals') 
  
  qqnorm(y=res.model, 
         main = c(paste0("QQ plot of standardised residuals of fitted ",
                         toString(modelname))))
  
  qqline(y=res.model, 
         col = 2, 
         lwd = 1, 
         lty = 2)
  
  shapiro.test(res.model)
  
  acf(res.model,
      main = c(paste0("ACF plot of standardised residuals of fitted ",
                      toString(modelname))))
  
  pacf(res.model, 
       main = c(paste0("PACF plot of standardised residuals of fitted ",
                       toString(modelname))))    
  par(mfrow=c(1,1))
}

#----------------------------------------------------------------------
#------------------------ Descriptive Analysis ------------------------
#----------------------------------------------------------------------


# Original Time Series
plot_ts(ozoneTS, "Time Series")

# ACF and PACF of original Time Series
autoCorrelation(ozoneTS, "Time Series")

win.graph(width = 20, 
          height = 15, 
          pointsize=8)

# Scatter Plot of Consecutive Years
plot(y=ozoneTS,
     x=zlag(ozoneTS),
     ylab='Dobson Units', 
     xlab='Previous Year Dobson Units', 
     main = "Scatter plot of Change in Ozone Layer Thickness in consequtive years.")

# Correlation of Consecutive Years
y = ozoneTS             
x = zlag(ozoneTS)       
index = 2:length(x)    
cor(y[index],x[index])

#----------------------------------------------------------------------
#------------------------------- Task 1 -------------------------------
#----------------------------------------------------------------------



#----------------------------
#-----Linear trend model-----
#----------------------------

model1 = lm(ozoneTS~time(ozoneTS))
summary(model1)

model(ozoneTS, model1, "Linear Model")

#-------------------------------
#-----Quadratic trend model-----
#-------------------------------

t = time(ozoneTS)
t2 = t^2
model2 = lm(ozoneTS~ t + t2)
summary(model2)

model(ozoneTS, model2, "Quadratic Model")

#-------------------------------
#-----Seasonal trend model------
#-------------------------------

ozoneTS_S <- ts(ozone, start=1927, frequency = 7)

season.=season(ozoneTS_S) 
model3=lm(ozoneTS_S~season. -1)
summary(model3)

model(ozoneTS, model3, "Seasonal Model")

#-------------------------------
#-----harmonic trend model------
#-------------------------------

har. <- harmonic(ozoneTS_S, 0.4)
data <- data.frame(ozoneTS_S, har.)
model4 <- lm(ozoneTS_S ~ cos.2.pi.t. + sin.2.pi.t. , data = data)
summary(model4)

model(ozoneTS, model4, "Cosine Model")

#--------------------------------
#------ Finding Best Model ------
#--------------------------------

AIC(model1,model2,model3,model4)
BIC(model1,model2,model3,model4)

#-------------------------------
#-------- Prediction -----------
#-------------------------------

h <- 5 
t <- time(ozoneTS)
t2 <- t^2

firstYear <- t[1]
lastYear <- t[length(t)]

newYear <- seq(lastYear+1, lastYear+h, 1)

newYearData <- data.frame(t = newYear, 
                          t2 = newYear^2) 

forecasting <- predict(model2, newdata = newYearData, interval = "prediction")

combine <- c(ozoneTS,forecasting[,1])

win.graph(width = 15, 
          height = 10, 
          pointsize=8)

plot(ozoneTS, 
     xlim= c(1927, 2016+1+h), 
     ylim = c(min(combine)-5, max(combine)+5), 
     ylab = "Change in Dobson units",
     main = "Forecast of quadratic model on Time series of Ozone layer thickness")

lines(ts(as.vector(forecasting[,3]), 
         start = c(2017), frequency = 1), 
      col="blue")

lines(ts(as.vector(forecasting[,1]), 
         start = c(2017), 
         frequency = 1), 
      col="red")

lines(ts(fitted(model2), 
         start = c(1927), 
         frequency = 1), 
      col="green")

lines(ts(as.vector(forecasting[,2]), 
         start = c(2017), 
         frequency = 1), 
      col="blue")

legend("topleft", 
       lty=1, 
       pch=1, 
       col=c("black","blue","red","green"), 
       text.width = 18,
       c("Change in Ozone Layer Thickness","5% forecast limits", "Forecasts","Fitted model"))

#----------------------------------------------------------------------
#------------------------------- Task 2 -------------------------------
#----------------------------------------------------------------------

# Check the stationarity
adf.test(ozoneTS)

# Moving the Time-Series for positive values
ozoneTSM <- ozoneTS + abs(min(ozoneTS))+1

#------------------------------------
#---------Log Transformation---------
#------------------------------------

ozoneTSMLog <- log(ozoneTSM)
plot_ts(ozoneTSMLog, "Log Transformation Time Series")

#------------------------------------
#------Box-cox Transformation--------
#------------------------------------
BC <- BoxCox.ar(ozoneTSM)

# box-cox confidence Interval
BC$ci

# maximum likelihood
lambda <- BC$lambda[which(max(BC$loglike) == BC$loglike)]
lambda

# box-cox transformation
ozoneTSMBC = ((ozoneTSM^lambda)-1)/lambda

plot_ts(ozoneTSMBC, "Box-Cox Transformation Time Series")

#------------------------------------
#-------first differencing-----------
#------------------------------------

ozoneTSMBC_Diff1 <- diff(ozoneTSMBC,
                         differences = 1)

plot_ts(ozoneTSMBC_Diff1, "Box-Cox Transformation First Difference Time Series")

# Check the stationarity
adf.test(ozoneTSMBC_Diff1)

#------------------------------------
#-----------Model Parameters---------
#------------------------------------

# ACF and PACF
autoCorrelation(ozoneTSMBC_Diff1, "Box-Cox Transformation First Difference Time Series")
# ARMIA(1,1,1), ARMIA(2,1,1), ARMIA(3,1,1)
# ARMIA(1,1,2), ARMIA(2,1,2), ARMIA(3,1,2)

# EACF
eacf(ozoneTSMBC_Diff1,
     ar.max = 10, 
     ma.max = 10)
# ARIMA(1,1,3), ARIMA(2,1,3),
# ARIMA(1,1,4), ARIMA(2,1,4)

# BIC
win.graph(width = 10, 
          height = 10,
          pointsize = 8)

par(mfrow=c(1,1))
res = armasubsets(y=ozoneTSMBC_Diff1,
                  nar=5,
                  nma=5,
                  y.name='test',
                  ar.method='ols')
plot(res)
# ARIMA(3,1,2), ARIMA(4,1,2)

# Possible set of Models
# ARMIA(1,1,1), ARMIA(2,1,1), ARMIA(3,1,1)
# ARMIA(1,1,2), ARMIA(2,1,2), ARMIA(3,1,2)
# ARIMA(1,1,3), ARIMA(2,1,3), 
# ARIMA(1,1,4), ARIMA(2,1,4)
# ARIMA(4,1,2)