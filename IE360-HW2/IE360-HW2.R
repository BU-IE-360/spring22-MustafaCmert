# Set working directory properly
setwd("C:/Users/MUSTAFA CÖM/Desktop/Mustafa/Semester 8/IE360 - Statistical Forecasting/Homeworks/HW2/IE360-HW2")

# Necessary Packages
library(ggplot2) # Plot   
library(forecast) # Forecast
library(xts) # Time Series
library(GGally) # To find correlations

# Manipulation of the data
data <- read.csv(file = "IE360_Spring22_HW2_data.csv", header = T, sep = ",")
colnames(data) <- c("Time", "UGS", "RNUV", "NLPG", "PU", "PG", "NUGV", "NDGV", "GNPA", "GNPC", "GNPT")
data$Time <- as.yearqtr(gsub("_", " ", data$Time)) 
data$UGS<- as.integer(gsub(" ","",data$UGS))
data$NLPG<- as.integer(gsub(" ","",data$NLPG))
data$NUGV<- as.integer(gsub(" ","",data$NUGV))
data$GNPA<- as.integer(gsub(" ","",data$GNPA))
data$GNPC<- as.integer(gsub(" ","",data$GNPC))
data$GNPT<- as.integer(gsub(" ","",data$GNPT))

# Summary of the data
str(data)

# Convert data to time series
data_ts0 <- xts(x = data[-1], order.by = data$Time, frequency = 4)

# Plot the time series of UGS
autoplot(data_ts0$UGS/1000) + 
  geom_smooth(fill = NA, color="orange", size = 1) +
  ggtitle("Unleaded Gasoline Sales in a Given Quarter") + xlab("Quarters") + 
  ylab("Unleaded Gasoline Sales in millions of m^3")

# Correlation pairs of given variables
ggpairs(data[2:11])

# Regression with given independent variables
model0 <- lm(UGS ~ NLPG + PU + PG + NUGV + NDGV, data = data_ts0)
summary(model0)
checkresiduals(model0)

# Regression with only NUGV variable
model0_1 <- lm(UGS ~ NUGV, data_ts0)
summary(model0_1)
checkresiduals(model0_1)

# Autocorrelation function
acf(data_ts0$UGS, na.action = na.pass)

# Add trend and seasonality
data$Trend <- 1:32 # since there are eight years of data and 4 quarters in a year
data$Quarter <- rep(1:4, 8)
data_ts1 <- xts(x = data[-1], order.by = data$Time, frequency = 4)

# Time series linear regression with trend and seasonality
model1 <- lm(UGS ~ Trend + as.factor(Quarter), data = data_ts1)
summary(model1)
checkresiduals(model1)

# Add one year (4 quarter) lagged variable
data$UGS_lag4 <- c(rep(0,4), data$UGS[0:28])
data_ts2 <- xts(x = data[-1], order.by = data$Time, frequency = 4)

# Time series linear regression with trend, seasonals and lagged variable
model2 <- lm(UGS ~ Trend + as.factor(Quarter) + UGS_lag4, data = data_ts2)
summary(model2)
checkresiduals(model2)

# Add dummy variable to 5th and 28th observations
data$Dummy <- rep(0,32)
data$Dummy[c(5,28)] <- c(1,1)
data_ts3 <- xts(x = data[-1], order.by = data$Time, frequency = 4)

# Time series linear regression with extra dummy variable
model2_1 <- lm(UGS ~ Trend + as.factor(Quarter) + UGS_lag4 + Dummy, data = data_ts3)
summary(model2_1)
checkresiduals(model2_1)

# Add NUGV variable to see if there is improvement in the model
model3 <- lm(UGS ~ Trend + as.factor(Quarter) + UGS_lag4 + NUGV + Dummy, data = data_ts3)
summary(model3)
checkresiduals(model3)

# Predict the missing values with model2 which gives the best model
predicted_UGS <- predict(model2_1,data_ts3)

# Final time series that contains predicted and actual UGS values
data_ts_final <- xts(x = data.frame(data$UGS, predicted_UGS), order.by = data$Time, frequency = 4)

# Predicted values for the four quarters of 2007
predicted_UGS[29:32]

# Plot predicted vs actual UGS
plot(data_ts_final/1000,
     legend.loc = "topright",
     main = "Actual vs. Predicted UGS in a Given Quarter",
     minor.ticks = "quarters",
     grid.ticks.on = "quarters",
     yaxis.right = FALSE, col = c("black","goldenrod"))


