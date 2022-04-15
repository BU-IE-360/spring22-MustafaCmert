#set the working directory to read the data
setwd("C:/Users/MUSTAFA CÖM/Desktop/Mustafa/Semester 8/IE360 - Statistical Forecasting/Homeworks/HW1/Data") 

#plot the graphs package
library(ggplot2) 
#manipulation of dates package
library(lubridate) 
#time series package
library(xts)
#forecast package
library(forecast)
#plot correlations package
library(corrplot) 
#ggplot2 extension for ts objects
library(ggfortify)

#----------------EVDS DATA-----------------

#read the weekly evds data in csv format
evds_data <- read.csv(file = "EVDS.csv", header = TRUE, sep = ";") 

#change colunm names properly
colnames(evds_data) <- c("Date", "BIST100Price", "BIST100Volume", "DollarRate")

#replace commas with dots then convert variable type from chr to num
evds_data$BIST100Price <- as.numeric(gsub(",", ".", 
                                          gsub("\\.", "", evds_data$BIST100Price)))
evds_data$BIST100Volume <- as.numeric(gsub(",", ".", 
                                           gsub("\\.", "", evds_data$BIST100Volume)))
evds_data$DollarRate <- as.numeric(gsub(",", ".", 
                                        gsub("\\.", "", evds_data$DollarRate)))

#convert date variable from chr to Date
evds_data$Date <- as.Date(evds_data$Date, format = "%Y-%m-%d")

#add years to data as factor
years <- c("2010":"2022")
evds_data$Year <- factor(format(evds_data$Date, "%Y"), levels = years)

#first 6 rows of the EVDS data
head(evds_data)
#summary of the EVDS data
str(evds_data)

#convert evds data to time series object
evds_ts <- ts(evds_data[-1], start = 2010, frequency = 52)

#time plots of EVDS data
autoplot(evds_ts[,"BIST100Price"], size=1) +
  geom_smooth(aes(y=evds_ts[,"BIST100Price"]),fill = NA, color="orange", size = 1) +
  ggtitle("BIST100 Price") + ylab("Price in TL") + xlab("Year") 

autoplot(evds_ts[,"BIST100Volume"], size=1) + 
  geom_smooth(aes(y=evds_ts[,"BIST100Volume"]), fill = NA, color="orange", size = 1) +
  ggtitle("BIST100 Volume") + ylab("VOlume in Thousand TL") + xlab("Year")

autoplot(evds_ts[,"DollarRate"], size=1) + 
  geom_smooth(aes(y=evds_ts[,"DollarRate"]),fill = NA, color="orange", size = 1) +
  ggtitle("Dollar Exchange Rate") + ylab("Exchange Rate") + xlab("Year")
  
#histograms of EVDS data
ggplot(data = evds_data, aes(x=BIST100Price)) +
  geom_histogram(bins = 8, alpha = 0.3, aes(color = Year, fill = Year)) +
  facet_wrap(Year~.,scales = "free",ncol = 5) +
  labs(title = "Histograms of BIST100 Prices for Years 2010-2022",
       x = "BIST100 Price",
       y = "Frequency") +
  theme(legend.position = "none") + 
  theme(axis.text.x=element_text(angle=60, hjust=1.5, vjust = 1.5))  

ggplot(data = evds_data, aes(x=BIST100Volume)) +
  geom_histogram(bins = 10, alpha = 0.3, aes(color = Year, fill = Year)) +
  facet_wrap(Year~.,scales = "free",ncol = 5) +
  labs(title = "Histograms of BIST100 Volumes for Years 2010-2022",
       x = "BIST100 Volume",
       y = "Frequency") +
  theme(axis.text.x=element_text(angle=60, hjust=1.5, vjust = 1.5)) +
  theme(legend.position = "none")

ggplot(data = evds_data, aes(x=DollarRate)) +
  geom_histogram(bins = 8, alpha = 0.3, aes(color = Year, fill = Year)) +
  facet_wrap(Year~.,scales = "free",ncol = 5) +
  labs(title = "Histograms of Dollar Exchange Rates for Years 2010-2022",
       x = "Dollar Exchange Rates",
       y = "Frequency") +
  theme(legend.position = "none")

#Box Plots of EVDS Data
ggplot(data = evds_data, aes(x=as.factor(Year), y=BIST100Price)) +
  geom_boxplot(aes(y=BIST100Price, fill = Year)) +
  labs(title = "Boxplots of BIST100 Prices for Years 2010-2022",
       x = "Years",
       y = "BIST100 Prices")

ggplot(data = evds_data, aes(x=as.factor(Year), y=BIST100Volume)) +
  geom_boxplot(aes(y=BIST100Volume, fill = Year)) +
  labs(title = "Boxplots of BIST100 Volumes for Years 2010-2022",
       x = "Years",
       y = "BIST100 Volumes") 


ggplot(data = evds_data, aes(x=as.factor(Year), y=DollarRate)) +
  geom_boxplot(aes(y=DollarRate, fill = Year)) +
  labs(title = "Boxplots of Dollar Rates for Years 2010-2022",
       x = "Years",
       y = "Dollar Rates") 

#-------------Google Trend Data------------------

#read the monthly google trend "Dolar" search data
gt_dolar_data <- read.csv(file = "GTDolar.csv", header = T) 
#change the names of the columns
colnames(gt_dolar_data) <- c("Date","DolarSearch")
#change the chr to num
gt_dolar_data$DolarSearch <- as.numeric(gt_dolar_data$DolarSearch)
#replace NA's with 0's
gt_dolar_data$DolarSearch[is.na(gt_dolar_data$DolarSearch)] = 0 
#turn months to complete date
gt_dolar_data$Date <- as.Date(parse_date_time(gt_dolar_data$Date,"Ym"),
                              format = "%Y-%m-%d")
#add years to data frame as factors
gt_dolar_data$Year <- factor(format(gt_dolar_data$Date, "%Y"), levels = years)

#read the monthly google trend "Borsa" search data
gt_borsa_data <- read.csv("GTBorsa.csv")
#change the names of the columns
colnames(gt_borsa_data) <- c("Date", "BorsaSearch") 
#change the chr to num
gt_borsa_data$BorsaSearch <- as.numeric(gt_borsa_data$BorsaSearch)

#combine gt_borsa and gt_dolar data 
gt_data <- cbind(gt_dolar_data, gt_borsa_data[,"BorsaSearch"])
colnames(gt_data) <- c("Date", "DolarSearch", "Year", "BorsaSearch")

#summary of Google Trend data
head(gt_data)
str(gt_data)

#convert google trend data to time series object
gt_ts <- ts(gt_data[-1], start = 2010, frequency = 12)

#time plots of Google Trend Data
autoplot(gt_ts[,"DolarSearch"],size = 1) +
  geom_smooth(aes(y=gt_ts[,"DolarSearch"]),fill = NA, color="orange",size = 1) +
  labs(title = "Dolar Search in Google Trend",
       x = "Time",
       y = "Search Volume" ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y %b") +
  theme(axis.text.x=element_text(angle=60, hjust=1.5, vjust = 1.5))

autoplot(gt_ts[,"BorsaSearch"], size=1) +
  geom_smooth(aes(y=gt_ts[,"BorsaSearch"]),fill = NA, color="orange",size = 1) +
  labs(title = "Dollar Exchange Rates",
       x = "Time",
       y = "Exchange Rate" ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y %b") +
  theme(axis.text.x=element_text(angle=60, hjust=1.5, vjust = 1.5))

#Box Plots of Google Trend Data

ggplot(data = gt_data, aes(x=Year, y=DolarSearch)) +
  geom_boxplot(aes(y=DolarSearch, fill=Year)) +
  labs(title = "Boxplots of Dolar Search for Years 2010-2022",
       x = "Years",
       y = "Dolar Search")

ggplot(data = gt_data, aes(x=Year, y=BorsaSearch)) +
  geom_boxplot(aes(y=BorsaSearch, fill=Year)) +
  labs(title = "Boxplots of Dolar Search for Years 2010-2022",
       x = "Years",
       y = "Borsa Search")

#-------------------Monthly EVDS Data------------------

#I tried to convert weekly data into monthly data by manipulation but I could not 
#managed it and it was very time consuming, so I decided to download monthly data 
#directly and manipulate it to find correlations between monthly EVDS data and
#Google Trend data

#read the monthly evds data in csv format
evds_monthly_data <- read.csv(file = "EVDSmonthly.csv", header = TRUE, sep = ";") 
#remove empty rows
evds_monthly_data <- evds_monthly_data[!apply(evds_monthly_data == "", 1, all),]
#change colunm names properly
colnames(evds_monthly_data) <- c("Date", "BIST100Price", "BIST100Volume", "DollarRate")

#replace commas with dots then convert variable type from chr to num
evds_monthly_data$BIST100Price <- as.numeric(gsub(",", ".", 
                                  gsub("\\.", "", evds_monthly_data$BIST100Price)))
evds_monthly_data$BIST100Volume <- as.numeric(gsub(",", ".", 
                                  gsub("\\.", "", evds_monthly_data$BIST100Volume)))
evds_monthly_data$DollarRate <- as.numeric(gsub(",", ".", 
                                  gsub("\\.", "", evds_monthly_data$DollarRate)))

#turn months to complete date
evds_monthly_data$Date <- as.Date(parse_date_time(evds_monthly_data$Date,"Ym"),
                                  format = "%Y-%m-%d")

#add years to data as factor
evds_monthly_data$Year <- factor(format(evds_monthly_data$Date, "%Y"), levels = years)

#create monthly time series to graph correlation plots
evds_monthly_data_ts <- cbind(ts(evds_monthly_data[-c(1,5)], start = 2010, frequency = 12), 
                              ts(gt_dolar_data$DolarSearch, start = 2010, frequency = 12),
                              ts(gt_borsa_data$BorsaSearch, start = 2010, frequency = 12))
colnames(evds_monthly_data_ts) <- c("BIST100 Price", "BIST100 Volume", "Dollar Rate", 
                                    "GT Dolar Search", "GT Borsa Search")

#-----------------------Correlations-----------------------

#correlation between BIST100 Price and Dollar Rate
cor.test(x = evds_data$BIST100Price, y = evds_data$DollarRate, 
         method = "pearson", alternative = "greater")

#correlation between BIST100 VOlume and Dollar Rate
cor.test(x = evds_data$BIST100Volume, y = evds_data$DollarRate, 
         method = "pearson", alternative = "greater")

#correlation between BIST100 Price and Dollar Rate
cor.test(x = evds_data$BIST100Price, y = evds_data$BIST100Volume,
         method = "pearson", alternative = "greater")

#correlation between BIST100 Price and Google Trend Borsa Search
cor.test(x = evds_monthly_data$BIST100Price, y = gt_borsa_data$BorsaSearch,
         method = "pearson", alternative = "greater")

#correlation between BIST100 Volume and Google Trend Borsa Search
cor.test(x = evds_monthly_data$BIST100Volume, y = gt_borsa_data$BorsaSearch, 
         method = "pearson", alternative = "greater")

#correlation between Dollar Rate and Google Trend Dolar Search
cor.test(x = evds_monthly_data$DollarRate, y = gt_dolar_data$DolarSearch, 
         method = "pearson", alternative = "greater")


#Correlation plots
corrplot.mixed(cor(evds_monthly_data_ts, use="pairwise.complete.obs"), 
               tl.col="black", tl.pos = "lt")

#Scatter plots
par(mfrow=c(1,3))
plot(x = evds_data$BIST100Price,y = evds_data$DollarRate,
     main = "BIST100 Price vs. Dollar Rate",
     xlab = "BIST100 Price", ylab = "Dollar Rate")
plot(x = evds_data$BIST100Volume,y = evds_data$DollarRate,
     main = "BIST100 Volume vs. Dollar Rate",
     xlab = "BIST100 Volume", ylab = "DollarRate")
plot(x = evds_data$BIST100Price,y = evds_data$BIST100Volume,
     main = "BIST100 Price vs. BIST100 Volume",
     xlab = "BIST100 Price", ylab = "BIST100 Volume)")

par(mfrow=c(1,3))
plot(x = evds_monthly_data$BIST100Price,y = gt_borsa_data$BorsaSearch,
     main = "BIST100 Price vs. GT Borsa Search",
     xlab = "BIST100 Price", ylab = "GT Borsa Search")
plot(x = evds_monthly_data$BIST100Volume,y = gt_borsa_data$BorsaSearch,
     main = "BIST100 Volume vs. GT BorsaSearch",
     xlab = "BIST100 Volume", ylab = "GT BorsaSearch")
plot(x = evds_monthly_data$DollarRate,y = gt_dolar_data$DolarSearch,
     main = "Dollar Rate vs. GT DolarSearch",
     xlab = "Dollar Rate", ylab = "GT DolarSearch")


