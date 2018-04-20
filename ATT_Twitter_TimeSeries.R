install.packages("data.table")

library("data.table")


data <- fread("ATT_Twitter.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

install.packages("timeSeries", repos = "https://cran.r-project.org")
install.packages("forecast", repos = "https://cran.r-project.org")
install.packages("rugarch",  repos = "https://cran.r-project.org", dependencies=TRUE)
#remove.packages("rgl")

head(data)


install.packages("dplyr")
library("dplyr")
data %>% summarise_all(funs(n_distinct(.)))

#checking for nulls
result<-sapply(data, function(x) length(unique(x)))
nas<-sapply(data, function(x) sum(is.na(x)))

#convert to date time format
# datetime <- as.POSIXct(data$time)
# str(datetime)
# 
# 
# newdata <- data
# newdata$time <- datetime

# install.packages("lubridate")
# library("lubridate")
# 
# newdata$date <- as.date(newdata$time)


#since the data doesnt have any 24 hour pattern, we are not converting the data to a 24 hour series
# firstHour <- (as.Date("2016-06-16 00:00:00")-as.Date("2016-1-1 00:00:00"))
# firstHour
# series <-ts(newdata$x, start = c(firstHour,1), frequency=24)
# series
# plot(series)


library("forecast")
x_ts <-ts(data$x)
x_ts
plot(x_ts)

#not rquired since the series doesnt have variance that chnages with time
# z=log10(series)
# plot(z)


# nas<-sapply(y, function(x) sum(is.infinite(x)))
# nas


#using Phillips-Perron unit Root test to check if the series is a Random walk
PP.test(x_ts)


#ACF and PACF plots
par(mfrow = c(1,2))
acf(x_ts,main='ACF Twitter sentiment')
pacf(x_ts,main='PACF Twitter sentiment')


#using auto.arima function to determine best ARIMA model for the data
ARIMAfit = auto.arima(x_ts, approximation=FALSE,trace=TRUE)
summary(ARIMAfit)

#differencing the data to be able to model the data using GARCH/apARCH models
y=diff(x_ts)
plot(y)


install.packages("quantmod", repos = "https://cran.r-project.org")
install.packages("lattice", repos = "https://cran.r-project.org")
install.packages("rugarch", repos = "https://cran.r-project.org", dependencies = TRUE)

library(quantmod)
library(lattice)
library(rugarch)

#using simple GARCH
#armaorder 0,0
spec1=ugarchspec(variance.model=list(model="sGARCH"),mean.model=list(armaOrder=c(0,0)))
fit1=ugarchfit(data=y,spec=spec1)
show(fit1)

#armaorder 0,1
spec2=ugarchspec(variance.model=list(model="sGARCH"),mean.model=list(armaOrder=c(0,1)))
fit2=ugarchfit(data=y,spec=spec2)
show(fit2)

#using simple apARCH
#armaorder 0,0
spec1ap=ugarchspec(variance.model=list(model="apARCH"),mean.model=list(armaOrder=c(0,0)))
fit1ap=ugarchfit(data=y,spec=spec1ap)
show(fit1ap)


#armaorder 0,1
spec2ap=ugarchspec(variance.model=list(model="apARCH"),mean.model=list(armaOrder=c(0,1)))
fit2ap=ugarchfit(data=y,spec=spec2ap)
show(fit2ap)
