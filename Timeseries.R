# Data description

## Load datas

# ts is our time series
ts<-read.csv("monthly-hotel-occupied-room-av-6.csv")
names(ts)[2]="rmNumber" # rename as rmNumber=room number
ts<-na.omit(ts)

# show the example of the data
head(ts,4)
summary(ts)

## Plot the original data
plot(ts,xlab="time",ylab="Room Number",pch=c(16))
lines(ts,type="l",col = "steelblue",xlab="time",ylab="Room Number",pch=c(16))

# Moving Average Smoothing
## Define the moving average function
# Moving Average filter function
# n = size of the window of the moving average
# data = time series to apply SMA on
SMA <- function(data, n){
  size=nrow(data)
  result=vector(mode="numeric",length=size)
  sum=0
  a=data$rmNumber
  for (i in 1:n) {
    sum=sum+a[i]
    result[i]=sum/(i+1)
  }
  for (i in (n+1):size) {
    sum = sum-a[i-n]+a[i]
    result[i] = sum/n
  }
  return(result)
}

## Moving Average result
window=4 # size of the window of MA
ts1=SMA(ts, window)
plot(ts,xlab="time",ylab="Room Number",pch=c(16))
# plot the lines for the original data
lines(ts,type="l",col = "steelblue",xlab="time",ylab="Room Number",pch=c(16))
# plot the lines for the data after moving average
lines(ts$Month,ts1,type="l",col = "red",xlab="time",ylab="Room Number",pch=c(16))

## Removing the trend
```{r}
ts3=ts$rmNumber-ts1 #ts1 is the result of moving average
plot(ts$Month,ts3,xlab="time",ylab="Room Number",pch=c(16))
lines(ts$Month,ts3,col = "steelblue", xlab="time",ylab="Room Number",pch=c(16))


# Exponential Smoothing
ES <- function(s,alpha){
  size=nrow(s)
  s_out=vector(mode="numeric",length=size)
  s_out[1]=s$rmNumber[1]
  for (i in 2:size) {
    s_out[i]=alpha*s$rmNumber[i]+(1-alpha)*s_out[i-1]
  }
  return(s_out)
}

## ES results
alpha=0.2 # size of the window of MA
ts_e=ES(ts, alpha)
plot(ts,xlab="time",ylab="Room Number",pch=c(16))
# plot the lines for the original data
lines(ts,type="l",col = "steelblue",xlab="time",ylab="Room Number",pch=c(16))
# plot the lines for the data after moving average
lines(ts$Month,ts_e,type="l",col = "red",xlab="time",ylab="Room Number",pch=c(16))

# Differencing
# Definition of difference operator
difference <- function(ts){
  size = nrow(ts)
  result=vector(mode="numeric",length=size)
  result[1] = ts[1,2]
  for (i in 2:(size)) {
    result[i]=ts[i,2]-ts[i-1,2]
  }
  return(result)
}

ts2 <- difference(ts)

plot(ts$Month,ts2,ts,xlab="time",ylab="Room Number",pch=c(16))
# plot the lines for the original data
lines(ts$Month,ts2,type="l",col = "orange",xlab="time",ylab="Room Number",pch=c(16))
