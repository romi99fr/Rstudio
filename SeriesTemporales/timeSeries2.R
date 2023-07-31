#Problema 1
path.data <- "./series temporales/"
path.data <- path.data.0
nm <- names(read.csv2(paste0(path.data,"INE_IPC.csv"), skip=6, header=TRUE, nrows=1))

yEnd <- min(grep(".3",nm,fixed = TRUE))-1
vEnd <- yEnd + 195
end <- min(grep(".1",nm,fixed = TRUE))-1
nm <- nm[end:2]
numYear <- as.numeric(substr(nm,start=2,stop=5))
numMonth <- as.numeric(substr(nm,start=7,stop=8))
IPC <- as.numeric(unlist(t(read.csv2(paste0(path.data,"INE_IPC.csv"), skip=6, sep = ";")[1:13,end:2])))
inflation <- 100*diff(IPC,lag=12)/lag(IPC,k=-12)
IPC.d1.d12 <- diff( diff(IPC,lag=12), lag=1)


#Problema 2
#IPC
IPC.ts <- ts(IPC, frequency = 12, start=c(numYear[1],numMonth[1]), end=c(2018,3))
IPC.dec <- decompose(IPC.ts)
plot(IPC.dec)
IPC.ts.dec <- stl(IPC.dec$seasonal, s.window = "periodic")
plot(IPC.ts.dec)
barplot(IPC.ts.dec$time.series[1:12,1], main="Seasonal component()")


#Monthly variation
path.data <- path.data.0
nm <- names(read.csv2(paste0(path.data,"INE_IPC.csv"), skip=6, header=TRUE, nrows=1))
mEnd <- min(grep(".2",nm,fixed = TRUE))-1
nm2 = nm[mEnd:end]
monthly <- as.numeric(unlist(t(read.csv2(paste0(path.data,"INE_IPC.csv"), skip=6, sep = ";")[1:13,mEnd:end])))
numYear <- as.numeric(substr(nm2,start=2,stop=5))
numMonth <- as.numeric(substr(nm2,start=7,stop=8))

monthly.ts <- ts(monthly, frequency = 12, start=c(numYear[1],numMonth[1]), end=c(2018,3))
monthly.dec <- decompose(monthly.ts)
plot(monthly.dec)
monthly.d1.d12 <- stl(monthly.dec$seasonal, s.window = "periodic")
plot(monthly.d1.d12)
barplot(monthly.d1.d12$time.series[1:12,1], main="Seasonal component()")


#Inflation
inflation.ts <- ts(inflation, frequency = 12, start=c(numYear[1],numMonth[1]), end=c(2018,3)) 
inflation.dec <- decompose(inflation.ts)
plot(inflation.dec)
inflation.d1.d12 <- stl(inflation.dec$seasonal, s.window = "periodic")
plot(inflation.d1.d12)
barplot(inflation.d1.d12$time.series[1:12,1], main="Seasonal component()")


#IPC Series
IPC.d1.d12.ts <- ts(IPC.d1.d12, frequency = 12, start=c(numYear[1],numMonth[1]), end=c(2018,3)) 
IPC2.d1 <- decompose(IPC.d1.d12.ts)
plot(IPC2.d1)
IPC2.d1.d12 <- stl(IPC2.d1$seasonal, s.window = "periodic")
plot(IPC2.d1.d12)
barplot(IPC2.d1.d12$time.series[1:12,1], main="Seasonal component()")


#Problema 3
IPC.tr <- window(IPC.ts,start=c(2002,1), end=c(2016,12))
IPC.tr
IPC.te = window(IPC.ts,start=c(2017,1), end=c(2018,3))
IPC.te
length(IPC.te)
length(IPC.tr)


#Problema 4
IPC.tr.d1.d12 <- diff( diff(IPC.tr,lag=12), lag=1)

#a
acf(IPC.tr.d1.d12)
pacf(IPC.tr.d1.d12)

#b
#No es white noise, ya que hay varios valores suficietemente grandes como para 
#considerar que no es aleatorio y por tanto que no sera ruido blanco




#Problema 5
library(astsa)
end.train <- c(2018,3)
time.IPC <- time(IPC.tr.d1.d12)
IPC.tr.d1.d12.df <- as.data.frame( ts.union(time.IPC,IPC.tr.d1.d12) )
span <- .1
loess.IPC <- loess(IPC.tr.d1.d12 ~ time.IPC, span=span,
                   data=IPC.tr.d1.d12.df,
                   subset=(time.IPC<=sum(end.train*c(1,1/12))-1/12),
                   control = loess.control(surface = "direct"))

HW.IPC.tr.d1.d12 <- HoltWinters(window(IPC.tr.d1.d12,end=end.train),beta=FALSE, gamma=FALSE)
frcst.WP.IPC.tr.d1.d12 <- predict(HW.IPC.tr.d1.d12,n.ahead=15, prediction.interval = TRUE)
plot(IPC.tr.d1.d12,col=8,main="Exponential smoothing forecasting, with no seasonal component")
lines(HW.IPC.tr.d1.d12$fitted[,1],lwd=2)
abline(v=max(loess.IPC$x),lty=2,col=6)
lines(frcst.WP.IPC.tr.d1.d12[,1],col=2,lwd=2)
lines(frcst.WP.IPC.tr.d1.d12[,2],col=4,lty=2,lwd=2)
lines(frcst.WP.IPC.tr.d1.d12[,3],col=4,lty=2,lwd=2)


#Problema 6
library(astsa)
end.train <- c(2018,3)
time.IPC <- time(IPC.tr)
IPC.tr.df <- as.data.frame( ts.union(time.IPC,IPC.tr) )
span <- .1
loess.IPC <- loess(IPC.tr ~ time.IPC, span=span,
                   data=IPC.tr.df,
                   subset=(time.IPC<=sum(end.train*c(1,1/12))-1/12),
                   control = loess.control(surface = "direct"))

HW.IPC.tr <- HoltWinters(window(IPC.tr,end=end.train),beta=FALSE, gamma=FALSE)
frcst.WP.IPC.tr <- predict(HW.IPC.tr,n.ahead=15, prediction.interval = TRUE)
plot(IPC.tr,col=8,main="Exponential smoothing forecasting, with no seasonal component")
lines(HW.IPC.tr$fitted[,1],lwd=2)
abline(v=max(loess.IPC$x),lty=2,col=6)
lines(frcst.WP.IPC.tr[,1],col=2,lwd=2)
lines(frcst.WP.IPC.tr[,2],col=4,lty=2,lwd=2)
lines(frcst.WP.IPC.tr[,3],col=4,lty=2,lwd=2)