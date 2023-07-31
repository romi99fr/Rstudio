#Problema 1
path.data <- "./series temporales/"
path.data <- path.data.0
nm <- names(read.csv2(paste0(path.data,"INE_IPC.csv"), skip=6, header=TRUE, nrows=1))

yEnd <- min(grep(".3",nm,fixed = TRUE))-1
vEnd <- yEnd + 195
end <- min(grep(".1",nm,fixed = TRUE))-1
nm <- nm[end:2]

IPC <- as.numeric(unlist(t(read.csv2(paste0(path.data,"INE_IPC.csv"), skip=6, sep = ";")[1:13,(end-1):2])))

numYear <- as.numeric(substr(nm,start=2,stop=5))
numMonth <- as.numeric(substr(nm,start=7,stop=8))

IPC.ts <- ts(IPC, frequency = 196, start=c(numYear[1],numMonth[1]))
IPC.ts
plot(IPC.ts,type="l", main="IPC", xlab="Time", ylab = "Valores")

#Monthly variation
path.data <- path.data.0
nm <- names(read.csv2(paste0(path.data,"INE_IPC.csv"), skip=6, header=TRUE, nrows=1))
mEnd <- min(grep(".2",nm,fixed = TRUE))-1
nm2 = nm[mEnd:end]
monthly <- as.numeric(unlist(t(read.csv2(paste0(path.data,"INE_IPC.csv"), skip=6, sep = ";")[1:13,mEnd:end+1])))

numYear <- as.numeric(substr(nm2,start=2,stop=5))
numMonth <- as.numeric(substr(nm2,start=7,stop=8))

monthly.ts <- ts(monthly, frequency = 196, start=c(numYear[1],numMonth[1]))
monthly.ts
plot(monthly.ts, main="Monthly Variation", xlab="Time", ylab = "Valores")
#Year Variation
path.data <- path.data.0
nm <- names(read.csv2(paste0(path.data,"INE_IPC.csv"), skip=6, header=TRUE, nrows=1))
yEnd <- min(grep(".3",nm,fixed = TRUE))-1
nm3 = nm[yEnd:mEnd]
yearly <- as.numeric(unlist(t(read.csv2(paste0(path.data,"INE_IPC.csv"), skip=6, sep = ";")[1:13,yEnd:mEnd+1])))

numYear <- as.numeric(substr(nm3,start=2,stop=5))
numMonth <- as.numeric(substr(nm3,start=7,stop=8))

yearly.ts <- ts(yearly, frequency = 196, start=c(numYear[1],numMonth[1]))
yearly.ts
plot(yearly.ts, main="Yearly Variation", xlab="Time", ylab = "Valores")

#Variation
path.data <- path.data.0
nm <- names(read.csv2(paste0(path.data,"INE_IPC.csv"), skip=6, header=TRUE))
nm4 = nm[vEnd:yEnd]
variation <- as.numeric(unlist(t(read.csv2(paste0(path.data,"INE_IPC.csv"), skip=6, sep = ";")[1:13,780:587])))
numYear <- as.numeric(substr(nm4,start=2,stop=5))
numMonth <- as.numeric(substr(nm4,start=7,stop=8))

variation.ts <- ts(variation, frequency = 196, start=c(numYear[1],numMonth[1]))
plot(variation.ts, main="IPC: Consumer price index,", xlab="Time", ylab = "Valores")

#Union
plot(ts.union(IPC.ts, monthly.ts, yearly.ts, variation.ts),type="l", main="Unio", xlab="Time", ylab = "IPC")


#Problema 3
plot(diff(log(IPC.ts),lag=12), main="Seasonal diff. of log(IPC)", ylab="")
unio <- cbind(IPC.ts, variation.ts, diff(log(IPC.ts)))
plot(unio,lag=12, main="Seasonal diff. of log(IPC)", ylab="")

#Problema 4
inflation <- 100*diff(IPC,lag=12)/lag(IPC,k=-12)
inflation


#Problema 5
acf(ts.union(IPC.ts, monthly.ts, yearly.ts, variation.ts),na.action = na.pass)
acf(inflation, na.action = na.pass)
acf(diff( diff(IPC,lag=1), lag=12))

