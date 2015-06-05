setwd("C:/Users/Vanessa Lepe/Documents/Graduate Stuff/Classes/Spring 2015/ST599/Project 3")
companies = list.files(pattern="*.csv")
#install.packages("data.table")
#install.packages("bit64")

library(data.table)
library(bit64)
stock_data <- do.call(rbind, lapply(companies, fread))
stock_data <- as.data.frame(stock_data)
head(stock_data)
install.packages("stringi")
library('stringi')
temp <- companies
temp <- stri_sub(temp, 7, -5)
companies_names <- temp
rm(temp)
stock_data$V2 <- NULL


#####Adding Company Column########
lengthc<-lapply(companies,fread)
CompaniesL=rep(0,500)
CompaniesL[1]=dim(as.data.frame(lengthc[1]))[1]
Companies=rep(companies_names[1],CompaniesL)

for (i in 2:500){
  CompaniesL[i]=dim(as.data.frame(lengthc[i]))[1]
  #Companies=c(Companies,rep(companies_names[i],CompaniesL[i]))
}
stock_data$Company=Companies
#########################################

## MEMORIAL DAY - CODE CONTINUED
library(lubridate)
stock_data$V1 <- ymd(as.character(stock_data$V1))
colnames(stock_data)[1] <- "Date"
colnames(stock_data)[2] <- "Open"
colnames(stock_data)[3] <- "High"
colnames(stock_data)[4] <- "Low"
colnames(stock_data)[5] <- "Close"
colnames(stock_data)[6] <- "Volume"
stock_data$weekday <- wday(stock_data$Date)
stock_data$year_day <- yday(stock_data$Date)
stock_data$julian_day <- julian((stock_data$Date))
stock_data$month <- month((stock_data$Date))
stock_data$month_day <- mday((stock_data$Date))
head(stock_data)
plot(stock_data2[,1],stock_data2[,6],type="l")
wday(Sys.time())
test <- subset(stock_data, stock_data$julian_day >= (max(stock_data$julian_day) - 365))
train <- subset(stock_data, stock_data$julian_day < (max(stock_data$julian_day) - 365))
test2 <- as.matrix(test)
train2 <- as.matrix(train)
library(plyr)
impute.median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
stock_data2 <- ddply(dat, ~ taxa, transform, length = impute.mean(length),
              width = impute.mean(width))

stock_data2[order(dat2$id), ]

## 5/27/15 - Continued

set.seed()
install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)

subtrainfeature <- as.matrix(train[c("Open", "High", "Low", "Close", "weekday", "year_day", "julian_day", "month", "month_day")])
validfeature <- as.matrix(test[c("Open", "High", "Low", "Close", "weekday", "year_day", "julian_day", "month", "month_day")])
subtrainresp <- as.matrix(train["Volume"])
validresp <- as.matrix(test["Volume"])

#######################################################################
Com=as.data.frame(companies_names)
Com$Len=CompaniesL
Com=Com[order(Com$Len),]

library(lubridate)
# generally a good idea in my book - stops character strings being converted to factors automatically
options(stringsAsFactors = FALSE)

stock_data<-read.csv("stock_data.csv",header=T)
stock_data$Company=as.factor(stock_data$Company)
stock_data=stock_data[,-1]
stock_data$Date=ymd(stock_data$Date)
stock_data$year=year(stock_data$Date)

stock_data$Monday=as.Date(stock_data$Date)-(stock_data$weekday-2)

SD=stock_data[,c(2:7,14)]

############################Code to Run####################################
library(plyr)
SDavgO=ddply(SD, c("Company","Monday"), summarize,Wopen = mean(Open))
SDavgH=ddply(SD, c("Company","Monday"), summarize,Whigh = mean(High))
SDavgL=ddply(SD, c("Company","Monday"), summarize,Wlow = mean(Low))
SDavgC=ddply(SD, c("Company","Monday"), summarize,Wclose = mean(Close))
SDavgV=ddply(SD, c("Company","Monday"), summarize,Wvolume = mean(Volume))
SDavgALL <- cbind(SDavgO, SDavgH$Whigh, SDavgL$Wlow, SDavgC$Wclose, SDavgV$Wvolume)
colnames(SDavgALL)[4] <- "Whigh"
colnames(SDavgALL)[5] <- "Wlow"
colnames(SDavgALL)[6] <- "Wclose"
colnames(SDavgALL)[7] <- "Wvolume"
rm(SDavgO, SDavgH, SDavgL, SDavgC, SDavgV, SDavgT, SDT)
###########################################################################

SDT=SD[SD$Company=="a"|SD$Company=="aa",]
SDavgT=ddply(SDT, c("Company","Monday"), summarize,Wclose = mean(Close))


#####Getting rid of company with less than 100 weeks of info
avgC=table(SDavgALL$Company)
SDavg=SDavgALL
len=rep(avgC[1],avgC[1])
for (i in 2:500){len=c(len,rep(avgC[i],avgC[i]))}
SDavg$len=len
SDavg=SDavg[SDavg$len>=100,]


###########Getting 10 explanatory variables###############
SDavg$Company=as.character(SDavg$Company)
SDavg$Company=as.factor(SDavg$Company)
newLen=as.numeric(table(SDavg$Company))


Fcount=0+1
count=newLen[1]

SDnew=as.data.frame(cbind(SDavg[(Fcount+10):count,6],SDavg[(Fcount+9):(count-1),6],SDavg[(Fcount+8):(count-2),6],
          SDavg[(Fcount+7):(count-3),6],SDavg[(Fcount+6):(count-4),6],SDavg[(Fcount+5):(count-5),6],
          SDavg[(Fcount+4):(count-6),6],SDavg[(Fcount+3):(count-7),6],SDavg[(Fcount+2):(count-8),6],
          SDavg[(Fcount+1):(count-9),6],SDavg[(Fcount):(count-10),6]))
SDnew$Date=SDavg[(Fcount+10):count,2]
SDnew$Company=SDavg[(Fcount+10):count,1]


for (i in 2:491){
  Fcount=count+1
  count=count+newLen[i]
  
  temp=as.data.frame(cbind(SDavg[(Fcount+10):count,6],SDavg[(Fcount+9):(count-1),6],SDavg[(Fcount+8):(count-2),6],
             SDavg[(Fcount+7):(count-3),6],SDavg[(Fcount+6):(count-4),6],SDavg[(Fcount+5):(count-5),6],
             SDavg[(Fcount+4):(count-6),6],SDavg[(Fcount+3):(count-7),6],SDavg[(Fcount+2):(count-8),6],
             SDavg[(Fcount+1):(count-9),6],SDavg[(Fcount):(count-10),6]))
  temp$Date=SDavg[(Fcount+10):count,2]
  temp$Company=SDavg[(Fcount+10):count,1]
  
  SDnew=rbind(SDnew,temp)
}


####Getting test and predict data########
newLen2=as.numeric(table(SDnew$Company))
newLen2<-newLen2[1:483]
Fcount=0+1
count=newLen2[1]
SDP=SDnew[(count-7):count,] #Data to predict
SDT=SDnew[Fcount:(count-8),] #Data to test

for (i in 2:491){
  Fcount=count+1
  count=count+newLen2[i]
  SDP=rbind(SDP,SDnew[(count-7):count,])
  SDT=rbind(SDT,SDnew[Fcount:(count-8),])
}

##################### Ridge Regression ######################################
library(MASS)
train_ridge <- lm.ridge(V1 ~ V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11, SDT, Company == "a", y = TRUE)
train_ridge$coef
select(train_ridge)
test_a <- subset(SDP, Company == "a")
predict(test_a$V1, train_ridge)

pred.ridge <- (coef(train_ridge)[1] + coef(train_ridge)[2]*test_a[,2]+ coef(train_ridge)[3]*test_a[,3] 
+ coef(train_ridge)[4]*test_a[,4] + coef(train_ridge)[5]*test_a[,5] + coef(train_ridge)[6]*test_a[,6]
+ coef(train_ridge)[7]*test_a[,7] + coef(train_ridge)[8]*test_a[,8] + coef(train_ridge)[9]*test_a[,9]
+ coef(train_ridge)[10]*test_a[,10] + coef(train_ridge)[11]*test_a[,11])

te <- cbind(test_a$V1, pred.ridge)
plot(te[,1], te[,2])
MSPE.ridge <- sum((pred.ridge - mean(te[,1]))^2)/8

largecomp <- as.character(unique(SDT$Company))
largecomp[1]

trainfull_ridge <- NULL
predfull <- NULL
MSPEfull <- NULL
test_comps <- NULL

#### There is no predict function for ridge, so had to do manually
for (i in 1:491){
  test_comps[[i]] <- subset(SDP, Company == largecomp[i])
}

for (i in 1:491){
  trainfull_ridge[[i]] <- lm.ridge(V1 ~ V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11, SDT, Company == largecomp[i], y = TRUE)
}

for (i in 1:491){
  predfull[[i]] <- (coef(trainfull_ridge[[i]])[1] + coef(trainfull_ridge[[i]])[2]*test_comps[[i]][,2]+ coef(trainfull_ridge[[i]])[3]*test_comps[[i]][,3] 
                    + coef(trainfull_ridge[[i]])[4]*test_comps[[i]][,4] + coef(trainfull_ridge[[i]])[5]*test_comps[[i]][,5] + coef(trainfull_ridge[[i]])[6]*test_comps[[i]][,6]
                    + coef(trainfull_ridge[[i]])[7]*test_comps[[i]][,7] + coef(trainfull_ridge[[i]])[8]*test_comps[[i]][,8] + coef(trainfull_ridge[[i]])[9]*test_comps[[i]][,9]
                    + coef(trainfull_ridge[[i]])[10]*test_comps[[i]][,10] + coef(trainfull_ridge[[i]])[11]*test_comps[[i]][,11])
}

for (i in 1:491){
  MSPEfull[[i]] <- sum((predfull[[i]] - mean(test_comps[[i]][,1]))^2)/8
}
range(MSPEfull)
plot(MSPEfull, xlab="Companies", ylab="MSPE", main="Prediction Error Before Differencing")

############## Random Walk Adjustment ##########################
adf.test(SDT[,1], alternative = "stationary")


predP <- as.matrix(SDP[,1:11])
compsP <- as.character(SDP$Company)
dateP <- as.Date(SDP$Date)
predtest <- predP[1:3927,]
newLen11 <- c(1:491)
newLen11[1:491] <- 8
newLen7 <- newLen11[-491]
length(newLen5)
newLen8 <- newLen7
for (i in 1:3927){
  predtest[i,] <- predP[i+1,]-predP[i,]
}
for (i in 2:490){
  newLen8[i] <- sum(newLen7[1:i])
}
predtest2 <- predtest[-newLen8,]
compsP2 <- compsP[-newLen8]
compsP2 <- compsP2[-3438]
dateP2 <- dateP[-newLen8]
dateP2 <- dateP2[-3438]
SDPrw <- data.frame(predtest2, dateP2, compsP2)
colnames(SDPrw)[12] <- "Date"
colnames(SDPrw)[13] <- "Company"


predT <- as.matrix(SDT[,1:11])
compsT <- as.character(SDT$Company)
dateT <- as.Date(SDT$Date)
predtrain <- predT[1:366004,]
newLen4<-newLen[newLen>0]-10
newLen9 <- newLen4[-491]
length(newLen9)
newLen10 <- newLen9
for (i in 1:366004){
  predtrain[i,] <- predT[i+1,]-predT[i,]
}
for (i in 2:490){
  newLen10[i] <- sum(newLen9[1:i])
}
predtrain2 <- predtrain[-newLen10,]
compsT2 <- compsT[-newLen10]
compsT2 <- compsT2[-365520]
dateT2 <- dateT[-newLen10]
dateT2 <- dateT2[-365520]
SDTrw <- data.frame(predtrain2, dateT2, compsT2)
colnames(SDTrw)[12] <- "Date"
colnames(SDTrw)[13] <- "Company"

library(MASS)

trainfull_ridge2 <- NULL
predfull2 <- NULL
MSPEfull2 <- NULL
test_comps2 <- NULL

#### There is no predict function for ridge, so had to do manually
for (i in 1:491){
  test_comps2[[i]] <- subset(SDPrw, Company == largecomp[i])
}

for (i in 1:491){
  trainfull_ridge2[[i]] <- lm.ridge(V1 ~ V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11, SDTrw, Company == largecomp[i], y = TRUE)
}

for (i in 1:491){
  predfull2[[i]] <- (coef(trainfull_ridge2[[i]])[1] + coef(trainfull_ridge2[[i]])[2]*test_comps2[[i]][,2]+ coef(trainfull_ridge2[[i]])[3]*test_comps2[[i]][,3] 
                    + coef(trainfull_ridge2[[i]])[4]*test_comps2[[i]][,4] + coef(trainfull_ridge2[[i]])[5]*test_comps2[[i]][,5] + coef(trainfull_ridge2[[i]])[6]*test_comps2[[i]][,6]
                    + coef(trainfull_ridge2[[i]])[7]*test_comps2[[i]][,7] + coef(trainfull_ridge2[[i]])[8]*test_comps2[[i]][,8] + coef(trainfull_ridge2[[i]])[9]*test_comps2[[i]][,9]
                    + coef(trainfull_ridge2[[i]])[10]*test_comps2[[i]][,10] + coef(trainfull_ridge2[[i]])[11]*test_comps2[[i]][,11])
}

for (i in 1:491){
  MSPEfull2[[i]] <- sum((predfull2[[i]] - mean(test_comps2[[i]][,1]))^2)/8
}
range(MSPEfull2)
plot(MSPEfull2, xlab="Companies", ylab="MSPE", main="Prediction Error After Differencing")
bestpreds <- data.frame(MSPEfull2, largecomp)
bestpreds <- bestpreds[order(MSPEfull2),]
head(bestpreds)
tail(bestpreds)
plot(SDTrw$Date,SDTrw$V1)


library(lubridate)

maxL=max(lengthc[[1]]$V6)
for (i in 2:500){
  maxL=c(maxL,max(lengthc[[i]]$V6))
}

plot(ymd(lengthc[[1]]$V1),lengthc[[1]]$V6, type="l",ylim=c(0,max(maxL)), xlab="Date", ylab="Daily Closing Stock Price",
     main="Daily Closing Stock Price From 1999-2013")
for (i in 2:500){
  lines(ymd(lengthc[[i]]$V1),lengthc[[i]]$V6)
}