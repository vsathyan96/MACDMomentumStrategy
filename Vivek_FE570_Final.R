#Vivek Sathyanarayana
#FE 570 Spring 2019
#Final Exam

#Problem 8-- (1)

library('TTR')

#Read file from text file
data <- read.csv("sp500hst.txt", head=T);

#To enlarge the numbers to print.
options(max.print=40000)

#Declare constants
nf = 12
ns = 26
m1 = 9

#Extract BAC
dataBAC <- data[data[,2]=="BAC",]

r<-vector(mode="numeric",length=(nrow(dataBAC)-1))
for(i in 1:(nrow(dataBAC)-1))
{
  r[i] <- dataBAC$Close[i]
}

MACDsig <- MACD(r,nFast = nf,nSlow = ns, nSig = m1)
MACDsig[is.na(MACDsig)] <- 0

#Counter to assess when MACD line is less than signal line to identify crossover
j=0
count <- vector(mode = "numeric")
for (i in 1:length(MACDsig[,1])) {
  j=j+1
  if(MACDsig[i,1]<MACDsig[i,2])
    count[j]= 1
  else
    count[j]=0
}

buyMACD <- vector(mode="numeric")
sellMACD <- vector(mode="numeric")
#Loop starts from 34 as the first 33 values are 0 for the signal line 
#and hence the program shouldn't accidentally recognize 
#the first spike as a buy/sell signal

for (a in (34:((length(count)-1)))) {
  if((count[a+1]==0)&(count[a]==1)) {
    buyMACD <-cbind(buyMACD,a)
  }
  else if((count[a+1]==1)&(count[a]==0)) {
    sellMACD <-cbind(sellMACD,a)
  }
}

#MACD Returns Analysis
P = 10000 #initial amount
tcost = 5 #transaction cost

buyMACD <- cbind(buyMACD,length(r))
weightl <- vector(mode = "numeric")
weightsh <- vector(mode = "numeric")
weightl <- floor(P/r[buyMACD])
weightsh <- floor(P/r[sellMACD])

longAmt <- r[buyMACD]*weightl
shortAmt <- r[sellMACD]*weightsh
profitsh <- vector(mode="numeric")
profitl <- vector(mode="numeric")

#Long position profit
for (i in 1:length(shortAmt)-1) {
  profitl[i] <- ((weightl[i]*r[sellMACD[i+1]])-longAmt[i]) -(tcost) #Two transactions --open and close
}

#Short position profit
for (i in 1:length(shortAmt)) {
  profitsh[i] <- (-(weightsh[i]*r[buyMACD[i]])+shortAmt[i])-(tcost)
}

#Comput P&L as percentage
ReturnPerc <- vector(mode = "numeric")

for (k in 1:length(sellMACD)) {
  ReturnPerc[2*k-1] <- (profitsh[k]/shortAmt[k])*100
}

for (k in 1:(length(buyMACD)-1)) {
  ReturnPerc[2*k] <- (profitl[k]/longAmt[k])*100
}

#Create vectors with price at entry and exit
longP <- r[buyMACD]
shortP <- r[sellMACD]

#ROI Table for MACD
ROIMACD1 <- data.frame()
for (i in 1:length(sellMACD)) {
  ROIMACD1[(2*i-1),1] <- sellMACD[i]
  ROIMACD1[(2*i-1),2] <- buyMACD[i]
  ROIMACD1[(2*i-1),3] <- "SHORT"
  ROIMACD1[(2*i-1),4] <- shortP[i]
  ROIMACD1[(2*i-1),5] <- longP[i]
  ROIMACD1[(2*i-1),6] <- weightsh[i]
  ROIMACD1[(2*i-1),7] <- profitsh[i]
  ROIMACD1[(2*i-1),8] <- ReturnPerc[(2*i)-1]
}
for (i in 1:length(buyMACD)-1) {
  ROIMACD1[(2*i),1] <- buyMACD[i]
  ROIMACD1[(2*i),2] <- sellMACD[i+1]
  ROIMACD1[(2*i),3] <- "LONG"
  ROIMACD1[(2*i),4] <- longP[i]
  ROIMACD1[(2*i),5] <- shortP[i+1]
  ROIMACD1[(2*i),6] <- weightl[i]
  ROIMACD1[(2*i),7] <- profitl[i]
  ROIMACD1[(2*i),8] <- ReturnPerc[(2*i)]
}

colnames(ROIMACD1) <- c("Start","End","Position","Entry Price ($)",
                       "Exit Price ($)","No. of Shares","P/L (Amount)","P/L (%)")

#Create vector with trading times for plot
tvec <- vector(mode="numeric")
tvec <- cbind(tvec,buyMACD)
tvec <- cbind(tvec,sellMACD)
tradevec <- vector(mode = "numeric")
tradevec <- MACDsig[tvec,1]



#Problem 8-- (2)
#Trading period  is over 2 years to rf is taken to be as the mean of the two rates
rf = ((26.46+15.06)/2)/100

#Calculate Sharpe Ratio
Sharpe9 <- (sum((ReturnPerc/100-rf))/length(ReturnPerc/100))/sqrt(var((ReturnPerc/100)-rf))
#Sharpe ratio is low because Risk-free rate is extremely high

#Problem 8-- (3)
m2 = 7
m3 = 11
MACDsig2 <- MACD(r,nFast = nf,nSlow = ns, nSig = m2)
MACDsig2[is.na(MACDsig2)] <- 0

#Counter to assess when MACD line is less than signal line to identify crossover
j=0
count2 <- vector(mode = "numeric")
for (i in 1:length(MACDsig2[,1])) {
  j=j+1
  if(MACDsig2[i,1]<MACDsig2[i,2])
    count2[j]= 1
  else
    count2[j]=0
}

buyMACD2 <- vector(mode="numeric")
sellMACD2 <- vector(mode="numeric")
#Loop starts from 34 as the first 33 values are 0 for the signal line 
#and hence the program shouldn't accidentally recognize 
#the first spike as a buy/sell signal

for (a in (34:((length(count2)-1)))) {
  if((count2[a+1]==0)&(count2[a]==1)) {
    buyMACD2 <-cbind(buyMACD2,a)
  }
  else if((count2[a+1]==1)&(count2[a]==0)) {
    sellMACD2 <-cbind(sellMACD2,a)
  }
}

#MACD Returns Analysis
buyMACD2 <- cbind(buyMACD2,length(r))
weightl2 <- vector(mode = "numeric")
weightsh2 <- vector(mode = "numeric")
weightl2 <- floor(P/r[buyMACD2])
weightsh2 <- floor(P/r[sellMACD2])

longAmt2 <- r[buyMACD2]*weightl2
shortAmt2 <- r[sellMACD2]*weightsh2
profitsh2 <- vector(mode="numeric")
profitl2 <- vector(mode="numeric")

#Long position profit
for (i in 1:length(shortAmt2)-1) {
  profitl2[i] <- ((weightl2[i]*r[sellMACD2[i+1]])-longAmt2[i]) -(tcost) #Two transactions --open and close
}

#Short position profit
for (i in 1:length(shortAmt2)) {
  profitsh2[i] <- (-(weightsh2[i]*r[buyMACD2[i]])+shortAmt2[i])-(tcost)
}

#Comput P&L as percentage
ReturnPerc2 <- vector(mode = "numeric")

for (k in 1:length(sellMACD2)) {
  ReturnPerc2[2*k-1] <- (profitsh2[k]/shortAmt2[k])*100
}

for (k in 1:(length(buyMACD2)-1)) {
  ReturnPerc2[2*k] <- (profitl2[k]/longAmt2[k])*100
}

#Create vectors with price at entry and exit
longP2 <- r[buyMACD2]
shortP2 <- r[sellMACD2]

#ROI Table for MACD
ROIMACD2 <- data.frame()
for (i in 1:length(sellMACD2)) {
  ROIMACD2[(2*i-1),1] <- sellMACD2[i]
  ROIMACD2[(2*i-1),2] <- buyMACD2[i]
  ROIMACD2[(2*i-1),3] <- "SHORT"
  ROIMACD2[(2*i-1),4] <- shortP2[i]
  ROIMACD2[(2*i-1),5] <- longP2[i]
  ROIMACD2[(2*i-1),6] <- weightsh2[i]
  ROIMACD2[(2*i-1),7] <- profitsh2[i]
  ROIMACD2[(2*i-1),8] <- ReturnPerc2[(2*i)-1]
}
for (i in 1:length(buyMACD2)-1) {
  ROIMACD2[(2*i),1] <- buyMACD2[i]
  ROIMACD2[(2*i),2] <- sellMACD2[i+1]
  ROIMACD2[(2*i),3] <- "LONG"
  ROIMACD2[(2*i),4] <- longP2[i]
  ROIMACD2[(2*i),5] <- shortP2[i+1]
  ROIMACD2[(2*i),6] <- weightl2[i]
  ROIMACD2[(2*i),7] <- profitl2[i]
  ROIMACD2[(2*i),8] <- ReturnPerc2[(2*i)]
}

colnames(ROIMACD2) <- c("Start","End","Position","Entry Price ($)",
                       "Exit Price ($)","No. of Shares","P/L (Amount)","P/L (%)")

#Create vector with trading times for plot
tvec2 <- vector(mode="numeric")
tvec2 <- cbind(tvec2,buyMACD2)
tvec2 <- cbind(tvec2,sellMACD2)
tradevec2 <- vector(mode = "numeric")
tradevec2 <- MACDsig2[tvec2,1]

#Calculate Sharpe Ratio for m = 7
Sharpe7 <- (sum((ReturnPerc2/100-rf))/length(ReturnPerc2/100))/sqrt(var((ReturnPerc2/100)-rf))

#Recompute strategy for m = 11
MACDsig3 <- MACD(r,nFast = nf,nSlow = ns, nSig = m3)
MACDsig3[is.na(MACDsig3)] <- 0

#Counter to assess when MACD line is less than signal line to identify crossover
j=0
count3 <- vector(mode = "numeric")
for (i in 1:length(MACDsig3[,1])) {
  j=j+1
  if(MACDsig3[i,1]<MACDsig3[i,2])
    count3[j]= 1
  else
    count3[j]=0
}

buyMACD3 <- vector(mode="numeric")
sellMACD3 <- vector(mode="numeric")
#Loop starts from 34 as the first 33 values are 0 for the signal line 
#and hence the program shouldn't accidentally recognize 
#the first spike as a buy/sell signal

for (a in (34:((length(count3)-1)))) {
  if((count3[a+1]==0)&(count3[a]==1)) {
    buyMACD3 <-cbind(buyMACD3,a)
  }
  else if((count3[a+1]==1)&(count3[a]==0)) {
    sellMACD3 <-cbind(sellMACD3,a)
  }
}

#MACD Returns Analysis
buyMACD3 <- cbind(buyMACD3,length(r))
weightl3 <- vector(mode = "numeric")
weightsh3 <- vector(mode = "numeric")
weightl3 <- floor(P/r[buyMACD3])
weightsh3 <- floor(P/r[sellMACD3])

longAmt3 <- r[buyMACD3]*weightl3
shortAmt3 <- r[sellMACD3]*weightsh3
profitsh3 <- vector(mode="numeric")
profitl3 <- vector(mode="numeric")

#Long position profit
for (i in 1:length(longAmt3)) {
  profitl3[i] <- ((weightl3[i]*r[sellMACD3[i]])-longAmt3[i]) -(tcost) #Two transactions --open and close
}

#Short position profit
for (i in 1:length(shortAmt3)) {
  profitsh3[i] <- (-(weightsh3[i]*r[buyMACD3[i+1]])+shortAmt3[i])-(tcost)
}

#Comput P&L as percentage
ReturnPerc3 <- vector(mode = "numeric")

for (k in 1:length(sellMACD3)) {
  ReturnPerc3[2*k] <- (profitsh3[k]/shortAmt3[k])*100
}

for (k in 1:(length(buyMACD3)-1)) {
  ReturnPerc3[2*k-1] <- (profitl3[k]/longAmt3[k])*100
}

#Create vectors with price at entry and exit
longP3 <- r[buyMACD3]
shortP3 <- r[sellMACD3]

#ROI Table for MACD
ROIMACD3 <- data.frame()
for (i in 1:length(sellMACD3)) {
  ROIMACD3[(2*i),1] <- sellMACD3[i]
  ROIMACD3[(2*i),2] <- buyMACD3[i+1]
  ROIMACD3[(2*i),3] <- "SHORT"
  ROIMACD3[(2*i),4] <- shortP3[i]
  ROIMACD3[(2*i),5] <- longP3[i+1]
  ROIMACD3[(2*i),6] <- weightsh3[i]
  ROIMACD3[(2*i),7] <- profitsh3[i]
  ROIMACD3[(2*i),8] <- ReturnPerc3[(2*i)]
}
for (i in 1:(length(buyMACD3)-1)) {
  ROIMACD3[(2*i-1),1] <- buyMACD3[i]
  ROIMACD3[(2*i-1),2] <- sellMACD3[i]
  ROIMACD3[(2*i-1),3] <- "LONG"
  ROIMACD3[(2*i-1),4] <- longP3[i]
  ROIMACD3[(2*i-1),5] <- shortP3[i]
  ROIMACD3[(2*i-1),6] <- weightl3[i]
  ROIMACD3[(2*i-1),7] <- profitl3[i]
  ROIMACD3[(2*i-1),8] <- ReturnPerc3[(2*i-1)]
}

colnames(ROIMACD3) <- c("Start","End","Position","Entry Price ($)",
                       "Exit Price ($)","No. of Shares","P/L (Amount)","P/L (%)")

#Create vector with trading times for plot
tvec3 <- vector(mode="numeric")
tvec3 <- cbind(tvec3,buyMACD3)
tvec3 <- cbind(tvec3,sellMACD3)
tradevec3 <- vector(mode = "numeric")
tradevec3 <- MACDsig3[tvec3,1]



#Calculate Sharpe Ratio for m = 11
Sharpe11 <- (sum((ReturnPerc3/100-rf))/length(ReturnPerc3/100))/sqrt(var((ReturnPerc3/100)-rf))

Sharpe <- data.frame(c(Sharpe7,Sharpe9,Sharpe11))
colnames(Sharpe) <- "Sharpe Ratio"
rownames(Sharpe) <- c("m = 7","m = 9","m = 11")

#Plots
plot(1:length(MACDsig[,1]),MACDsig[,1],type="l", main="MACD for m = 9", xlab="Time", 
     ylab="Crossover Analysis", col="blue")
lines(1:length(MACDsig[,2]), MACDsig[,2],col="green")
lines(tvec,tradevec,type = "p")

plot(1:length(MACDsig2[,1]),MACDsig2[,1],type="l", main="MACD for m = 7", xlab="Time", 
     ylab="Crossover Analysis", col="blue")
lines(1:length(MACDsig[,2]), MACDsig[,2],col="green")
lines(tvec,tradevec,type = "p")

plot(1:length(MACDsig3[,1]),MACDsig3[,1],type="l", main="MACD for m = 11", xlab="Time", 
     ylab="Crossover Analysis", col="blue")
lines(1:length(MACDsig3[,2]), MACDsig3[,2],col="green")
lines(tvec3,tradevec3,type = "p")

#Data Tables
#m=9
ROIMACD1

#m=7
ROIMACD2

#m=11
ROIMACD3

#Output Sharpe
Sharpe

#As you can see from the table, m = 7 is the best strategy 