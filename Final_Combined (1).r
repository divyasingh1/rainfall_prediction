data<-read.csv("rainfall.csv",stringsAsFactors = FALSE)
library(dplyr)
library(tseries)
library(forecast)
library(reshape)
head(data)
data<-filter(data,SD_Name=="WEST UTTAR PRADESH")

####   JAN-APR

testjan<-filter(data,YEAR<=2013)
trainjan<-filter(data,YEAR<=2012)
trainjan<-select(trainjan,YEAR,JAN:APR)
testjan<-select(testjan,YEAR,JAN:APR)
trainjan<-melt(trainjan,c("YEAR"))
testjan<-melt(testjan,c("YEAR"))
head(trainjan)
tail(testjan)
trainjan<-arrange(trainjan,YEAR)
testjan<-arrange(testjan,YEAR)
head(trainjan,12)
trainjan<-ts(trainjan$value,frequency=4)
testjan<-ts(testjan$value,frequency=4)
class(trainjan)
class(testjan)
plot(decompose(trainjan))
jantrend<-decompose(trainjan)$trend
jantrendtest<-decompose(testjan)$trend
janseasonal<-decompose(trainjan)$seasonal
janrandom<-decompose(trainjan)$random
jantrend<-na.omit(jantrend)
jantrendtest<-na.omit(jantrendtest)
janrandom<-na.omit(janrandom)
jantrend<-ts(jantrend,frequency=4)
jantrendtest<-ts(jantrendtest,frequency = 4)
janseasonal<-ts(janseasonal,frequency=4)
janrandom<-ts(janrandom,frequency = 4)
modjantrend<-Arima(jantrend,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
modjanrandom<-Arima(janrandom,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))

forjantrend<-forecast(modjantrend,h=4)
forjanrandom<-forecast(modjanrandom,h=4)
forjantrend<-as.data.frame(forjantrend)
janseas<-head(janseasonal,4)
janseas<-as.data.frame(janseas)
forjanrandom<-as.data.frame(forjanrandom)
janseas<-as.data.frame(janseas)

janfinal<-janseas$x+forjantrend$`Point Forecast`+forjanrandom$`Point Forecast`
janfinal
plot(janfinal)


testjan1<-filter(data,YEAR==2013)
testjan1<-filter(testjan1,SD_Name=="WEST UTTAR PRADESH")
testjan1<-select(testjan1,YEAR,JAN:APR)
testjan1<-melt(testjan1,c("YEAR"))
testjan1<-ts(testjan1$value,frequency=4)
janfinal<-ifelse(janfinal>0,janfinal,0)
plot(testjan1,col='red')
lines(janfinal)

####

####    MAY-OCT

testmay<-filter(data,YEAR<=2013)
trainmay<-filter(data,YEAR<=2012)
trainmay<-select(trainmay,YEAR,MAY:OCT)
testmay<-select(testmay,YEAR,MAY:OCT)
trainmay<-melt(trainmay,c("YEAR"))
testmay<-melt(testmay,c("YEAR"))
head(trainmay)
tail(testmay)
trainmay<-arrange(trainmay,YEAR)
testmay<-arrange(testmay,YEAR)
head(trainmay,12)
trainmay<-ts(trainmay$value,frequency=6)
testmay<-ts(testmay$value,frequency=6)
class(trainmay)
class(testmay)
plot(decompose(trainmay))
maytrend<-decompose(trainmay)$trend
maytrendtest<-decompose(testmay)$trend
mayseasonal<-decompose(trainmay)$seasonal
mayrandom<-decompose(trainmay)$random
maytrend<-na.omit(maytrend)
maytrendtest<-na.omit(maytrendtest)
mayrandom<-na.omit(mayrandom)
maytrend<-ts(maytrend,frequency=6)
maytrendtest<-ts(maytrendtest,frequency = 6)
mayseasonal<-ts(mayseasonal,frequency=6)
mayrandom<-ts(mayrandom,frequency = 6)
modmaytrend<-Arima(maytrend,order=c(2,1,2),seasonal=list(order=c(1,0,1),period=6))
modmayrandom<-Arima(mayrandom,order=c(2,0,2))

formaytrend<-forecast(modmaytrend,h=6)
formayrandom<-forecast(modmayrandom,h=6)
formaytrend<-as.data.frame(formaytrend)
mayseas<-head(mayseasonal,6)
mayseas<-as.data.frame(mayseas)
formayrandom<-as.data.frame(formayrandom)
mayseas<-as.data.frame(mayseas)

mayfinal<-mayseas$x+formaytrend$`Point Forecast`+formayrandom$`Point Forecast`
mayfinal
plot(mayfinal)


testmay1<-filter(data,YEAR==2013)
testmay1<-filter(testmay1,SD_Name=="WEST UTTAR PRADESH")
testmay1<-select(testmay1,YEAR,MAY:OCT)
testmay1<-melt(testmay1,c("YEAR"))
testmay1<-ts(testmay1$value,frequency=6)
mayfinal<-ifelse(mayfinal>0,mayfinal,0)
plot(testmay1,col='red')
lines(mayfinal)

####

####  NOV-DEC

testnov<-filter(data,YEAR<=2013)
trainnov<-filter(data,YEAR<=2012)
trainnov<-select(trainnov,YEAR,NOV:DEC)
testnov<-select(testnov,YEAR,NOV:DEC)
trainnov<-melt(trainnov,c("YEAR"))
testnov<-melt(testnov,c("YEAR"))
head(trainnov)
tail(testnov)
trainnov<-arrange(trainnov,YEAR)
testnov<-arrange(testnov,YEAR)
head(trainnov,12)
trainnov<-ts(trainnov$value,frequency=2)
testnov<-ts(testnov$value,frequency=2)
class(trainnov)
class(testnov)
plot(decompose(trainnov))
novtrend<-decompose(trainnov)$trend
novtrendtest<-decompose(testnov)$trend
novseasonal<-decompose(trainnov)$seasonal
novrandom<-decompose(trainnov)$random
novtrend<-na.omit(novtrend)
novtrendtest<-na.omit(novtrendtest)
novrandom<-na.omit(novrandom)
novtrend<-ts(novtrend,frequency=2)
novtrendtest<-ts(novtrendtest,frequency = 2)
novseasonal<-ts(novseasonal,frequency=2)
novrandom<-ts(novrandom,frequency = 2)
modnovtrend<-Arima(novtrend,order=c(1,0,0),seasonal=list(order=c(2,0,0),period=2))
modnovrandom<-Arima(novrandom,order=c(1,0,0),seasonal=list(order=c(2,0,0),period=2))

fornovtrend<-forecast(modnovtrend,h=2)
fornovrandom<-forecast(modnovrandom,h=2)
fornovtrend<-as.data.frame(fornovtrend)
novseas<-head(novseasonal,2)
novseas<-as.data.frame(novseas)
fornovrandom<-as.data.frame(fornovrandom)
novseas<-as.data.frame(novseas)

novfinal<-novseas$x+fornovtrend$`Point Forecast`+fornovrandom$`Point Forecast`
novfinal
plot(janfinal)


testnov1<-filter(data,YEAR==2013)
testnov1<-filter(testnov1,SD_Name=="WEST UTTAR PRADESH")
testnov1<-select(testnov1,YEAR,NOV:DEC)
testnov1<-melt(testnov1,c("YEAR"))
testnov1<-ts(testnov1$value,frequency=2)
novfinal<-ifelse(novfinal>0,novfinal,0)
plot(testnov1,col='red')
lines(novfinal)

####

janfinal<-as.data.frame(janfinal)
mayfinal<-as.data.frame(mayfinal)
novfinal<-as.data.frame(novfinal)

jan<-as.numeric(janfinal$x)
jan<-as.data.frame(jan)
jan<-rename(jan,c(jan="rain"))

may<-as.numeric(mayfinal$x)
may<-as.data.frame(may)
may<-rename(may,c(may="rain"))

nov<-as.numeric(novfinal$x)
nov<-as.data.frame(nov)
nov<-rename(nov,c(nov="rain"))

final<-rbind(jan,may,nov)
final<-ts(final$rain,frequency=12)
plot(final)

finaltest<-filter(data,YEAR==2013)
finaltest<-filter(finaltest,SD_Name=="WEST UTTAR PRADESH")
finaltest<-select(finaltest,YEAR,JAN:DEC)
finaltest<-melt(finaltest,c("YEAR"))
finaltest<-ts(finaltest$value,frequency=12)

plot(finaltest,col='red')
lines(final)
final<-as.data.frame(final)
finallabel<-select(data,JAN:DEC)
finallabel<-names(finallabel)
finallabel
final<-cbind(final,finallabel)
final
final<-rename(final,c(x="rain",finallabel="month"))

write.csv(final,"Prediction.csv")