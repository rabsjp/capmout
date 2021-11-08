rm(list = ls())
#setwd("/cloud/project/data/abcone")
setwd("~/Desktop/jotarepos/capmout/a2c/")
files = list.files(pattern="*.csv")
d<-read.csv(files[1],sep=",",header=T, stringsAsFactors = FALSE)
d<-d[d$round_number>3,]
d$tiempo<-cumsum(d$timestamp)

d$dt[2:length(d$round_number)]<-d$round_number[2:length(d$round_number)]-d$round_number[1:length(d$round_number)-1]
d$dt[1]<-0

breakdance<-d$dt*d$tiempo
breakdance<-breakdance[breakdance>0]
plot(d$tiempo[d$asset=="A"],d$price[d$asset=="A"],type="p",ylim=c(0,500),col='red',xlab="time",ylab = "price")
points(d$tiempo[d$asset=="D"],d$price[d$asset=="D"],col="black")
points(d$tiempo[d$asset=="C"],d$price[d$asset=="C"],col="gray")
#plot(d$tiempo[d$asset=="C"],d$price[d$asset=="C"],type="p",ylim=c(0,120),col='gray',xlab="time",ylab = "price")
points(d$tiempo[d$asset=="B"],d$price[d$asset=="B"],col="blue")
abline(h=200)
abline(h=80,col='red')
abline(h=60,col='blue')
for(i in seq(breakdance)){
  abline(v=breakdance[i],lty=2)
}

median(d$price[d$asset=="B"])
median(d$price[d$asset=="C"])

  
sd(d$price[d$asset=="B"])
sd(d$price[d$asset=="C"])

