library(dplyr)
library(tidyverse)
library(xtable)
library(here)

d<-read.csv(here("detf.csv"),sep=",",header=T, stringsAsFactors = FALSE)

dprice<- d %>%
  group_by(sess, tre, r, activo)  %>%
  summarize(prices=  mean(precio,na.rm = T))

plot(dprice$prices[dprice$activo==2 & dprice$tre==1],dprice$prices[dprice$activo==3 & dprice$tre==1] )
abline(0,1)
points(dprice$prices[dprice$activo==2 & dprice$tre==2],dprice$prices[dprice$activo==3 & dprice$tre==2],col="blue")
points(dprice$prices[dprice$activo==2 & dprice$tre==3],dprice$prices[dprice$activo==3 & dprice$tre==3],col="red")

dprice2<- d %>%
  group_by(tre, r, activo)  %>%
  summarize(prices=  mean(precio,na.rm = T),
            ordenes= mean(z,na.rm = T)
  )

pdf("sum_prices.pdf")
plot(dprice2$r[dprice2$activo==2 & dprice2$tre==1],dprice2$prices[dprice2$activo==3 & dprice2$tre==1]-dprice2$prices[dprice2$activo==2 & dprice2$tre==1],type="l",ylim=c(-10,40),xlab="periods",ylab=expression('P'[C] - 'P'[B]),lwd=2)
lines(dprice2$r[dprice2$activo==2 & dprice2$tre==2],dprice2$prices[dprice2$activo==3 & dprice2$tre==2]-dprice2$prices[dprice2$activo==2 & dprice2$tre==2],col="blue",lwd=2,lty=2)
lines(dprice2$r[dprice2$activo==2 & dprice2$tre==3],dprice2$prices[dprice2$activo==3 & dprice2$tre==3]-dprice2$prices[dprice2$activo==2 & dprice2$tre==3],col="red",lwd=2,lty=3)
abline(h=0,lty=2,col="gray")
legend(1.1,30,c("ABC","A2C","A2C short"),col=c("black", "blue","red"),lty=c(1,2,3),cex=.9,bty = "n")
dev.off()

pdf("sum_ordenes.pdf")
plot(dprice2$r[dprice2$activo==2 & dprice2$tre==1],dprice2$ordenes[dprice2$activo==3 & dprice2$tre==1]-dprice2$ordenes[dprice2$activo==2 & dprice2$tre==1],type="l",ylim=c(-2,5),xlab="periods",ylab=expression('z'[C] - 'z'[B]),lwd=2)
lines(dprice2$r[dprice2$activo==2 & dprice2$tre==2],dprice2$ordenes[dprice2$activo==3 & dprice2$tre==2]-dprice2$ordenes[dprice2$activo==2 & dprice2$tre==2],col="blue",lwd=2,lty=2)
lines(dprice2$r[dprice2$activo==2 & dprice2$tre==3],dprice2$ordenes[dprice2$activo==3 & dprice2$tre==3]-dprice2$ordenes[dprice2$activo==2 & dprice2$tre==3],col="red",lwd=2,lty=3)
abline(h=0,lty=2,col="gray")
legend(5,4,c("ABC","A2C","A2C short"),col=c("black", "blue","red"),lty=c(1,2,3),cex=.9,bty = "n")
dev.off()

data<-d
numero<-1
breakdance<-data$dt*c(1:dim(data)[1])
breakdance<-breakdance[breakdance>0]
#5,6,7,9,


#Draw examples
png("example_z_1.png", width = 700, height = 500)
exa<-"mqshq7o7"
plot(data$z[data$activo==1 & data$sess==exa],cex=1,col="gray",ylim=c(-10,12),type="l",ylab = "order imbalance",xlab="periods",xaxt='n')
lines(data$z[data$activo==2 & data$sess==exa],cex=1,col="red",type="l")
lines(data$z[data$activo==3 & data$sess==exa],lwd=1.5,col="orange",type="l")
lines(data$z[data$activo==4 & data$sess==exa],cex=1,col="black",type="l")
abline(h=0,lty=2,col="gray")
for(j in seq(breakdance)){
  abline(v=breakdance[j],lty=2)
  
}
legend(200,11,legend=c("A","B","C","ETF"),col=c("gray","red","orange","black"),cex=1,bty="n",lty=1,horiz=TRUE,inset=.02)
abline(v=0,lty=2)
xtick<-c(0,breakdance[1:6])
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = seq(1,7), srt = 45, pos = 1, xpd = T)
dev.off()

png("example_p_1.png", width = 700, height = 500)
plot(data$precio[data$activo==1 & data$sess==exa],cex=1,col="gray",ylim=c(0,250),type="o",ylab = "price",xlab="periods",xaxt='n')
points(data$precio[data$activo==2 & data$sess==exa],cex=1,col="red")
points(data$precio[data$activo==3 & data$sess==exa],col="orange")
points(data$precio[data$activo==4 & data$sess==exa],cex=1,pch=16,col="black")
abline(h=200,lty=2,)
abline(h=80,lty=2)
abline(h=60,lty=2)
for(j in seq(breakdance)){
  abline(v=breakdance[j],lty=2)
}
abline(v=0,lty=2)
xtick<-c(0,breakdance[1:6])
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = seq(1,7), srt = 45, pos = 1, xpd = T)

legend(200,11,legend=c("A","B","C","ETF"),col=c("gray","red","orange","black"),cex=1,bty="n",horiz=TRUE,inset=.02,pch=c(1,1,1,16))
dev.off()


png("example_z_2.png", width = 700, height = 500)
exa<-"fbyj5j85"
plot(data$z[data$activo==1 & data$sess==exa],cex=1,col="gray",ylim=c(-10,12),type="l",ylab = "order imbalance",xlab="periods",xaxt='n')
lines(data$z[data$activo==2 & data$sess==exa],cex=1,col="red",type="l")
lines(data$z[data$activo==3 & data$sess==exa],lwd=1.5,col="orange",type="l")
lines(data$z[data$activo==4 & data$sess==exa],cex=1,col="black",type="l")
abline(h=0,lty=2,col="gray")
for(j in seq(breakdance)){
  abline(v=breakdance[j],lty=2)
  
}
abline(v=0,lty=2)
xtick<-c(0,breakdance[1:6])
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = seq(1,7), srt = 45, pos = 1, xpd = T)

legend(200,11,legend=c("A","B","C","ETF"),col=c("gray","red","orange","black"),cex=1,bty="n",lty=1,horiz=TRUE,inset=.02)
dev.off()

png("example_p_2.png", width = 700, height = 500)
plot(data$precio[data$activo==1 & data$sess==exa],cex=1,col="gray",ylim=c(0,300),type="o",ylab = "price",xlab="periods",xaxt='n')
points(data$precio[data$activo==2 & data$sess==exa],cex=1,col="red")
points(data$precio[data$activo==3 & data$sess==exa],col="orange")
points(data$precio[data$activo==4 & data$sess==exa],cex=1,pch=16,col="black")
abline(h=200,lty=2,)
abline(h=80,lty=2)
abline(h=60,lty=2)
for(j in seq(breakdance)){
  abline(v=breakdance[j],lty=2)
}
abline(v=0,lty=2)
xtick<-c(0,breakdance[1:6])
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = seq(1,7), srt = 45, pos = 1, xpd = T)
legend(200,11,legend=c("A","B","C","ETF"),col=c("gray","red","orange","black"),cex=1,bty="n",horiz=TRUE,inset=.02,pch=c(1,1,1,16))
dev.off()




for(numero in seq(1:length(unique(data$sess)))){
  exa<-unique(data$sess)[numero]
  t<-max(data$tre[data$sess==exa])
  nombre<-paste(t,"_",exa,".png",sep="")
  
  png(nombre, width = 1000, height = 500)
  par(mfrow=c(3,2))
  plot(data$bb[data$activo==4 & data$sess==exa],type="l",ylim=c(0,300),ylab = "value",xlab="period",main="ETF",xaxt='n')
  #points(data$z[data$activo==4 & data$sess==exa]*200/60+200,pch=2,cex=.1,col="blue")
  lines(data$bo[data$activo==4 & data$sess==exa],col="red")
  points(data$precio[data$activo==4 & data$sess==exa],pch=1,cex=1)
  abline(h=200,lty=2,col="gray")
  for(j in seq(breakdance)){
    abline(v=breakdance[j],lty=2)
  }
  abline(v=0,lty=2)
  xtick<-c(0,breakdance[1:6])
  axis(side=1, at=xtick, labels = FALSE)
  text(x=xtick,  par("usr")[3], 
       labels = seq(1,7), srt = 45, pos = 1, xpd = T)
  
  plot(data$bb[data$activo==1 & data$sess==exa],type="l",ylim=c(0,100),ylab = "value",xlab="period",main=LETTERS[1],xaxt='n')
  lines(data$bo[data$activo==1 & data$sess==exa],col="red")
  points(data$precio[data$activo==1 & data$sess==exa],pch=1,cex=1)
  abline(h=80,lty=2,col="gray")
  for(j in seq(breakdance)){
    abline(v=breakdance[j],lty=2)
  }
  abline(v=0,lty=2)
  xtick<-c(0,breakdance[1:6])
  axis(side=1, at=xtick, labels = FALSE)
  text(x=xtick,  par("usr")[3], 
       labels = seq(1,7), srt = 45, pos = 1, xpd = T)
  
  plot(data$bb[data$activo==2 & data$sess==exa],type="l",ylim=c(0,100),ylab = "value",xlab="period",main=LETTERS[2],xaxt='n')
  lines(data$bo[data$activo==2 & data$sess==exa],col="red")
  points(data$precio[data$activo==2 & data$sess==exa],pch=1,cex=1)
  abline(h=60,lty=2,col="gray")
  for(j in seq(breakdance)){
    abline(v=breakdance[j],lty=2)
  }
  abline(v=0,lty=2)
  xtick<-c(0,breakdance[1:6])
  axis(side=1, at=xtick, labels = FALSE)
  text(x=xtick,  par("usr")[3], 
       labels = seq(1,7), srt = 45, pos = 1, xpd = T)
  
  
  plot(data$bb[data$activo==3 & data$sess==exa],type="l",ylim=c(0,100),ylab = "value",xlab="period",main=LETTERS[3],xaxt='n')
  lines(data$bo[data$activo==3 & data$sess==exa],col="red")
  points(data$precio[data$activo==3 & data$sess==exa],pch=1,cex=1)
  abline(h=60,lty=2,col="gray")
  for(j in seq(breakdance)){
    abline(v=breakdance[j],lty=2)
  }
  abline(v=0,lty=2)
  xtick<-c(0,breakdance[1:6])
  axis(side=1, at=xtick, labels = FALSE)
  text(x=xtick,  par("usr")[3], 
       labels = seq(1,7), srt = 45, pos = 1, xpd = T)
  
  plot(data$z[data$activo==1 & data$sess==exa],cex=1,col="gray",ylim=c(-10,12),type="l",ylab = "order imbalance",xlab="periods",xaxt='n')
  lines(data$z[data$activo==2 & data$sess==exa],cex=1,col="red",type="l")
  lines(data$z[data$activo==3 & data$sess==exa],lwd=1.5,col="orange",type="l")
  lines(data$z[data$activo==4 & data$sess==exa],cex=1,col="black",type="l")
  abline(h=0,lty=2,col="gray")
  for(j in seq(breakdance)){
    abline(v=breakdance[j],lty=2)
  }
  abline(v=0,lty=2)
  xtick<-c(0,breakdance[1:6])
  axis(side=1, at=xtick, labels = FALSE)
  text(x=xtick,  par("usr")[3], 
       labels = seq(1,7), srt = 45, pos = 1, xpd = T)
  legend(300,11,legend=c("A","B","C","ETF"),col=c("gray","red","orange","black"),cex=.6,bty="n",lty=1,horiz=TRUE,inset=.02)
  dev.off()
}  