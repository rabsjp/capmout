rm(list = ls())
library(dplyr)
library(tidyverse)
library(xtable)
library(here)
library(coin)

d<-read.csv(here("detf.csv"),sep=",",header=T, stringsAsFactors = FALSE)

spreads<-NA
spreads2<-NA
bb_2<-NA
bb_3<-NA

ba_2<-NA
ba_3<-NA

for(t in c(67:67)){
  d_bids <- d %>% filter(i==t)%>%
    select(r,sess,tre,bb,activo)
  
  d_wb = d_bids %>% 
    spread(activo, bb)
  
  names(d_wb)[names(d_wb) == "1"] <- "bb_1"
  names(d_wb)[names(d_wb) == "2"] <- "bb_2"
  names(d_wb)[names(d_wb) == "3"] <- "bb_3"
  names(d_wb)[names(d_wb) == "4"] <- "bb_4"
  
  d_asks <- d %>% filter(i==t)%>%
    select(r,sess,tre,bo,activo)
  
  d_wa = d_asks %>% 
    spread(activo, bo)
  
  names(d_wa)[names(d_wa) == "1"] <- "bo_1"
  names(d_wa)[names(d_wa) == "2"] <- "bo_2"
  names(d_wa)[names(d_wa) == "3"] <- "bo_3"
  names(d_wa)[names(d_wa) == "4"] <- "bo_4"
  
  d_bo = inner_join(d_wb,d_wa ,by.x=c("r", "sess","tre"),by.y=c("r", "sess","tre"))

  d_bo$b3a2 <-d_bo$bb_3-d_bo$bo_2
  d_bo$a3b2 <-d_bo$bo_3-d_bo$bb_2
  bb_2 = rbind(bb_2,tapply(d_bo$bb_2,d_bo$tre,mean,na.rm=T))
  bb_3 = rbind(bb_3,tapply(d_bo$bb_3,d_bo$tre,mean,na.rm=T))
  ba_2 = rbind(ba_2,tapply(d_bo$bo_2,d_bo$tre,mean,na.rm=T))
  ba_3 = rbind(ba_3,tapply(d_bo$bo_3,d_bo$tre,mean,na.rm=T))
  
  spreads = rbind(spreads,tapply(d_bo$b3a2,d_bo$tre,mean,na.rm=T))
  spreads2 = rbind(spreads2,tapply(d_bo$a3b2,d_bo$tre,mean,na.rm=T))
  
}

table_spr<-rbind(ba_2,ba_3,bb_2,bb_3)
table_spr<-table_spr[!is.na(table_spr[,1]),]
table_spr<-rbind(table_spr,table_spr[4,]-table_spr[1,])

xtable(table_spr)


xtable(as.data.frame(t(table_vender)))


table_vender <- apply(spreads,2,mean,na.rm=T)
xtable(as.data.frame(t(table_vender)))


table_vender2 <- apply(spreads2,2,mean,na.rm=T)
xtable(as.data.frame(t(table_vender2)))


t1 = tapply(d_bo$b3a2[d_bo$tre==1],d_bo$sess[d_bo$tre==1],mean,na.rm=T)
t2 = tapply(d_bo$b3a2[d_bo$tre==2],d_bo$sess[d_bo$tre==2],mean,na.rm=T)
t3 = tapply(d_bo$b3a2[d_bo$tre==3],d_bo$sess[d_bo$tre==3],mean,na.rm=T)

wilcox.test(t1,alternative= "greater")
wilcox.test(t2,alternative= "greater")
wilcox.test(t3,alternative= "greater")

wilcox.test(t1,)
wilcox.test(t2)
wilcox.test(t3)



wilcox.test(t1,t2)
wilcox.test(t1,t3)
wilcox.test(t1,t3)

tt1 = tapply(d_bo$bo_3[d_bo$tre==1]-d_bo$bo_2[d_bo$tre==1],d_bo$sess[d_bo$tre==1],mean,na.rm=T)
tt2 = tapply(d_bo$bo_3[d_bo$tre==2]-d_bo$bo_2[d_bo$tre==2],d_bo$sess[d_bo$tre==2],mean,na.rm=T)
tt3 = tapply(d_bo$bo_3[d_bo$tre==3]-d_bo$bo_2[d_bo$tre==3],d_bo$sess[d_bo$tre==3],mean,na.rm=T)

tt2a= tapply(d_bo$bo_2[d_bo$tre==2],d_bo$sess[d_bo$tre==2],mean,na.rm=T)
tt3a= tapply(d_bo$bo_3[d_bo$tre==2],d_bo$sess[d_bo$tre==2],mean,na.rm=T)

ttt2a= tapply(d_bo$bo_2[d_bo$tre==3],d_bo$sess[d_bo$tre==3],mean,na.rm=T)
ttt3a= tapply(d_bo$bo_3[d_bo$tre==3],d_bo$sess[d_bo$tre==3],mean,na.rm=T)

wilcox.test(tt2,tt1)
wilcox.test(tt3,tt1)

wilcox.test(tt3a,tt3a)



