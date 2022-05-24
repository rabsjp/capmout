rm(list = ls())
library(dplyr)
library(tidyverse)
library(xtable)
library(here)
load("~/Desktop/jotarepos/capmout/clean/alltrades.Rda")


dd[["isBot"]] <- grepl("Bot", dd[["make_pcode"]]) | grepl("Bot", dd[["take_pcode"]])
dd[["maker_isBot"]] <- grepl("Bot", dd[["make_pcode"]])
dd[["taker_isBot"]] <- grepl("Bot", dd[["take_pcode"]])
write.csv(dd,file="dtrades.csv",row.names=FALSE)


table(dd$asset,dd$isBot)

bot_d <- filter(dd, asset==4 & tre==1)
bot_u <- filter(dd, asset!=4 & tre==1)

bot_bought_d <- sum(bot_d  %>% filter(maker_isBot==TRUE & make_isbid ==TRUE ) %>% select(ex_price))
bot_bought_d2 <- sum(bot_d  %>% filter(taker_isBot==TRUE & make_isbid ==FALSE ) %>% select(ex_price))

bot_sold_d <- sum(bot_d  %>% filter(maker_isBot==TRUE & make_isbid ==FALSE ) %>% select(ex_price))
bot_sold_d2 <- sum(bot_d  %>% filter(taker_isBot==TRUE & make_isbid ==TRUE ) %>% select(ex_price))

bot_bought_u <- sum(bot_u  %>% filter(maker_isBot==TRUE & make_isbid ==TRUE ) %>% select(ex_price))
bot_bought_u2 <- sum(bot_u  %>% filter(taker_isBot==TRUE & make_isbid ==FALSE ) %>% select(ex_price))

bot_sold_u <- sum(bot_u  %>% filter(maker_isBot==TRUE & make_isbid ==FALSE ) %>% select(ex_price))
bot_sold_u2 <- sum(bot_u %>% filter(taker_isBot==TRUE & make_isbid ==TRUE ) %>% select(ex_price))

profits_bot <- (bot_sold_d+bot_sold_d2-bot_bought_u-bot_bought_u2)+bot_sold_u+bot_sold_u2-bot_bought_d-bot_bought_d2
profits_bot/(5*7)


sum(dd$ex_price[dd$asset==4 & dd$isBot==TRUE & dd$tre==1]>0 )/(5*7)


mean(dd$ex_price[dd$asset==3 & dd$round>5])-mean(dd$ex_price[dd$asset==2& dd$round>5])
sum(dd$ex_price[dd$asset!=4& dd$isBot==TRUE])

mean(dd$ex_price[dd$asset==3 & dd$isBot==TRUE & dd$tre==1])
mean(dd$ex_price[dd$asset==4 & dd$isBot==FALSE & dd$session=='5c09zarp'])

mean(dd$ex_price[dd$asset==2  & dd$session=='5c09zarp'])

mean(dd$ex_price[dd$asset==3])-mean(dd$ex_price[dd$asset==2])


tapply(dd$ex_price[dd$asset==2 ],dd$sess[dd$asset==2],mean,na.rm=T)
tapply(dd$ex_price[dd$asset==3],dd$sess[dd$asset==3],mean,na.rm=T)

tapply(dd$ex_price[dd$asset==2 & dd$isBot==F],dd$sess[dd$asset==2 & dd$isBot==F],mean,na.rm=T)
tapply(dd$ex_price[dd$asset==3 & dd$isBot==F],dd$sess[dd$asset==3 & dd$isBot==F],mean,na.rm=T)

tapply(dd$ex_price[dd$asset==3],dd$sess[dd$asset==3],mean,na.rm=T)


dd$taker_isBot*dd$make_isbid
dd$ex_price[c(dd$asset==4 & dd$taker_isBot==TRUE & dd$make_isbid==FALSE)]


q1_1<-c(table(dd$sess[dd$asset==1 &dd$tre==1]))
q2_1<-c(table(dd$sess[dd$asset==2 &dd$tre==1]))
q3_1<-c(table(dd$sess[dd$asset==3 &dd$tre==1]))
q4_1<-c(table(dd$sess[dd$asset==4 &dd$tre==1]))

mean(q2_1/q1_1)

q1_2<-c(table(dd$sess[dd$asset==1 &dd$tre==2]))
q2_2<-c(table(dd$sess[dd$asset==2 &dd$tre==2]))
q3_2<-c(table(dd$sess[dd$asset==3 &dd$tre==2]))
q4_2<-c(table(dd$sess[dd$asset==4 &dd$tre==2]))

mean(q2_2/q1_2)
mean(q3_2/q1_2)

wilcox.test(q2_2/q1_2,q3_2/q1_2)
q1_3<-c(table(dd$sess[dd$asset==1 &dd$tre==3]))
q2_3<-c(table(dd$sess[dd$asset==2 &dd$tre==3]))
q3_3<-c(table(dd$sess[dd$asset==3 &dd$tre==3]))
q4_3<-c(table(dd$sess[dd$asset==4 &dd$tre==3]))

mean(q2_3/q1_3)
mean(q3_3/q1_3)

wilcox.test(q2_3-q3_3)
wilcox.test(q2_2/q3_2,q2_1/q3_1)
wilcox.test(q2_3/q3_3,q2_1/q3_1)

t1 = c(mean(q2_1/q1_1),mean(q3_1/q1_1),mean(q4_1/q1_1))
t2 = c(mean(q2_2/q1_2),mean(q3_2/q1_2),mean(q4_2/q1_2))
t3 = c(mean(q2_3/q1_3),mean(q3_3/q1_3),mean(q4_2/q1_3))
rbind(t1,t2,t3)


t1c = c(mean(q1_1/q3_1),mean(q2_1/q1_1),mean(q4_1/q3_1))
t2c = c(mean(q1_2/q3_2),mean(q2_2/q3_2),mean(q4_2/q3_2))
t3c = c(mean(q1_3/q3_3),mean(q2_3/q3_3),mean(q4_2/q3_3))
rbind(t1c,t2c,t3c)



mean(q1_1/q3_1)



spread<-matrix(NA,3,5)
qETF<-matrix(NA,3,5)
pETF<-matrix(NA,3,5)
##Count how many transactions are in parity, 
for(t in 1:3){
  list_s <-unique(dd$session[dd$tre==t])
  
  for(s in 1:length(list_s)){
    bot_etf <- filter(dd, asset==4 & tre==t  & session==list_s[s] & isBot==T)
    bot_und <- filter(dd, asset!=4 & tre==t  & session==list_s[s] & isBot==T)
    
    bot_etf <- bot_etf %>% arrange(tiempo)
    bot_und <- bot_und %>% arrange(tiempo)
    
    bot_etf<- mutate(bot_etf, id = row_number())
    bot_und<- mutate(bot_und, id = ceiling(row_number()/3))
    et<- tapply(bot_etf$ex_price,bot_etf$id,sum,na.rm=T)
    ud<-tapply(bot_und$ex_price,bot_und$id,sum,na.rm=T)
    
    spread[t,s]<- sum(et-ud)
    qETF[t,s]<- length(et)
    pETF[t,s]<- sum(et==ud)
  }
}  

