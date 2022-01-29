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

bot_d <- filter(dd, asset==4)
bot_bought_d <- sum(bot_d  %>% filter(maker_isBot==TRUE & make_isbid ==TRUE ) %>% select(ex_price))
bot_bought_d2 <- sum(bot_d  %>% filter(taker_isBot==TRUE & make_isbid ==FALSE ) %>% select(ex_price))

bot_sold_d <- sum(bot_d  %>% filter(maker_isBot==TRUE & make_isbid ==FALSE ) %>% select(ex_price))
bot_sold_d2 <- sum(bot_d  %>% filter(taker_isBot==TRUE & make_isbid ==TRUE ) %>% select(ex_price))

bot_u <- filter(dd, asset!=4)
bot_bought_u <- sum(bot_u  %>% filter(maker_isBot==TRUE & make_isbid ==TRUE ) %>% select(ex_price))
bot_bought_u2 <- sum(bot_u  %>% filter(taker_isBot==TRUE & make_isbid ==FALSE ) %>% select(ex_price))

bot_sold_u <- sum(bot_u  %>% filter(maker_isBot==TRUE & make_isbid ==FALSE ) %>% select(ex_price))
bot_sold_u2 <- sum(bot_u %>% filter(taker_isBot==TRUE & make_isbid ==TRUE ) %>% select(ex_price))

profits_bot <- (bot_sold_d+bot_sold_d2-bot_bought_u-bot_bought_u)+bot_sold_u+bot_sold_u2-bot_bought_d-bot_bought_d2
profits_bot/(length(unique(dd$session))*7)

mean(dd$ex_price[dd$asset==3 & dd$round>5])-mean(dd$ex_price[dd$asset==2& dd$round>5])
sum(dd$ex_price[dd$asset==4 & dd$isBot==TRUE])
sum(dd$ex_price[dd$asset!=4& dd$isBot==TRUE])

mean(dd$ex_price[dd$asset==3 & dd$isBot==TRUE & dd$session=='5c09zarp'])
mean(dd$ex_price[dd$asset==3 & dd$isBot==FALSE & dd$session=='5c09zarp'])
mean(dd$ex_price[dd$asset==2  & dd$session=='5c09zarp'])


mean(dd$ex_price[dd$asset==3])-mean(dd$ex_price[dd$asset==2])


tapply(dd$ex_price[dd$asset==2 ],dd$sess[dd$asset==2],mean,na.rm=T)
tapply(dd$ex_price[dd$asset==3],dd$sess[dd$asset==3],mean,na.rm=T)

tapply(dd$ex_price[dd$asset==2 & dd$isBot==F],dd$sess[dd$asset==2 & dd$isBot==F],mean,na.rm=T)
tapply(dd$ex_price[dd$asset==3 & dd$isBot==F],dd$sess[dd$asset==3 & dd$isBot==F],mean,na.rm=T)





dd$taker_isBot*dd$make_isbid
dd$ex_price[c(dd$asset==4 & dd$taker_isBot==TRUE & dd$make_isbid==FALSE)]

