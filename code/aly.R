rm(list = ls())
library(dplyr)
library(tidyverse)
library(xtable)
library(here)

load("~/Desktop/jotarepos/capmout/clean/alldata.Rda")
d[["isBot"]] <- grepl("Bot", d[["pcode"]]) 
write.csv(d,file="dtrades.csv",row.names=FALSE)

sujetos<-unique(cbind(d$pcode[d$isBot==F],d$tre[d$isBot==F]))
table(d$session[d$isBot==F],d$asset[d$isBot==F])

