rm(list = ls())
library(dplyr)
library(tidyverse)
library(xtable)
library(here)
library(nloptr)
load("~/Desktop/jotarepos/capmout/clean/alltrades.Rda")

###An indicator for optimal portfolio is the Sharpe ratio, or the reward to volatility ratio. 
### (r_p - r_f) / sigma_p
### in our case, r_f = 0 due to loan. 
### call theta the risky assets and D the ETF
##### no etfs, if no ETFs are created by the AP, then 
####### the market portfolio quantities are total assets, and the returns are FV/p
######### one needs to check whether that portfolio gives you the maximum ratio compared to others
##### etfs are created, 
######### so now, we have less quantity of underlying assets, and a new quantity of ETFs,
########## again we check whether other portfolio leads to other assets, but now, we include positions like destroying the ETF?


#####we include nav or last transacted price, and current shares outstandings. 

dw = dd 

dw = dd %>% 
  spread(asset, ex_price)

dw[["maker_isBot"]] <- grepl("Bot", dw[["make_pcode"]])
dw[["taker_isBot"]] <- grepl("Bot", dw[["take_pcode"]])

dw$n1<- 0
dw$n2<- 0
dw$n3<- 0
dw$n4<- 0

names(dw)[names(dw) == "1"] <- "a_1"
names(dw)[names(dw) == "2"] <- "a_2"
names(dw)[names(dw) == "3"] <- "a_3"
names(dw)[names(dw) == "4"] <- "a_4"

dw$n4[dw$a_4 > 0 & dw$make_isbid==FALSE & dw$maker_isBot==TRUE]<- 1 #bot sold asset 4 (ETF)
dw$n4[dw$a_4 > 0 & dw$make_isbid==TRUE & dw$taker_isBot==TRUE]<- 1 #bot sold asset 4 

dw$n4[dw$a_4 > 0 & dw$make_isbid==TRUE & dw$maker_isBot==TRUE]<- -1 #bot bought asset 4 
dw$n4[dw$a_4 > 0 & dw$make_isbid==FALSE & dw$taker_isBot==TRUE]<- -1 #bot bought asset 4 

##it turns out that the bot never bought ETF, therefore it never returned underlying assets. 
#### bot is always a taker in underlying assets 

dw$n1[dw$a_1 > 0 & dw$make_isbid==TRUE & dw$taker_isBot==TRUE]<- 1 #bot sold asset 1
dw$n1[dw$a_1 > 0 & dw$make_isbid==FALSE & dw$taker_isBot==TRUE]<- -1 #bot bought asset 1

dw$n2[dw$a_2 > 0 & dw$make_isbid==TRUE & dw$taker_isBot==TRUE]<- 1 #bot sold asset 1
dw$n2[dw$a_2 > 0 & dw$make_isbid==FALSE & dw$taker_isBot==TRUE]<- -1 #bot bought asset 1

dw$n3[dw$a_3 > 0 & dw$make_isbid==TRUE & dw$taker_isBot==TRUE]<- 1 #bot sold asset 1
dw$n3[dw$a_3 > 0 & dw$make_isbid==FALSE & dw$taker_isBot==TRUE]<- -1 #bot bought asset 1

#sort data by time, round, session
dw <- dw %>% group_by(round,session) %>% 
  arrange(tiempo, .by_group = TRUE)

#compute NAV for each asset, 
da <- dw %>% group_by(round,session) %>% 
  fill(a_1, a_2, a_3, a_4)

#compute how many assets are created so far
da <- da %>% group_by(round,session) %>% 
  mutate(q_1 = cumsum(n1), q_2 = cumsum(n2),q_3 = cumsum(n3),q_4 = cumsum(n4))

#keep only when the bot submitted all market orders
da <- da %>% 
  mutate(q_1 = cumsum(n1), q_2 = cumsum(n2),q_3 = cumsum(n3),q_4 = cumsum(n4))

df <- da %>% 
  filter(abs(q_1) == abs(q_4) & tre==1 | abs(q_1)+abs(q_4)==abs(q_3) & tre>1)

df3 <- df %>% 
  filter(is.na(a_1) == F & is.na(a_2) == F & is.na(a_3) == F & is.na(a_4) == T & tre<3)

df3$market_sharpe<-NA
df3$mejor_sharpe<-NA


fv <-matrix(NA,4,3)
fv[1,]<-c(0,120,120)
fv[2,]<-c(90,0,90)
fv[3,]<-c(90,0,90)
fv[4,]<-fv[1,]+2*fv[2,]

m_fv <- apply(fv,1,mean)
m_cov <- cov(t(fv))*2/3


local_opts <- list( "algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-15 )
opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 1000000,
              "local_opts" = local_opts,
              "print_level" = 0 )




for(r in c(1:(dim(df3)[1]))){
  
  eval_f <- function(x){
    prices<-c(df3$a_1[r],df3$a_2[r],df3$a_3[r])
    fvs <- m_fv[1:3]/prices-1
    macov <- cov(t(fv[1:3,]/prices-1))*2/3
    retornos = x%*%fvs
    riesgo = sqrt(t(x)%*%macov%*%x)
    return( -retornos/riesgo )
  }
  
  # constraint function
  eval_g_e <- function(x) {
    return( sum(x)-1)
  }
  
  # Inequality constraints
  #eval_g_ineq <- function (x) {
  #  constr <- c(-x)
  #  return (constr)
  #}
  

  
  # Lower and upper bounds
  lb <- c(0,0,0)
  ub <- c(1,1,1)
  
  # define parameters
  prices<-c(df3$a_1[r],df3$a_2[r],df3$a_3[r])
  fvs <- m_fv[1:3]/prices-1
  macov <- cov(t(fv[1:3,]/prices-1))*2/3
  actual_w <- c(2,2,2)*prices/sum(c(2,2,2)*prices)
  actual_sharpe<- (fvs%*%actual_w)/ sqrt(t(actual_w)%*%macov%*%actual_w)
  df3$market_sharpe[r]<-actual_sharpe
  
  x0 <- c(1/3,1/3,1/3)
  if(fvs[1]<0 & fvs[2]>=0 & fvs[3]>=0){
    x0 <- c(0.0,0.5,0.5)
    }
  #initial values
  if(fvs[1]<0 & fvs[2]<0 & fvs[3]>=0){
    x0 <- c(0.0,0.0,1)
  }
  
  #initial values
  if(fvs[1]<0 & fvs[2]>=0 & fvs[3]<0){
    x0 <- c(0.0,1,0)
  }
  
  #initial values
  if(fvs[1]>=0 & fvs[2]>=0 & fvs[3]<0){
    x0 <- c(0.5,.5,0)
  }
  
  #initial values
  if(fvs[1]>=0 & fvs[2]<0 & fvs[3]>=0){
    x0 <- c(0.5,0,.5)
  }
  
  if(fvs[1]>=0 & fvs[2]<0 & fvs[3]<0){
    x0 <- c(1,0,0)
  }
  
  # Set optimization options.

  res <- nloptr ( x0 = x0,
                  eval_f = eval_f,
                  #a = prices,
                  lb = lb,
                  ub = ub,
                  eval_g_eq = eval_g_e,
                  opts = opts,
                  #fvs = fvs,
                  #macov = macov,
  )
  #print(res)
  wsol = res$solution
  optimal_sharpe<- (fvs%*%wsol)/ sqrt(t(wsol)%*%macov%*%wsol)
  
  df3$mejor_sharpe[r]<-optimal_sharpe
  print(r)
  df3s1<-df3[,c("round","tiempo","session","tre","market_sharpe","mejor_sharpe")]
  save(df3s1,file="sharpe_3s1.Rda")
}
# objective function
  
df3s1$d<-df3s1$mejor_sharpe-df3s1$market_sharpe
tapply(df3s1$d,df3s1$tre,mean)