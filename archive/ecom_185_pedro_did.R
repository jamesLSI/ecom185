
# recorded #1 ####
## ---- rcode000
library(tidyverse)
library(fixest)
set.seed(1)

## ---- rcode001
id <- rep(seq(1:1000),2)
treat <- (id>500)+0
time <- rep(c(2020,2021),each=1000)
post <- (time==2021)+0
data <- tibble(id=id,time=time,treat=treat,post=post,treatXpost=treat*post)

## ---- rcode002a
data <- data %>% mutate(y=5+0*treat+0*post+10*treatXpost+rnorm(2000))

## ---- rcode002b
data %>% ggplot(aes(x=factor(time),y=y,colour=factor(treat))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(time),y=y,group=factor(treat)),formula = y~x, method="lm") + theme_bw()

## ---- rcode002c
reg1 <- feols(y~treatXpost,data=data)
reg2 <- feols(y~post+treatXpost,data=data)
reg3 <- feols(y~treat+treatXpost,data=data)
reg4 <- feols(y~post+treat+treatXpost,data=data)
etable(reg1,reg2,reg3,reg4,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

## ---- rcode003a
data <- data %>% mutate(y=5+0*treat+5*post+10*treatXpost+rnorm(2000))

## ---- rcode003b
data %>% ggplot(aes(x=factor(time),y=y,colour=factor(treat))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(time),y=y,group=factor(treat)),formula = y~x, method="lm") + theme_bw()

## ---- rcode003c
reg1 <- feols(y~treatXpost,data=data)
reg2 <- feols(y~post+treatXpost,data=data)
reg3 <- feols(y~treat+treatXpost,data=data)
reg4 <- feols(y~post+treat+treatXpost,data=data)
etable(reg1,reg2,reg3,reg4,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

## ---- rcode004a
data <- data %>% mutate(y=5+0*treat-5*post+10*treatXpost+rnorm(2000))

## ---- rcode004b
data %>% ggplot(aes(x=factor(time),y=y,colour=factor(treat))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(time),y=y,group=factor(treat)),formula = y~x, method="lm") + theme_bw()

## ---- rcode004c
reg1 <- feols(y~treatXpost,data=data)
reg2 <- feols(y~post+treatXpost,data=data)
reg3 <- feols(y~treat+treatXpost,data=data)
reg4 <- feols(y~post+treat+treatXpost,data=data)
etable(reg1,reg2,reg3,reg4,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

## ---- rcode005a
data <- data %>% mutate(y=5+0*treat-10*post+10*treatXpost+rnorm(2000))

## ---- rcode005b
data %>% ggplot(aes(x=factor(time),y=y,colour=factor(treat))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(time),y=y,group=factor(treat)),formula = y~x, method="lm") + theme_bw()

## ---- rcode005c
reg1 <- feols(y~treatXpost,data=data)
reg2 <- feols(y~post+treatXpost,data=data)
reg3 <- feols(y~treat+treatXpost,data=data)
reg4 <- feols(y~post+treat+treatXpost,data=data)
etable(reg1,reg2,reg3,reg4,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

## ---- rcode006a
data <- data %>% mutate(y=5+2*treat-2*post+10*treatXpost+rnorm(2000))

## ---- rcode006b
data %>% ggplot(aes(x=factor(time),y=y,colour=factor(treat))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(time),y=y,group=factor(treat)),formula = y~x, method="lm") + theme_bw()

## ---- rcode006c
reg1 <- feols(y~treatXpost,data=data)
reg2 <- feols(y~post+treatXpost,data=data)
reg3 <- feols(y~treat+treatXpost,data=data)
reg4 <- feols(y~post+treat+treatXpost,data=data)
etable(reg1,reg2,reg3,reg4,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

## ---- rcode007a
data <- data %>% mutate(y=5-10*treat-10*post+10*treatXpost+rnorm(2000))

## ---- rcode007b
data %>% ggplot(aes(x=factor(time),y=y,colour=factor(treat))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(time),y=y,group=factor(treat)),formula = y~x, method="lm") + theme_bw()

## ---- rcode007c
reg1 <- feols(y~treatXpost,data=data)
reg2 <- feols(y~post+treatXpost,data=data)
reg3 <- feols(y~treat+treatXpost,data=data)
reg4 <- feols(y~post+treat+treatXpost,data=data)
etable(reg1,reg2,reg3,reg4,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

## ---- rcode008a
data <- data %>% mutate(y=5+10*treat+10*post+0*treatXpost+rnorm(2000))

## ---- rcode008b
data %>% ggplot(aes(x=factor(time),y=y,colour=factor(treat))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(time),y=y,group=factor(treat)),formula = y~x, method="lm") + theme_bw()

## ---- rcode008c
reg1 <- feols(y~treatXpost,data=data)
reg2 <- feols(y~post+treatXpost,data=data)
reg3 <- feols(y~treat+treatXpost,data=data)
reg4 <- feols(y~post+treat+treatXpost,data=data)
etable(reg1,reg2,reg3,reg4,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

rm(list=ls())

# Live #2 ####
## ---- rcode000a_2
library(tidyverse)
library(fixest)
set.seed(1)

## ---- rcode000b_2
N <- 100
T <- 10
id <- rep(seq(1:N),T)
time <- rep(seq(1:T),each=N)

## ---- rcode001a_2
treat <- (id>50)+0
post <- (time>5)+0
data <- tibble(id=id,time=time,treat=treat,post=post,treatXpost=treat*post)
data <- data %>% mutate(y=5+5*treat-2*post+10*treatXpost+rnorm(N*T))

## ---- rcode001b_2
reg1 <- feols(y~treat+post+treatXpost,data=data)
reg2 <- feols(y~treatXpost|id+time,data=data)
etable(reg1,reg2,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

## ---- rcode001c_2
indshock <- rep(runif(N),T)
timeshock <- rep(runif(T),each=N)
data <- data %>% mutate(y=indshock+timeshock+10*treatXpost+rnorm(N*T))

## ---- rcode001d_2
reg1 <- feols(y~treat+post+treatXpost,data=data)
reg2 <- feols(y~treatXpost|id+time,data=data)
etable(reg1,reg2,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

## ---- rcode001e_2
data <- data %>% mutate(y=100*indshock+100*timeshock+10*treatXpost+rnorm(N*T))

## ---- rcode001f_2
reg1 <- feols(y~treat+post+treatXpost,data=data)
reg2 <- feols(y~treatXpost|id+time,data=data)
etable(reg1,reg2,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

## ---- rcode002a_2
data$treatintensity <- rep(runif(N),T)*(time>5)*treat
data <- data %>% mutate(y=indshock+timeshock+10*treatintensity+rnorm(N*T))

## ---- rcode002b_2
p <- filter(data,time==6) %>% ggplot(aes(x=treatintensity)) + geom_histogram() + theme_bw()
plot(p)

## ---- rcode002c_2
reg1 <- feols(y~treat+post+treatintensity,data=data)
reg2 <- feols(y~treatintensity|id+time,data=data)
etable(reg1,reg2,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

## ---- rcode003a_2
treatperiod <- rep(sample(seq(2,9),size=N,replace=TRUE),T)
treatd <- (time>=treatperiod)*treat+0
data$treatd <- treatd
data <- data %>% mutate(y=indshock+timeshock+10*treatd+rnorm(N*T))

## ---- rcode003b_2
p <- filter(data,time==6) %>% ggplot(aes(x=treatd)) + geom_histogram() + theme_bw()
plot(p)

## ---- rcode003c_2
p <- filter(data,time==7) %>% ggplot(aes(x=treatd)) + geom_histogram() + theme_bw()
plot(p)

## ---- rcode003d_2
p <- filter(data,time==8) %>% ggplot(aes(x=treatd)) + geom_histogram() + theme_bw()
plot(p)

## ---- rcode003e_2
p <- filter(data,time==9) %>% ggplot(aes(x=treatd)) + geom_histogram() + theme_bw()
plot(p)

## ---- rcode003f_2
reg1 <- feols(y~treat+treatd,data=data)
reg2 <- feols(y~treatd|id+time,data=data)
etable(reg1,reg2,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

## ---- rcode004a_2
data <- data %>% mutate(y=indshock+timeshock+10*treatXpost+rnorm(N*T))
reg1 <- feols(y~i(time,treat,5)|id+time,data=data)
etable(reg1,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)

## ---- rcode004b_2
iplot(reg1)

## ---- rcode005a_2
data <- data %>% mutate(treateffect=recode(time,`1`=0,`2`=0,`3`=0,`4`=0,`5`=0,`6`=0,`7`=1,`8`=5,`9`=10,`10`=10)*treat)
data <- data %>% mutate(y=indshock+timeshock+treateffect+rnorm(N*T))
reg1 = feols(y~i(time,treat,5)|id+time,data=data)
iplot(reg1)

## ---- rcode006a_2
data <- data %>% mutate(treateffect=recode(time,`1`=0,`2`=0,`3`=0,`4`=0,`5`=0,`6`=10,`7`=9,`8`=6,`9`=2,`10`=0)*treat)
data <- data %>% mutate(y=indshock+timeshock+treateffect+rnorm(N*T))
reg1 = feols(y~i(time,treat,5)|id+time,data=data)
iplot(reg1)

## ---- rcode007a_2
data <- data %>% mutate(treateffect=recode(time,`1`=0,`2`=0,`3`=0,`4`=0,`5`=4,`6`=7,`7`=9,`8`=10,`9`=10,`10`=10)*treat)
data <- data %>% mutate(y=indshock+timeshock+treateffect+rnorm(N*T))
reg1 = feols(y~i(time,treat,5)|id+time,data=data)
iplot(reg1)

## ---- rcode007b_2
data <- data %>% mutate(treateffect=recode(time,`1`=0,`2`=0,`3`=0,`4`=0,`5`=4,`6`=7,`7`=9,`8`=10,`9`=10,`10`=10)*treat)
data <- data %>% mutate(y=indshock+timeshock+treateffect+rnorm(N*T))
reg1 = feols(y~i(time,treat,1)|id+time,data=data)
iplot(reg1)

## ---- rcode008a_2
timeshock <- treat*time
data <- data %>% mutate(y=indshock+timeshock+0*treatXpost+rnorm(N*T))

## ---- rcode008b_2
data %>% ggplot(aes(x=factor(time),y=y,colour=factor(treat))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(time),y=y,group=factor(treat)),formula = y~x, method="lm") + theme_bw()

## ---- rcode008c_2
reg008c = feols(y~i(time,treat,5)|id+time,data=data)
iplot(reg008c)

## ---- rcode008f_2
reg008f = feols(y~i(time,treat,keep=c(2:10),ref=5)|id[time],data=data)

## ---- rcode009a_2
timeshock <- 100*treat*time
data <- data %>% mutate(y=indshock+timeshock+10*treatXpost+rnorm(N*T))

## ---- rcode009b_2
reg009b = feols(y~i(time,treat,5)|id+time,data=data)
iplot(reg009b)

## ---- rcode009f_2
reg009f = feols(y~i(time,treat,keep=c(2:10),ref=5)|id[time],data=data)

## ---- rcode010a_2
timeshock <- treat*time + 25*treat*rep(runif(T),each=N)
data <- data %>% mutate(y=indshock+timeshock+0*treatXpost+rnorm(N*T))

## ---- rcode010b_2
reg010b = feols(y~i(time,treat,5)|id+time,data=data)
iplot(reg010b)

## ---- rcode010f_2
reg010f = feols(y~i(time,treat,keep=c(2:10),ref=5)|id[time],data=data)

## ---- rcode008fi_2
iplot(reg008c)

## ---- rcode008fii_2
iplot(reg008f)

## ---- rcode009fi_2
iplot(reg009b)

## ---- rcode009fii_2
iplot(reg009f)

## ---- rcode010fi_2
iplot(reg010b)

## ---- rcode010fii_2
iplot(reg010f)


