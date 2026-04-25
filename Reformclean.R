library(tidyverse)
library(readxl)
library(sandwich)
library(tseries)
library(tidyverse)
library(lubridate)
library(strucchange)
library(stargazer)
# 1. Convert Time to Date objects first 
# (Crucial to do this before filtering or plotting)
data_clean <- time_series_GB_20040101_0000_20260422_0807 %>%
  mutate(Time = ymd(Time))
data_clean$RefUKip = time_series_GB_20040101_0000_20260422_0807$`Reform UK`+time_series_GB_20040101_0000_20260422_0807$`UK Independence Party`
summary(strucchange::breakpoints(data_clean$Immigration~1))
summary(strucchange::breakpoints(data_clean$Immigration~data_clean$RefUKip))
# 2. Create the 'breakd' variable correctly using estimated breakpoints,Alternative(robust)
data_clean <- data_clean %>% 
  mutate(breakdBpl = ifelse(Time > as.Date("2022-12-01"), 1, 0),
         breakpukip= ifelse(Time>as.Date("2017-06-01"),1,0),
         breakpukipl= ifelse(Time>as.Date("2008-09-01"),1,0))
#Create breakpoints around known immigration controversies(less robust,main)
data_clean <- data_clean %>% 
  mutate(breakdBpl = ifelse(Time > as.Date("2019-04-01"), 1, 0),
         breakpukip= ifelse(Time>as.Date("2015-05-01"),1,0),
         breakpukipl= ifelse(Time>as.Date("2012-01-01"),1,0))

# 4. Plot Reform UK Support (Post-2023)
ggplot(filter(data_clean, Time > as.Date("2023-01-01")), aes(x = Time, y = `Reform UK`)) +
  geom_jitter(alpha = 0.4) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Reform UK Polling Trends", subtitle = "Post-2023 Data") +
  theme_minimal()
# The '*' shorthand automatically includes the main effects and the interaction
ols_interaction1 <- lm(RefUKip ~ Time+Immigration * breakdBpl+Immigration * breakpukip+Immigration * breakpukipl, data = data_clean)
ols_interaction2 <- lm(`Reform UK` ~ Time+Immigration * breakdBpl+Immigration * breakpukip+Immigration * breakpukipl, data = data_clean)
ols_interaction3 <- lm(`Labour Party` ~ Time+Immigration * breakdBpl+Immigration * breakpukip+Immigration * breakpukipl, data = data_clean)
ols_interaction4 <- lm(`Conservative Party` ~ Time+Immigration * breakdBpl+Immigration * breakpukip+Immigration * breakpukipl, data = data_clean)
plotdata <- data_clean %>% mutate(fitted1=predict(ols_interaction1),fitted2=predict(ols_interaction2),fitted3=predict(ols_interaction3),fitted4=predict(ols_interaction4))
ggplot(plotdata,aes(x=Time))+geom_point(aes(y=RefUKip),colour="purple")+geom_line(aes(y=fitted1),colour="darkblue",linewidth = 1.1)+geom_point(aes(y=`Labour Party`),colour="red")+geom_line(aes(y=fitted3),colour="darkred",linewidth = 1.1)+theme_classic()
ggplot(plotdata,aes(x=Time))+geom_point(aes(y=`Conservative Party`),colour="darkgreen")+geom_line(aes(y=fitted4),colour="darkblue",linewidth = 1.1)+theme_classic()
ggplot(plotdata,aes(x=Time))+geom_point(aes(y=`Labour Party`),colour="red")+geom_line(aes(y=fitted3),colour="darkred",linewidth = 1.1)+theme_classic()
ggplot(plotdata,aes(x=Time))+geom_point(aes(y=`Reform UK`),colour="skyblue")+geom_line(aes(y=fitted2),colour="darkblue",linewidth = 1.1)+theme_classic()
stargazer(ols_interaction1,ols_interaction2,ols_interaction3,ols_interaction4, 
          se = list(sqrt(diag(vcovHAC(ols_interaction1))),sqrt(diag(vcovHAC(ols_interaction2))),sqrt(diag(vcovHAC(ols_interaction3))),sqrt(diag(vcovHAC(ols_interaction4)))), 
          type = "text")
tseries::adf.test(resid(ols_interaction1))
tseries::adf.test(resid(ols_interaction2))
tseries::adf.test(resid(ols_interaction3))
tseries::adf.test(resid(ols_interaction4))
ols_interaction1 <- lm(RefUKip ~ Time+Inflation * breakdBpl+Inflation * breakpukip+Inflation * breakpukipl, data = data_clean)
ols_interaction2 <- lm(`Reform UK` ~ Time+Inflation * breakdBpl+Inflation * breakpukip+Inflation * breakpukipl, data = data_clean)
ols_interaction3 <- lm(`Labour Party` ~ Time+Inflation * breakdBpl+Inflation * breakpukip+Inflation * breakpukipl, data = data_clean)
ols_interaction4 <- lm(`Conservative Party` ~ Time+Inflation * breakdBpl+Inflation * breakpukip+Inflation * breakpukipl, data = data_clean)
stargazer(ols_interaction1,ols_interaction2,ols_interaction3,ols_interaction4, 
          se = list(sqrt(diag(vcovHAC(ols_interaction1))),sqrt(diag(vcovHAC(ols_interaction2))),sqrt(diag(vcovHAC(ols_interaction3))),sqrt(diag(vcovHAC(ols_interaction4)))), 
          type = "text")
tseries::adf.test(resid(ols_interaction1))
tseries::adf.test(resid(ols_interaction2))
tseries::adf.test(resid(ols_interaction3))
tseries::adf.test(resid(ols_interaction4))
plotdata <- data_clean %>% mutate(fitted1=predict(ols_interaction1),fitted2=predict(ols_interaction2),fitted3=predict(ols_interaction3),fitted4=predict(ols_interaction4))
ggplot(plotdata,aes(x=Time))+geom_point(aes(y=RefUKip),colour="purple")+geom_line(aes(y=fitted1),colour="darkblue",linewidth = 1.1)+theme_classic()
ggplot(plotdata,aes(x=Time))+geom_point(aes(y=`Conservative Party`),colour="darkgreen")+geom_line(aes(y=fitted4),colour="darkblue",linewidth = 1.1)+theme_classic()
ggplot(plotdata,aes(x=Time))+geom_point(aes(y=`Labour Party`),colour="red")+geom_line(aes(y=fitted3),colour="darkred",linewidth = 1.1)+theme_classic()
ggplot(plotdata,aes(x=Time))+geom_point(aes(y=`Reform UK`),colour="skyblue")+geom_line(aes(y=fitted2),colour="darkblue",linewidth = 1.1)+theme_classic()
