library(tidyverse)
library(readxl)
library(sandwich)
library(tseries)
ggplot(filter(time_series_GB_20040101_0000_20260421_1634,Time>2023-01-01),aes(y=Immigration,x=Time))+geom_smooth(method = "loess")+geom_jitter()
time_series_GB_20040101_0000_20260421_1634$Time <- ymd(time_series_GB_20040101_0000_20260421_1634$Time)
ggplot(filter(time_series_GB_20040101_0000_20260421_1634,Time>2023-01-01),aes(y=`Reform UK`,x=Time))+geom_smooth(method = "loess")+geom_jitter()
data <- time_series_GB_20040101_0000_20260421_1634 %>% mutate(breakd=ifelse(Time>2020-01-01,1,0))
library(tidyverse)
library(lubridate)

# 1. Convert Time to Date objects first 
# (Crucial to do this before filtering or plotting)
data_clean <- time_series_GB_20040101_0000_20260422_0807 %>%
  mutate(Time = ymd(Time))

# 2. Create the 'breakd' variable correctly using a Date object+)
data_clean <- data_clean %>% 
  mutate(breakdBpl = ifelse(Time > as.Date("2019-04-01"), 1, 0),
breakpukip= ifelse(Time>as.Date("2015-05-01"),1,0),
breakpukipl= ifelse(Time>as.Date("2012-01-01"),1,0))
data_clean$RefUKip = time_series_GB_20040101_0000_20260422_0807$`Reform UK`+time_series_GB_20040101_0000_20260422_0807$`UK Independence Party`

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
# 2. Setup Data with all Structural Break Dummies
data_final <- data_clean %>%
  mutate(
    break2012 = ifelse(Time >= as.Date("2012-01-01"), 1, 0),
    break2015 = ifelse(Time >= as.Date("2015-05-01"), 1, 0),
    break2017 = ifelse(Time >= as.Date("2017-02-01"), 1, 0),
    break2019 = ifelse(Time >= as.Date("2019-04-01"), 1, 0),
    break2020 = ifelse(Time >= as.Date("2020-05-01"), 1, 0),
    break2022 = ifelse(Time >= as.Date("2022-10-01"), 1, 0)
  )

# Define the set of structural dummies for the interaction
# This helps keep the formula clean
dummies <- "(break2012 + break2015 + break2017 + break2019 + break2020 + break2022)"

# 3. Running the Regressions

# Model 1: Reform UK only (The "New" Insurgent Right)
form_reform <- as.formula(paste("`Reform UK` ~ Time + Immigration *", dummies))
model_reform <- lm(form_reform, data = data_final)

# Model 2: Falsification - Conservative support vs. Immigration
form_con_imm <- as.formula(paste("`Conservative Party` ~ Time + Immigration *", dummies))
model_con_imm <- lm(form_con_imm, data = data_final)

# Model 3: Falsification - Labour support vs. Immigration
form_lab_imm <- as.formula(paste("`Labour Party` ~ Time + Immigration *", dummies))
model_lab_imm <- lm(form_lab_imm, data = data_final)

# Model 4: Full Radical Right (UKIP + Reform) as a Baseline
form_rad_sum <- as.formula(paste("RefUKip ~ Time + Immigration *", dummies))
model_rad_sum <- lm(form_rad_sum, data = data_final)

# 4. Generate Robust Standard Errors (HAC)
se_reform  <- sqrt(diag(vcovHAC(model_reform)))
se_con     <- sqrt(diag(vcovHAC(model_con_imm)))
se_lab     <- sqrt(diag(vcovHAC(model_lab_imm)))
se_rad_sum <- sqrt(diag(vcovHAC(model_rad_sum)))

# 5. Output Comparison Table
stargazer(model_reform, model_rad_sum, model_con_imm, model_lab_imm,
          type = "text",
          column.labels = c("Reform Only", "RR (Sum)", "Cons (Fals)", "Lab (Fals)"),
          se = list(se_reform, se_rad_sum, se_con, se_lab),
          star.cutoffs = c(0.05, 0.01, 0.001),
          title = "Falsification Test: Immigration Salience across Parties",
          notes = "Newey-West (HAC) Robust Standard Errors.")
# Define Formula for Inflation Interactions
form_inflation <- "(break2012 + break2015 + break2017 + break2019 + break2020 + break2022)"

# 1. Main Models: The Incumbent vs. The Challenger
model_con_inf <- lm(as.formula(paste("`Conservative Party` ~ Time + Inflation *", form_inflation)), data = data_final)
model_lab_inf <- lm(as.formula(paste("`Labour Party` ~ Time + Inflation *", form_inflation)), data = data_final)

# 2. Falsification: Does Inflation drive the Radical Right?
model_ref_inf <- lm(as.formula(paste("`Reform UK` ~ Time + Inflation *", form_inflation)), data = data_final)
model_rad_inf <- lm(as.formula(paste("RefUKip ~ Time + Inflation *", form_inflation)), data = data_final)

# 3. Robust Standard Errors (HAC)
se_con_inf <- sqrt(diag(vcovHAC(model_con_inf)))
se_lab_inf <- sqrt(diag(vcovHAC(model_lab_inf)))
se_ref_inf <- sqrt(diag(vcovHAC(model_ref_inf)))
se_rad_inf <- sqrt(diag(vcovHAC(model_rad_inf)))
plotdata <- data_clean %>% mutate(fitted1=predict(ols_interaction1),fitted2=predict(ols_interaction2),fitted3=predict(ols_interaction3))
ggplot(plotdata,aes(x=Time))+geom_point(aes(y=RefUKip),colour="purple")+geom_line(aes(y=fitted1),colour="darkblue",linewidth = 1.1)+geom_point(aes(y=`Reform UK`),colour="blue")+geom_line(aes(y=fitted2),colour="darkgreen",linewidth = 1.1)+geom_point(aes(y=`Labour Party`),colour="red")+geom_line(aes(y=fitted3),colour="darkred",linewidth = 1.1)+theme_classic()
ggplot(plotdata,aes(x=Time))+geom_point(aes(y=`Conservative Party`),colour="darkgreen")+geom_line(aes(y=fitted4),colour="darkblue",linewidth = 1.1)+theme_classic()
ggplot(plotdata,aes(x=Time))+geom_point(aes(y=`Labour Party`),colour="red")+geom_line(aes(y=fitted3),colour="darkred",linewidth = 1.1)+theme_classic()
ggplot(plotdata,aes(x=Time))+geom_point(aes(y=`Reform UK`),colour="skyblue")+geom_line(aes(y=fitted2),colour="darkblue",linewidth = 1.1)+theme_classic()
plotdata <- data_clean %>% mutate(fitted1=predict(ols_interaction1),fitted2=predict(ols_interaction2),fitted3=predict(ols_interaction3),fitted4=predict(ols_interaction4))