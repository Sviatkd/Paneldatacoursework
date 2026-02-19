library(readxl)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(lmtest)
library(sandwich)
library(stargazer)
library(coefplot)
intervention <- c(2022,2023,2024)
control <- c("Sweden","France","Norway","Switzerland","Denmark")
treatment <- c("Germany","Czechia","Italy","Netherlands","Belgium")
Industry <- read_csv("Industrycsv.txt")
panel_dfind <- Industry %>%
  mutate(
    intervention = ifelse(date %in% intervention, 1,0),
    covidshock = ifelse(date %in% c(2020,2021),1,0),  
    control = ifelse(country %in% control, 1, 0),
    treatment= ifelse(country %in% treatment,1,0),
    corrosion = ifelse(country=="France"&date%in%c(6,7),1,0),
    did= intervention*treatment)
ggplot(panel_dfind,aes(x=date,y=industrial_production_rate,colour = factor(country)))+geom_line()+theme_bw()
treatment_control_df <- panel_dfind %>%
  filter(treatment + control == 1)
# Visualisations for checking Linearity and Parallel trends#
pd <- position_dodge(0.2)
ggplot(treatment_control_df,aes(x=date,y=industrial_production_rate,colour = factor(country)))+geom_line()+theme_bw()+ggtitle("within group growth levels")
ggplot(treatment_control_df,aes(x=date,y=industrial_production_rate,colour = factor(treatment)))+stat_summary(fun=mean,geom="line",position = pd) + stat_summary(fun.data = mean_se,geom="errorbar",position = pd)+stat_summary(fun=mean,geom="point",position = pd)+theme_bw()+ggtitle(" Group means with standart errors")
ggplot(treatment_control_df,aes(x=date,y=industrial_production_rate,colour = factor(treatment)))+geom_point()+geom_smooth(method = "loess",se=FALSE)+theme_bw()+ggtitle("Loess lines for smooth group conditional means")
ggplot(treatment_control_df,aes(x=date,y=industrial_production_rate,colour = factor(interaction(treatment,intervention))))+geom_point()+geom_smooth(method="loess",se=FALSE,span=0.7)+theme_bw()+ggtitle("Loess regressions before vs after crisis")
ggplot(treatment_control_df,aes(x=date,y=industrial_production_rate,colour = factor(interaction(treatment,intervention))))+geom_point()+geom_smooth(method="lm",se=TRUE)+theme_bw()+ggtitle("Simple regressions before vs after crisis")
pooledcovid <- lm(industrial_production_rate~factor(covidshock)+factor(date)+factor(treatment)+factor(intervention)+factor(did),data=treatment_control_df)
poolednocovid <- lm(industrial_production_rate~factor(date)+factor(treatment)+factor(intervention)+factor(did),data=treatment_control_df)
AIC(pooledcovid,poolednocovid)
BIC(pooledcovid,poolednocovid)
anova(poolednocovid,pooledcovid)
pooledols <- lm(industrial_production_rate~factor(treatment)+factor(intervention)+factor(did),data=treatment_control_df)
olsyearfe <- lm(industrial_production_rate~factor(treatment)+factor(intervention)+factor(did)+factor(date),data=treatment_control_df)
olsyearcfe <- lm(industrial_production_rate~factor(treatment)+factor(intervention)+factor(did)+factor(date)+factor(country),data=treatment_control_df)
AIC(pooledols,olsyearfe,olsyearcfe)
BIC(pooledols,olsyearfe,olsyearcfe)
stargazer(pooledols,olsyearfe,olsyearcfe,type = "text")
coefplot(olsyearcfe)
bptest(olsyearcfe)