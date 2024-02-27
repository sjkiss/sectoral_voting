source("R_Scripts/1_master_file.R")
  # Analysis
#Check reference category for vote
ces$vote
table(ces$election)
library(nnet)
library(broom)
ces %>% 
  #Filter out vote ==Other and Green
  filter(vote!="Green"&vote!="Other") %>% 
  #Drop the 2015 web survey 
  filter(!(election==2015&mode=="Web"))%>%
group_by(election, mode)%>%
  # and the 2021 survey until we get some kind of sector in there
  #filter(election!=2021) %>% 
  #Note becasue we have two years where there are multiple modes (e.g. web and phone)
  #We should probably separate these out a bit. 
  nest(-c(election, mode)) %>% 
  #Somehow errors were popping up if years before 1974 were included
  # Do we have sector for those years? 
  filter(election>1974) %>% 
  mutate(model=map(data, function(x) multinom(vote~sector, data=x)), 
         model2=map(data, function(x) multinom(vote~sector+male+degree, data=x)),
         tidied=map(model, tidy), 
         tidied2=map(model2, tidy))->mods1


# Untidied models are best with modelsummary for tables
library(modelsummary)
library(gt)
mods1$model
names(mods1$model)<-c(1979, 1980, 1984, 1988, 1993, 1997, 2000, 2004, 2006, 2008, 2011, 2015, "2019 Phone", "2019 Web", 2021)

modelsummary(mods1$model, 
             shape=term+response~statistic, 
             coef_omit = c("(Intercept)"), 
             stars=T, output="gt") %>% 
gtsave("Tables/table1.html")

#Visualize raw coefficients
# Tidied models are best for plotting
mods1 %>% 
  unnest(tidied) %>% 
  filter(term!="(Intercept)") %>% 
  ggplot(., aes(x=election, y=estimate, col=mode))+
  geom_point()+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  facet_grid(~y.level)+labs(title="Raw multinomial coefficient\nOdds of supporting party v. Cons")
ggsave(filename="Plots/multinomial_coefficients_time.png", width=10, height=6)
# prop.table(table(as_factor(ces19web$vote), ces19web$sector),2)
# prop.table(table(as_factor(ces19phone$vote), ces19phone$sector),2)
# Tidied models are best for plotting
mods1 %>% 
  unnest(tidied2) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate, col=mode))+
  geom_point()+
  facet_grid(~y.level)+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  labs(title="Raw multinomial coefficient\nOdds of supporting party v. Cons controlling for degree and gender")
ggsave(filename="Plots/multinomial_coefficients_time_with_controls.png", width=10, height=6)


# Weight 2019 and 2021
library(srvyr)
lookfor(ces19web, "weigh")
lookfor(ces21, "weight")
#Create ces19web survey design
ces19web %>% 
  #filter in only conservative, liberal, NDP and bq
  filter(vote>0&vote<5) %>% 
  #No missing values are permissible on the weight variable
  filter(!is.na(cps19_weight_general_all)) %>% 
  #Save as survey design, specifying the weight variable
as_survey_design(weight=cps19_weight_general_all)->ces19web_des


#Create ces21 survey design
#Repeat as above
ces21 %>% 
  filter(!is.na(cps21_weight_general_all)) %>%
  filter(vote>0&vote<5) %>% 
  as_survey_design(weight=cps21_weight_general_all)->ces21web_des
library(survey)
#install.packages("VGAM")
#install.packages("svyVGAM")
library(svyVGAM)

#fit multinomial model
svy_vglm(as_factor(vote)~sector, 
         design=ces19web_des, 
         family=multinomial(refLevel="Conservative"))->mod1
svy_vglm(as_factor(vote)~sector, 
         design=ces21web_des, 
         family=multinomial(refLevel="Conservative"))->mod2
summary(mod1)
summary(mod2)
library(modelsummary)

#Compare with the unweighted

modelsummary(list(mod1,mod2))

#------------------------------------------------------------------------------------------------

#### Blais replication ####
ces %>% 
  #Filter out vote ==Other and Green
  filter(vote!="Green" & vote!="Other") %>% 
  #Drop the web surveys 
  filter(!(mode=="Web"))%>%
  group_by(election)%>%
  #We should probably separate these out a bit. 
  nest(-c(election)) %>% 
  #Filter out 1965 and 1972 (no sector), 2000 (no occupation)
  filter(election!=1965 & election!=1972 & election!=2000) %>% 
  mutate(model=map(data, function(x) multinom(vote~sector, data=x)), 
         model2=map(data, function(x) multinom(vote~sector+as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female, data=x)),
         tidied=map(model, tidy), 
         tidied2=map(model2, tidy))->mods10

# Untidied models are best with model summary for tables
library(modelsummary)
library(gt)
mods10$model
names(mods10$model)<-c(1968, 1974, 1979, 1980, 1984, 1988, 1993, 1997, 2004, 2006, 2008, 2011, 2015, "2019 Phone")

modelsummary(mods10$model2, 
             shape=term+response~statistic, 
             coef_omit = c("(Intercept)"), 
             stars=T, output="gt") %>% 
  gtsave("Tables/table2.html")

# Visualize raw coefficients
mods10 %>% 
  unnest(tidied) %>% 
  filter(term!="(Intercept)") %>% 
  ggplot(., aes(x=election, y=estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  facet_grid(~y.level)+labs(title="Raw multinomial coefficient\nOdds of supporting party v. Cons")
ggsave(filename="Plots/multinomial_coefficients_blais.png", width=10, height=6)

mods10 %>% 
  unnest(tidied2) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+
  geom_point()+
  facet_grid(~y.level)+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  labs(title="Raw multinomial coefficient\nOdds of supporting party v. Cons Blais replication")
ggsave(filename="Plots/multinomial_coefficients_time_blais_with_controls.png", width=10, height=6)

#### Modern controls ####
ces %>% 
  #Filter out vote ==Other and Green
  filter(vote!="Green" & vote!="Other") %>% 
  #Drop the web surveys 
  filter(!(mode=="Web"))%>%
  group_by(election)%>%
  #We should probably separate these out a bit. 
  nest(-c(election)) %>% 
  #Filter out 1965 and 1972 (no sector)
  filter(election!=1965 & election!=1972) %>% 
  mutate(model=map(data, function(x) multinom(vote~sector, data=x)), 
         model2=map(data, function(x) multinom(vote~sector+as.factor(region2)+as.factor(religion)+degree+income+age+female+foreign, data=x)),
         model3=map(data, function(x) multinom(vote~sector+as.factor(region2)+as.factor(religion)+degree+income+age+female+foreign+union_both, data=x)),
         tidied=map(model, tidy), 
         tidied2=map(model2, tidy), 
         tidied3=map(model3, tidy))->mods11

# Untidied models are best with model summary for tables
library(modelsummary)
library(gt)
mods11$model
names(mods11$model)<-c(1968, 1974, 1979, 1980, 1984, 1988, 1993, 1997, 2000, 2004, 2006, 2008, 2011, 2015, "2019 Phone")

modelsummary(mods11$model2, 
             shape=term+response~statistic, 
             coef_omit = c("(Intercept)"), 
             stars=T, output="gt") %>% 
  gtsave("Tables/table3.html")

modelsummary(mods11$model3, 
             shape=term+response~statistic, 
             coef_omit = c("(Intercept)"), 
             stars=T, output="gt") %>% 
  gtsave("Tables/table4.html")

# Visualize raw coefficients
mods11 %>% 
  unnest(tidied) %>% 
  filter(term!="(Intercept)") %>% 
  ggplot(., aes(x=election, y=estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  facet_grid(~y.level)+labs(title="Raw multinomial coefficient\nOdds of supporting party v. Cons")
ggsave(filename="Plots/multinomial_coefficients_time3.png", width=10, height=6)

mods11 %>% 
  unnest(tidied2) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+
  geom_point()+
  facet_grid(~y.level)+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  labs(title="Raw multinomial coefficient\nOdds of supporting party v. Cons with modern controls")
ggsave(filename="Plots/multinomial_coefficients_time_with_controls3.png", width=10, height=6)

mods11 %>% 
  unnest(tidied3) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+
  geom_point()+
  facet_grid(~y.level)+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  labs(title="Raw multinomial coefficient\nOdds of supporting party v. Cons with modern controls & union status")
ggsave(filename="Plots/multinomial_coefficients_time_with_controls4.png", width=10, height=6)

#### Modern controls with ideology ####
ces %>% 
  #Filter out vote ==Other and Green
  filter(vote!="Green" & vote!="Other") %>% 
  #Drop the web surveys 
  filter(!(mode=="Web"))%>%
  group_by(election)%>%
  #We should probably separate these out a bit. 
  nest(-c(election)) %>% 
  #Filter out pre-1993 (no political attitudes)
  filter(!(election<1990)) %>% 
  mutate(model=map(data, function(x) multinom(vote~sector, data=x)), 
         model2=map(data, function(x) multinom(vote~sector+as.factor(region2)+as.factor(religion)+degree+income+age+female+foreign+redistribution+immigration_rates, data=x)),
         tidied=map(model, tidy), 
         tidied2=map(model2, tidy))->mods12

# Untidied models are best with model summary for tables
library(modelsummary)
library(gt)
mods12$model
names(mods12$model)<-c(1993, 1997, 2000, 2004, 2006, 2008, 2011, 2015, "2019 Phone")

modelsummary(mods12$model2, 
             shape=term+response~statistic, 
             coef_omit = c("(Intercept)"), 
             stars=T, output="gt") %>% 
  gtsave("Tables/table4.html")

# Visualize raw coefficients
mods12 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+
  geom_point()+
  facet_grid(~y.level)+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  labs(title="Raw multinomial coefficient\nOdds of supporting party v. Cons with modern controls")
ggsave(filename="Plots/multinomial_coefficients_ideology1.png", width=10, height=6)

mods12 %>% 
  unnest(tidied2) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+
  geom_point()+
  facet_grid(~y.level)+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  labs(title="Raw multinomial coefficient\nOdds of supporting party v. Cons with modern controls & ideology")
ggsave(filename="Plots/multinomial_coefficients_ideology2.png", width=10, height=6)

#------------------------------------------------------------------------------------------------
#### Pooled OLS Models by decade ####
#What we need is by decade, minus the BQ and the Greens
ces %>% 
  filter(election<1970 & election> 1959 )->ces.1
ces %>% 
  filter(election<1980 & election> 1969 )->ces.2
ces %>% 
  filter(election<1990 & election> 1979 )->ces.3
ces %>% 
  filter(election<1999 & election> 1989 )->ces.4
ces %>% 
  filter(election<2010 & election> 1999 )->ces.5
ces %>% 
  filter(election<2020 & election> 2009 )->ces.6
names(ces)

# NDP Models
#ces$region2<-relevel(ces$region2, "Atlantic")
m1<-lm(ndp~as.factor(region2)+age+female+degree+income2+as.factor(religion)+foreign+sector+`1968`, data=ces.1)
m2<-lm(ndp~region2+age+female+degree+income2+religion2+foreign+sector+`1974`+`1979`, data=ces.2)
m3<-lm(ndp~region2+age+female+degree+income2+religion2+foreign+sector+`1980`+`1984`+`1988`, data=ces.3)
m4<-lm(ndp~region2+age+female+degree+income2+religion2+foreign+sector+`1993`+`1997`, data=ces.4)
m5<-lm(ndp~region2+age+female+degree+income2+religion2+foreign+sector+`2000`+`2004`+`2006`+`2008`, data=ces.5)
m6<-lm(ndp~region2+age+female+degree+income2+religion2+foreign+sector+`2011`+`2015`+`2019`, data=ces.6)

# Liberal models

# Conservative Models

ols.models<-list(m1, m2, m3, m4, m5, m6)