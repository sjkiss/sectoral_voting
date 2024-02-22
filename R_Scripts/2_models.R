source("R_Scripts/1_master_file.R")
  # Analysis
#Check reference category for vote
ces$vote
table(ces$election)
library(nnet)
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
         tidied2=map(model, tidy))->mods1

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
  ggplot(., aes(x=election, y=estimate))+
  geom_point()+
  facet_grid(~y.level)+labs(title="Raw multinomial coefficient\nOdds of supporting party v. Cons")
ggsave(filename="Plots/multinomial_coefficients_time.png")
# prop.table(table(as_factor(ces19web$vote), ces19web$sector),2)
# prop.table(table(as_factor(ces19phone$vote), ces19phone$sector),2)

