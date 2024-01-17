source("R_Scripts/1_master_file.R")
  # Analysis
#Check reference category for vote
ces$vote
as_factor(ces$vote)
library(nnet)
ces %>% 
  #Filter out vote ==Other and Green
  filter(vote!="Green"&vote!="Other") %>% 
  #Drop the 2015 web survey 
  # It is totally counterituitive but we need to use 
  # the pipe | or "or" in order to exclude the 2015 web survey
  # But not the 2015 phone survey. 
  # Note, should the 2019 web and phone survey are in there too
  filter(election!=2015|mode!="Web") %>% 
  # Note the sector variable is in the 2019 web file 
  # But it hasn't been recoded yet so we will exclude for now
  filter(election!=2019|mode!="Web") %>% 
  # and the 2021 survey until we get some kind of sector in there
  filter(election!=2021) %>% 
  #Note becasue we have two years where there are multiple modes (e.g. web and phone)
  #We should probably separate these out a bit. 
  nest(-c(election)) %>% 
  #Somehow errors were popping up if years before 1974 were included
  # Do we have sector for those years? 
  filter(election>1974) %>% 
  mutate(model=map(data, function(x) multinom(vote~sector, data=x)), 
         tidied=map(model, tidy))->mods1

library(modelsummary)
mods1$model
names(mods1$model)<-c(1979, 1980, 1984, 1988, 1993, 1997, 2000, 2004, 2006, 2008, 2011, 2015, 2019)
modelsummary(mods1$model, 
             shape=term+response~statistic, 
             coef_omit = c("(Intercept)"), 
             stars=T)

#Visualize raw coefficients

mods1 %>% 
  unnest(tidied) %>% 
  filter(term!="(Intercept)") %>% 
  ggplot(., aes(x=election, y=estimate))+
  geom_point()+
  facet_grid(~y.level)
