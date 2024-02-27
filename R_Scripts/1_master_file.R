#Libraries
library(here)
library(tidyverse)
library(labelled)
library(modelsummary)
#Install cesdata2
# Uncomment and install if necessary
#devtools::install_github("sjkiss/cesdata2")
#load cesdata2
library(cesdata2)

library(tidyverse)
#Install cesdata2
#devtools::install_github("sjkiss/github")


#Seprate ces79 and ces80 to two separate files
ces7980 %>% 
  #This variable indicates Rs who completed the 1979 survey, no panel respondents
  filter(V4002==1)->ces79

ces7980 %>% 
  #This variable indicates Rs who completed the 1980 survey, may also havecompleted the 1979 survey; a user should check.
  filter(V4008==1)->ces80
# Drop the `election` variable from ces80
# This is a quirk of the election80 variable; 
# I'll explain another time. Trust me; it is necessary and in this order.
ces80 %>% 
  select(-election)->ces80
#Remove the '80' from the duplicate ces80 variables
# in the ces780
names(ces80)<-str_remove_all(names(ces80), "80")
#Check
tail(names(ces80))

#### There was some wonkiness in Blais' 79-80 move about how he treated public sector workers in 1980
# Do we need to addres that??????
# For a conversation



### Decide On CES 1993 sample
# this command includes only those respondents that completed the 1993 Campaign and Post-Election Survey
# ces93[!is.na(ces93$RTYPE4), ] -> ces93

#### DO WE WANT THE PANEL RESPONDENTS IN THE 04-11 DATASETS? CURRENTLY THE LINES
# BELOW INCLUDES THEM TO MAXIMIZE SAMPLE SIZE 
# COMMENTED LINES BELOW SHOW HOW WE MIGHT 
#Use Panel Respondents for 2004
ces0411 %>%
  filter(str_detect(ces0411$survey, "PES04"))->ces04

# Use Panel Respondets for 2006
ces0411 %>%
  filter(str_detect(ces0411$survey, "PES06"))->ces06
# Use Panel Respondents for 2008
ces0411 %>% 
  filter(str_detect(ces0411$survey, "PES08"))->ces08

#Use Panel respondents for 2011
ces0411 %>% 
  filter(str_detect(ces0411$survey, "PES11"))->ces11

#Rename variables in 2004
#Strip out any instance of `04` in the names of 04
names(ces04)<-str_remove_all(names(ces04), "04")

#Rename variables in 2006
#Strip out any instance of `06` in the names of 06
names(ces06)<-str_remove_all(names(ces06), "06")
#Rename variables in 2008
#Strip out any instance of `08` in the names of 08
names(ces08)<-str_remove_all(names(ces08), "08")

#Rename variables in 2011
#Strip out any instance of `11` in the names of 11
names(ces11)<-str_remove_all(names(ces11), "11")

#### DO WE WANT THE CES15WEB SURVEY IN THIS? IF NOT, WE SHOULD DELETE IT FROM LINES BELOW

# List data frames
ces.list<-list(ces65, ces68, ces72_nov, ces74, ces79, ces80, ces84, ces88, ces93, ces97, ces00, ces04, ces06, ces08, ces11, ces15phone, ces15web, ces19phone, ces19web, ces21)
#Provide names for list

names(ces.list)<-c(1965, 1968, 1972, 1974, 1979, 1980, 1984, 1988, 1993, 1997, 2000, 2004, 2006, 2008, 2011, "2015 Phone", "2015 Web","2019 Phone", "2019 Web", 2021)
#Common variables to be selected
#common_vars<-c('male')
common_vars<-c('male',
               'sector', 
               'occupation',
               'employment', 
               'union_both',
               'region', 'union',
               'degree', 
               'quebec',
               'age', 
               'religion', 
               'vote', 
               'income',
               'redistribution',
               'market_liberalism', 
               'immigration_rates', 
               'traditionalism',
               'traditionalism2', 
               'trad1', 'trad2', 'immigration_rates',
               'market1','market2',
               'turnout', 'mip', 'occupation', 'occupation3', 'education', 'personal_retrospective', 'national_retrospective', 'vote3',
               'efficacy_external', 'efficacy_external2', 'efficacy_internal', 'political_efficacy', 'inequality', 'efficacy_rich', 'promise', 'trust', 'pol_interest', 'foreign',
               'non_charter_language', 'language', 'employment', 'satdem', 'satdem2', 'turnout', 'party_id', 'postgrad', 'income_tertile', 'income2', 'household', 'enviro', 'ideology', 'income_house', 'enviro_spend', 'mode', 'election')

ces.list %>% 
  map(., select, any_of(common_vars))%>%
  #bind_rows smushes all the data frames together, and creates a variable called election
  bind_rows()->ces
#show what we have.
glimpse(ces)

#### SECTION TO MAKE SOME DICHOTOMOUS VARIABLES 
#### WE DID A LOT OF THIS IN THE PREVIOUS 1_MASTER FILE

library(car)
ces$ndp<-car::Recode(ces$vote, "3=1; else=0")
table(ces$ndp)

#Set Reference Category for vote
ces$vote<-as_factor(ces$vote)
ces$vote<-fct_relevel(ces$vote, "Conservative", "Liberal", "NDP", "Bloc")

# Create region2 which is one region variable for all of Canada
ces %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces

# Turn region2 into factor with Quebec as reference case
ces$region2<-factor(ces$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
levels(ces$region2)

# Female
ces %>% 
  mutate(female=case_when(
    male==1~0,
    male==0~1
  ))->ces

# These are party dummies
# Note that we are setting the People's Party to be conservative
ces$ndp<-Recode(ces$vote, "3=1; 0:2=0; 4:6=0; NA=NA")
ces$liberal<-Recode(ces$vote, "1=1; 2:6=0; NA=NA")
ces$conservative<-Recode(ces$vote, "0:1=0; 2=1; 3:5=0; 6=1; NA=NA")
ces$bloc<-Recode(ces$vote, "4=1; 0:3=0; 6=0; else=NA")
ces$green<-Recode(ces$vote, "5=1; 0:4=0; 6=0; else=NA")

#Recode NDP vs Liberals/Right
ces$ndp_vs_right<-Recode(ces$vote, "3=1; 2=0; else=NA")
ces$liberal_vs_right<-Recode(ces$vote, "1=1; 2=0; else=NA")
ces$bloc_vs_right<-Recode(ces$vote, "4=1; 2=0; else=NA")
ces$left<-Recode(ces$vote, "1=1; 3=1; 5=1; 0=0; 2=0; 4=0; 6=0; else=NA")
ces$right<-Recode(ces$vote, "2=1; 0=0; 1=0; 3:5=0; 6=1; else=NA")

# Turn religion into factor with None as reference case
ces$religion2<-Recode(as.factor(ces$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(ces$religion2)
table(ces$religion2)
# Religion dummies
ces$catholic<-Recode(ces$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces$no_religion<-Recode(ces$religion, "0=1; 1:3=0; NA=NA")

# Occupation(occupation 3 and 4 include self-employed as a category)
# Occupation 2 and 4 collapse skilled and Unskilled
ces$occupation2<-Recode(as.factor(ces$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))
ces$occupation2<-fct_relevel(ces$occupation2, "Managers", "Professionals", "Routine_Nonmanual", 'Working_Class')
ces$occupation4<-Recode(as.factor(ces$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals','Self-Employed', 'Routine_Nonmanual'))
# Working Class variables (3 and 4 include self-employed; 2 and 4 are dichotomous where everyone else is set to 0)
ces$working_class<-Recode(ces$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
ces$working_class2<-Recode(ces$occupation, "4:5=1; else=0")
ces$working_class3<-Recode(ces$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
ces$working_class4<-Recode(ces$occupation3, "4:5=1; else=0")

# Create decade dummies
ces$`1960s`<-Recode(ces$election, "1965:1968=1; else=0")
ces$`1970s`<-Recode(ces$election, "1972:1979=1; else=0")
ces$`1980s`<-Recode(ces$election, "1980:1988=1; else=0")
ces$`1990s`<-Recode(ces$election, "1993:1997=1; else=0")
ces$`2000s`<-Recode(ces$election, "2000:2008=1; else=0")
ces$`2010s`<-Recode(ces$election, "2011:2019=1; else=0")

# Create Time Dummies
ces$`1965`<-Recode(ces$election, "1965=1; else=0")
ces$`1968`<-Recode(ces$election, "1968=1; else=0")
ces$`1972`<-Recode(ces$election, "1972=1; else=0")
ces$`1974`<-Recode(ces$election, "1974=1; else=0")
ces$`1979`<-Recode(ces$election, "1979=1; else=0")
ces$`1980`<-Recode(ces$election, "1980=1; else=0")
ces$`1984`<-Recode(ces$election, "1984=1; else=0")
ces$`1988`<-Recode(ces$election, "1988=1; else=0")
ces$`1993`<-Recode(ces$election, "1993=1; else=0")
ces$`1997`<-Recode(ces$election, "1997=1; else=0")
ces$`2000`<-Recode(ces$election, "2000=1; else=0")
ces$`2004`<-Recode(ces$election, "2004=1; else=0")
ces$`2006`<-Recode(ces$election, "2006=1; else=0")
ces$`2008`<-Recode(ces$election, "2008=1; else=0")
ces$`2011`<-Recode(ces$election, "2011=1; else=0")
ces$`2015`<-Recode(ces$election, "2015=1; else=0")
ces$`2019`<-Recode(ces$election, "2019=1; else=0")

