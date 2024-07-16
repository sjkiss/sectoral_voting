library(here)
#### Blais replication ####
source(here("R_Scripts/1_master_file.R"))
table(ces$occupation2)
theme_set(theme_bw(base_size=24))
theme_update(
  legend.position="bottom")

library(stringr)
ces$sector<-as_factor(ces$sector)
ces$region2<-relevel(ces$region2, "Ontario")
table(ces$region2)
ces$vote
ces %>%
  #Filter out vote b==Other and Green
  filter(vote!="Green"&vote!="Other") %>%
  #Drop the 2015 web survey
  filter(mode!="Web")%>%
  # group_by(election, ndp, )%>%
  # and the 2021 survey until we get some kind of sector in there
  filter(election!=2021) %>%
  filter(election!=2000) %>%
  filter(election!=1965) %>% 
  filter(election!=1972) %>% 
  #filter(election>1978) %>% 
  filter(!is.na(vote)) %>% 
  #Rename the dichotomous vote variables
  rename(NDP=ndp, Conservative=conservative, Liberal=liberal, Bloc=bloc) %>% 
  #pivot each dichotomous vote variable so that there is one column for the name of the vote variable
  # and a second column with the 1s and 0s of whether or not each R voted for each party. 
  pivot_longer(cols=c(NDP, Conservative, Liberal, Bloc), names_to=c("Party"), values_to=c("Vote")) %>% 
#group_by electiom and party
    group_by(election, Party) %>% 
#nest and store
  #we have to store it here because we have to filter this data frame 
  # to account for the fact that region should not be used with the BQ vote variable 
  nest() ->h1_data
#Do all parties except bloc
h1_data %>% 
  filter(Party!="Bloc") %>% 
  #Fit the model with only sector and controls
  mutate(model1=map(data, function(x) glm(Vote~sector+female+as_factor(region2)+degree+income_tertile+degree+as_factor(religion)+age,data=x, family="binomial")),
#Fit the model with union and controls
                  model2=map(data, function(x) glm(Vote~union_both+female+as_factor(region2)+degree+income_tertile+degree+as_factor(religion)+age,data=x, family="binomial")),
         model3=map(data, function(x) glm(Vote~sector+union_both+female+as_factor(region2)+degree+income_tertile+degree+as_factor(religion)+age,data=x, family="binomial")))  ->mod_h1 

h1_data %>% 
  filter(Party=="Bloc"&election> 1992) %>% 
  #Fit the model with only sector and controls
  mutate(model1=map(data, function(x) glm(Vote~sector+female+income_tertile+as_factor(religion)+age,data=filter(x, quebec==1), family="binomial")),
         #Fit the model with union and controls
         model2=map(data, function(x) glm(Vote~union_both+female+income_tertile+as_factor(religion)+age,data=filter(x, quebec==1), family="binomial")),
         model3=map(data, function(x) glm(Vote~sector+union_both+female+income_tertile+as_factor(religion)+age,data=filter(x, quebec==1), family="binomial")))->mod_h1_qc 
library(marginaleffects)
#non-qc models
mod_h1 %>% 
  #combine election year and party
  mutate(Name=paste(election, Party, sep=" ")) %>% 
  #pick the first model sector only, name with the Name variable
  pull(model1, name=Name) %>% 
  #for each generate comparisons delta P voting for each party public to private
  map(., avg_comparisons, variables=c("sector")) %>% 
  #Tidy
  map(., tidy) %>% 
  #Collapse into list
  #Name each model from Name
  bind_rows(., .id="Name") %>% 
  #create Model variable to distinguish sector from sector with union variable
  #Store in object
  mutate(Model=rep("Sector", nrow(.)))->mod_h1_probs
mod_h1_probs
#Repeat with non-QC model3
mod_h1 %>% 
  mutate(Name=paste(election, Party, sep=" ")) %>% 
  pull(model3, name=Name) %>% 
  #for each generate comparisons delta P voting for each party public to private
  map(., avg_comparisons, variables=c("sector")) %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Name") %>% 
  mutate(Model=rep("Sector+Union", nrow(.))) %>% 
  bind_rows(., mod_h1_probs) ->mod_h1_probs
#Quebec sector only models
mod_h1_qc %>% 
  mutate(Name=paste(election, Party, sep=" ")) %>% 
  pull(model1, name=Name) %>% 
  #for each generate comparisons delta P voting for each party public to private
  map(., avg_comparisons, variables=c("sector")) %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Name") %>% 
  mutate(Model=rep("Sector", nrow(.))) %>% 
  bind_rows(.,mod_h1_probs)->mod_h1_probs
#repeat with quebec sector +union
mod_h1_qc %>% 
  mutate(Name=paste(election, Party, sep=" ")) %>% 
  pull(model3, name=Name) %>% 
  #for each generate comparisons delta P voting for each party public to private
  map(., avg_comparisons, variables=c("sector")) %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Name") %>% 
  mutate(Model=rep("Sector+Union", nrow(.))) %>% 
  bind_rows(., mod_h1_probs) %>% 
  separate(Name, into=c("Election", "Party"))->mod_h1_probs

mod_h1_probs %>% 
  #filter(term=="sectorPublic") %>% 
  ggplot(., aes(x=as.numeric(Election), y=estimate, col=Party,size=Model))+
  geom_point()+
  facet_wrap(~fct_relevel(Party, "Conservative", "Liberal","NDP", "Bloc"),nrow=2, ncol=2)+
  scale_color_manual( values=c("cyan", "darkblue", "darkred", "orange"))+
  guides(col="none")+geom_hline(yintercept=0)+
  labs(x="Election", y="P of voting for Party")+
  geom_smooth(method="loess", aes(linetype=Model),se=F, linewidth=1)+
  scale_size_manual(values=c(1,3))
ggsave(here("Plots/figure_1_GLM_P_voting_Party.png"), width=10, height=7)
#### Create models tables

library(modelsummary)
#vec_rename
#Use this vector to order and rename coefficients in the tables. 
# 
vec_rename<-c("sectorPublic"="Public Sector", 
            "union_both"="Union", 
            "female"="Gender (Female)", 
            "degree"="Degree",
            "as_factor(region2)Atlantic"="Region (Atlantic)",
            "as_factor(region2)Quebec"="Region (Quebec)",
            "as_factor(region2)Ontario"="Region (Ontario)",
            "as_factor(region2)West"="Region (West)", 
            "income_tertile"="Income (Tertile)",
            "as_factor(religion)Catholic"="Religion (Catholic)",
            "as_factor(religion)Protestant"="Religion (Protestant)", 
            "as_factor(religion)Other"="Religion (Other)", 
            "(Intercept)"="Intercept")
mod_h1 %>% 
  filter(Party=="NDP") %>% 
  pull(model1, name='election') %>% 
  modelsummary(., stars=T, coef_map=vec_rename, 
               fmt=2,  gof_omit=c("AIC|BIC|Log.Lik.|RMSE"),
               title="GLM model of vote for NDP", 
               output=here("Tables/A1_ndp.html"))
mod_h1 %>% 
  filter(Party=="Liberal") %>% 
  pull(model1, name='election') %>% 
  modelsummary(., stars=T, gof_omit=c("AIC|BIC|Log.Lik.|RMSE"),coef_map=vec_rename, fmt=2,  title="GLM model of vote for Liberal", output=here("Tables/A2_liberal.html"))
mod_h1 %>% 
  filter(Party=="Conservative") %>% 
  pull(model1, name='election') %>% 
  modelsummary(., stars=T,gof_omit=c("AIC|BIC|Log.Lik.|RMSE"), coef_map=vec_rename, fmt=2,  title="GLM model of vote for Conservative", output=here("Tables/A3_conservative.html"))
mod_h1_qc %>% 
  #filter(Party=="Conservative") %>% 
  pull(model1, name='election') %>% 
  modelsummary(., stars=T,gof_omit=c("AIC|BIC|Log.Lik.|RMSE"), coef_map=vec_rename, fmt=2,  title="GLM model of vote for Conservative", output=here("Tables/A4_bloc.html"))

#### by Decade ####
ces %>% 
  mutate(Decade=case_when(
    election>1968&election<1980~1970,
    election>1979&election<1993~1980,
    election>1988&election<2000~1990,
    election>2000&election<2011~2000,
    election>2010&election<2020~2010
  ))->ces
ces$Decade<-factor(ces$Decade)

library(marginaleffects)
modh1_class_pooled<-glm(ndp~sector*occupation2+union_both+female+as_factor(region2)+degree+income_tertile+degree+as_factor(religion)+age+`1968`+`1974`+`1979`+`1980`+`1984`+`1988`+`1993`+`1997`+`2004`+`2006`+`2008`+`2011`+`2015`+`2019`,data=filter(ces, election>1965&election!=1972), family="binomial")
modh1_class_decade<-glm(ndp~sector*occupation2*Decade+union_both+female+as_factor(region2)+degree+income_tertile+degree+as_factor(religion)+age+`1968`+`1974`+`1979`+`1980`+`1984`+`1988`+`1993`+`1997`+`2004`+`2006`+`2008`+`2011`+`2015`+`2019`,data=filter(ces, election>1965&election!=1972), family="binomial")
modh1_class_1970<-glm(ndp~sector*occupation2+union_both+female+as_factor(region2)+degree+income_tertile+degree+as_factor(religion)+age,data=filter(ces, election>1969&election<1980&election!=1972), family="binomial")
modh1_class_1980<-glm(ndp~sector*occupation2+union_both+female+as_factor(region2)+degree+income_tertile+degree+as_factor(religion)+age,data=filter(ces, election>1979&election<1990), family="binomial")
modh1_class_1990<-glm(ndp~sector*occupation2+union_both+female+as_factor(region2)+degree+income_tertile+degree+as_factor(religion)+age,data=filter(ces, election>1990&election<2000), family="binomial")
modh1_class_2000<-glm(ndp~sector*occupation2+union_both+female+as_factor(region2)+degree+income_tertile+degree+as_factor(religion)+age,data=filter(ces, election>2000&election<2010), family="binomial")
modh1_class_2010<-glm(ndp~sector*occupation2+union_both+female+as_factor(region2)+degree+income_tertile+degree+as_factor(religion)+age,data=filter(ces, election>2010&election<2020&mode!="Web"), family="binomial")
modh1_class_decade_list<-list(modh1_class_1970,modh1_class_1980,modh1_class_1990,modh1_class_2000, modh1_class_2010)
modelsummary(modh1_class_decade_list, stars=T)
modelsummary(modh1_class_decade, stars=T)
modelsummary(modh1_class_decade, stars=T)
table(ces$Decade)
summary(modh1_class)
plot_predictions(modh1_class,by=c("occupation2", "sector"))+
  labs(x="Class", y="P of voting NDP", col="Sector")+
  scale_x_discrete(labels=scales::label_wrap(5))

ggsave(filename=here("Plots/figure_2_vote_vertical.png"), width=10, height=6)

plot_predictions(modh1_class_decade,by=c("occupation2", "sector", "Decade"))+
  labs(x="Class", y="P of voting NDP", col="Sector")+
  scale_x_discrete(labels=scales::label_wrap(5))+
  theme(axis.text=element_text(size=10))+ylim(c(0,0.4))
ggsave(filename=here("Plots/figure_3_vote_vertical_decade.png"), width=12, height=8)

#### 
modh1_vote_branch1<-glm(ndp~sector+as.factor(region2)+as_factor(religion)+non_charter_language+age+female+`1968`+`1974`+`1979`+`1980`+`1984`+`1988`+`1993`+`1997`+`2004`+`2006`+`2008`+`2011`+`2015`+`2019`,data=filter(ces, election>1965&election!=1972), family="binomial")
modh1_vote_branch2<-glm(ndp~sector2+as.factor(region2)+as_factor(religion)+non_charter_language+age+female+`1968`+`1974`+`1979`+`1980`+`1984`+`1988`+`1993`+`1997`+`2004`+`2006`+`2008`+`2011`+`2015`+`2019`,data=filter(ces, election>1965&election!=1972), family="binomial")
modelsummary(list(modh1_vote_branch1, modh1_vote_branch2), stars=T)
library(kableExtra)

modelsummary(modh1_vote, 
             stars=T, coef_map=c("sectorPublic"="Sector (Pubulic)",
                                 "occupation2Routine_Nonmanual"="Routine Non-Manual",
                                 "occupation2Professionals"="Professionals", 
                                 "occupation2Managers"="Managers",
                                 "union_both" ="Union",
                                 "as.factor(region2)Atlantic"="Atlantic",
                                 "as.factor(region2)Ontario"="Ontario" ,
                                 "as.factor(region2)West"="West",
                                 'as_factor(religion)Catholic'='Catholic', 
                                 'as_factor(religion)Protestant'='Protestant',
                                 'as_factor(religion)Other'='Other',
                                 'no_religion'='No Religion',
                                 'non_charter_language'='Non-Charter Language',
                                 'age'='Age',
                                 'female'='Female'),
             , output="kableExtra", 
             gof_omit="AIC|BIC|Log.Lik.|RMSE") %>% 
group_rows(group_label="Class", start_row=3, end_row=8) %>% 
  group_rows(group_label="Region", start_row=11, end_row=15) %>% 
  group_rows(group_label="Relgion", start_row=16, end_row=21) %>% 
  save_kable(., file=here("Tables/table_h1_vote.html"))
modh1_vote_horizontal<-glm(ndp~union_both+as.factor(region2)+as_factor(religion)+non_charter_language+age+female+sector+`1979`+`1980`+`1984`+`1988`+`1993`+`1997`+`2004`+`2006`+`2008`+`2011`+`2015`+`2019`,data=filter(ces, election>1978), family="binomial")
modh1_vote_horizontal2<-glm(ndp~union_both+as.factor(region2)+as_factor(religion)+non_charter_language+age+female+sector2+`1979`+`1980`+`1984`+`1988`+`1993`+`1997`+`2004`+`2006`+`2008`+`2011`+`2015`+`2019`,data=filter(ces, election>1978), family="binomial")
table(ces$sector2)
table(ces$sector_other, ces$sector_welfare)

