

#### M1 Blais Replication Extension ####

ces %>% 
  filter(election!=1965 & election!=1972 & election!=2000) %>%
  filter(!(mode=="Web"))%>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+as.factor(religion)+non_charter_language+working_class+union_both+age+male+sector, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete1

ces %>% 
  filter(election!=1965 & election!=1972 & election!=2000) %>%
  filter(!(mode=="Web"))%>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+as.factor(religion)+non_charter_language+working_class+as.factor(occupation)+union_both+age+male+sector, data=x)),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->conservative_models_complete1

ces %>% 
  filter(election!=1965 & election!=1972 & election!=2000) %>%
  filter(!(mode=="Web"))%>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+as.factor(religion)+non_charter_language+working_class+as.factor(occupation)+union_both+age+male+sector, data=x)),
         tidied=map(model, tidy), 
         vote=rep('Liberal', nrow(.)))->liberal_models_complete1

#Join all parties and plot sector coefficients
ndp_models_complete1 %>% 
  bind_rows(., liberal_models_complete1) %>% 
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  mutate(term=Recode(term, "'sector'='Sector'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Union")))+
  geom_point()+
  labs(title="OLS Coefficients of Public Sector on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  scale_color_manual(values=c("blue", "red", "orange"))+
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "M1_blais_replication_all_parties.png"))