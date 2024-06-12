### Statistiques descriptives sur le recours au contrat de professionnalisation par classe d'âges ###

library('dplyr')
library('tidyr')
library('lubridate')
library('ggplot2')

data_sese <- "C:\\Users\\kgaba\\OneDrive - GENES\\Internship"

source(file.path(data_sese,'dares_scripts', 'read_sip.R'),   encoding = 'UTF-8')

#Variables à garder pour l'analyse courante

var_to_keep <- c('CONT_DATEDEB', 'POND_POEM', 'SAL_AGEDEBCONT', 'CONT_DATEFINEFF_POEM', 'SAL_SITAVTCONT', 'CONT_MOTIFRUP_R')


#Contrats de professionnalisation débutés depuis 2017

read_sip(csv_file = file.path(data_sese, 'dares_data', 'SIP_CPRO_CONT_2017.csv'), name_out = 'annee2017')
read_sip(csv_file = file.path(data_sese, 'dares_data', 'SIP_CPRO_CONT_2018.csv'), name_out = 'annee2018')
read_sip(csv_file = file.path(data_sese, 'dares_data', 'SIP_CPRO_CONT_2019.csv'), name_out = 'annee2019')
read_sip(csv_file = file.path(data_sese, 'dares_data', 'SIP_CPRO_CONT_ACTU.csv'), name_out = 'actu')

                                 
toutes_tables <- rbind(annee2017, annee2018, annee2019, actu)


# On garde les variables se rapportant aux entrants en contrats pro 
#On rajoute 4 variables : Trimestre de début et de fin effective du contrat et sa durée effective ainsi qu'un dummy pour l'appartenance à la cohorte d'intérêt

table_sal_cont <- toutes_tables %>%
  select(starts_with("SAL") | starts_with("CONT")) %>% 
  mutate(CIBLE = as.factor(ifelse(SAL_SITAVTCONT == 9 & SAL_AGEDEBCONT >= 26, 1,0)),
         CONT_TRIM_DEB = quarter(CONT_DATEDEB, type = "year.quarter"),
         CONT_TRIM_FIN = quarter(CONT_DATEFINEFF_POEM, type = "year.quarter"),
         CONT_DUREFF = ceiling(difftime(CONT_DATEFINEFF_POEM, CONT_DATEDEB, units = "weeks")),
         CONT_ANNEEDEB = year(CONT_DATEDEB),
         CONT_ANNEEFIN = year(CONT_DATEFINEFF_POEM)) %>% 
  arrange(CONT_TRIM_DEB)



####--- SUR LES CARACTERISTIQUES DES SALARIES ---####

# SUR L'AGE

## Age en début de contrat par année de début de contrat

table_sal_cont %>% 
  filter(!SAL_SITAVTCONT %in% c("", "99")) %>% 
  group_by(CONT_TRIM_DEB, SAL_SITAVTCONT) %>% 
  summarise(mean_age = mean(SAL_AGEDEBCONT)) %>% 
  group_by(CONT_TRIM_DEB) %>% 
  slice_max(order_by = mean_age, n=2) %>% 
  View(title = "max_age_mean")

table_sal_cont %>% 
  group_by(SAL_SITAVTCONT) %>% 
  summarise(mean_age = mean(SAL_AGEDEBCONT)) %>% 
  View(title = "age mean") # Moyenne d'âge plus élevée dans la cohorte cible (sauf en contrat aidé mais marginalement)

# Distribution de la variable âge dans la cohorte cible

table_sal_cont %>% 
  filter(SAL_SITAVTCONT == "9") %>%
  ggplot(aes(x = SAL_AGEDEBCONT, colour = CONT_ANNEEDEB, fill = CONT_ANNEEDEB, group = CONT_ANNEEDEB))+
           geom_density(alpha = 0.5)+
           facet_grid(CONT_ANNEEDEB~.)

table_sal_cont %>% 
  filter(SAL_SITAVTCONT == "9" & as.numeric(SAL_AGEDEBCONT < 26)) %>%
  ggplot(aes(x = SAL_AGEDEBCONT, colour = CONT_ANNEEDEB, fill = CONT_ANNEEDEB, group = CONT_ANNEEDEB))+
  geom_density(alpha = 0.5)+
  facet_grid(CONT_ANNEEDEB~.)
  
table_sal_cont %>% 
  filter(SAL_SITAVTCONT == "9") %>%
  ggplot(aes(x = SAL_AGEDEBCONT, colour = CONT_ANNEEDEB, fill = CONT_ANNEEDEB, group = CONT_ANNEEDEB))+
  geom_boxplot()
  facet_grid(CONT_ANNEEDEB~.)
  
  

## SUR LE GENRE

# Distribution par genre dans les cohortes annuelles 

table_sal_cont %>% 
  ggplot(aes(x = CONT_ANNEEDEB, fill = SAL_SEXE_R)) +
  geom_bar(position = "fill")+
  facet_grid(CIBLE~.)
  
## SUR L'ORIGINE SOCIO-DEM

# Parmi ceux qui viennent de qpv qui est dans la cohorte ? 

table_sal_cont %>%
  filter(!is.na(SAL_QPV == TRUE))%>%
  group_by(CONT_ANNEEDEB, CIBLE) %>% 
  summarise(n=n()) %>%
  group_by(CONT_ANNEEDEB) %>% 
  mutate(share = n*100 / sum(n)) %>% 
  View()

# Dans chaque cohorte comment évolue la part de ceux qui viennent de qpv 


table_sal_cont %>%
  filter(CIBLE == 1) %>% 
  filter(!is.na(SAL_QPV == TRUE))%>%
  group_by(CONT_ANNEEDEB, SAL_QPV) %>% 
  summarise(n=n()) %>%
  group_by(CONT_ANNEEDEB) %>% 
  mutate(share = n*100 / sum(n)) %>%
  filter(SAL_QPV == TRUE) %>% 
  View()

table_sal_cont %>%
  filter(CIBLE == 0) %>% 
  filter(!is.na(SAL_QPV == TRUE))%>%
  group_by(CONT_ANNEEDEB, SAL_QPV) %>% 
  summarise(n=n()) %>%
  group_by(CONT_ANNEEDEB) %>% 
  mutate(share = n*100 / sum(n)) %>%
  filter(SAL_QPV == TRUE) %>% 
  View()


# Quelle répartition par type de contrats signés au fil des années dans la cohorte ?


table_sal_cont %>%
  filter(CIBLE == 0) %>% 
  group_by(CONT_ANNEEDEB, CONT_MODECONT) %>%
  summarise(n=n()) %>%
  group_by(CONT_ANNEEDEB) %>% 
  mutate(share = n*100 / sum(n)) %>% 
  arrange(CONT_MODECONT,CONT_ANNEEDEB) %>% 
  View()


table_sal_cont %>%
  filter(CIBLE == 1) %>% 
  group_by(CONT_ANNEEDEB, CONT_MODECONT) %>%
  summarise(n=n()) %>%
  group_by(CONT_ANNEEDEB) %>% 
  mutate(share = n*100 / sum(n)) %>% 
  arrange(CONT_MODECONT,CONT_ANNEEDEB) %>% 
  View()

# Comment évoluent leur part dans les entrées en contrat pro par trimestre d'entrée ? 

share_table %>% 
  ggplot(aes(CONT_TRIM_DEB, share, fill = CIBLE, group = CIBLE))+
  geom_col(position = "dodge")


share_table %>% 
  filter(CIBLE == 1) %>% 
  View()


toutes_tables %>%
  filter(SAL_SITAVTCONT == 3) %>% 
  mutate(CONT_ANNEEDEB = year(CONT_DATEDEB)) %>% 
  group_by(CONT_ANNEEDEB, CONT_TRIM_DEB)%>% 
  summarise(n=n()) %>%
  group_by(CONT_ANNDEB) %>% 
  mutate(share = n*100 / sum(n)) %>% 
  arrange(CONT_MODECONT,CONT_ANNEEDEB) %>% 
  View()

toutes_tables %>% 
  group_by(SAL_SITAVTCONT) %>% 
  summarise(n = n())


toutes_tables %>% 
  group_by(SAL_SITAVTCONT) %>% 
  summarise(mean_age = mean(SAL_AGEDEBCONT))



