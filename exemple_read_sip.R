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

read_sip(csv_file = file.path(data_sese, 'dares_data', 'SIP_CPRO_CONT_2017.csv'), keep_var = var_to_keep, name_out = 'annee2017')
read_sip(csv_file = file.path(data_sese, 'dares_data', 'SIP_CPRO_CONT_2018.csv'), keep_var = var_to_keep, name_out = 'annee2018')
read_sip(csv_file = file.path(data_sese, 'dares_data', 'SIP_CPRO_CONT_2019.csv'), keep_var = var_to_keep, name_out = 'annee2019')
read_sip(csv_file = file.path(data_sese, 'dares_data', 'SIP_CPRO_CONT_ACTU.csv'), keep_var = var_to_keep, name_out = 'actu')

                                 
toutes_tables <- rbind(annee2017, annee2018, annee2019, actu)

toutes_tables <- toutes_tables %>% 
  mutate(CIBLE = as.factor(ifelse(SAL_SITAVTCONT == 9 & SAL_AGEDEBCONT >= 26, 1,0)),
         CONT_TRIM_DEB = quarter(CONT_DATEDEB, type = "year.quarter"),
         CONT_TRIM_FIN = quarter(CONT_DATEFINEFF_POEM, type = "year.quarter"),
         CONT_DUREFF = ceiling(difftime(CONT_DATEFINEFF_POEM, CONT_DATEDEB, units = "weeks"))) %>% 
  arrange(CONT_TRIM_DEB)


# Comment évoluent leur part dans les entrées en contrat pro par trimestre d'entrée ? 

share_table <- toutes_tables %>%
  group_by(CONT_TRIM_DEB, CIBLE)%>% 
  summarise(n=n()) %>%
  group_by(CONT_TRIM_DEB) %>% 
  mutate(share = n*100 / sum(n))


share_table %>% 
  ggplot(aes(CONT_TRIM_DEB, share, fill = CIBLE, group = CIBLE))+
  geom_col(position = "dodge")


share_table %>% 
  filter(CIBLE == 1) %>% 
  View()


share_table <- toutes_tables %>%
  filter(SAL_SITAVTCONT == 3) %>% 
  mutate(CONT_ANNDEB = year(CONT_DATEDEB)) %>% 
  group_by(CONT_ANNDEB, CONT_TRIM_DEB)%>% 
  summarise(n=n()) %>%
  group_by(CONT_ANNDEB) %>% 
  mutate(share = n*100 / sum(n)) %>% 
  View()

toutes_tables %>% 
  group_by(SAL_SITAVTCONT) %>% 
  summarise(n = n())


toutes_tables %>% 
  group_by(SAL_SITAVTCONT) %>% 
  summarise(mean_age = mean(SAL_AGEDEBCONT))


# Quel niveau de diplômes à l'embauche ? 

