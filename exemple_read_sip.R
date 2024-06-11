### Statistiques descriptives sur le recours au contrat de professionnalisation par classe d'âges ###

library('dplyr')
library('tidyr')
library('lubridate')

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
  mutate(CIBLE = ifelse(SAL_SITAVTCONT == 9 & SAL_AGEDEBCONT >= 26, 1,0),
         CONT_TRIM_DEB = quarter(CONT_DATEDEB, type = "year.quarter"),
         CONT_TRIM_FIN = quarter(CONT_DATEFINEFF_POEM, type = "year.quarter"),
         CONT_DUREFF = ceiling(difftime(CONT_DATEDEB, CONT_DATEFINEFF_POEM, units = "weeks")))

# Comment évoluent leurs entrées par rapport aux autres ? 

toutes_tables %>%
  mutate(duree = CONT_DATEFINEFF_POEM - CONT_DATEDEB) %>% 
  filter(duree < month(6))
  view()

filter(CONT_DATEDEB > as.Date('2021-12-31') & CONT_DATEFINEFF_POEM <= as.Date('2022-12-31')) %>%
group_by(SAL_SITAVTCONT, JEUNE) %>%
summarise(n = n()) %>% 
group_by(SAL_SITAVTCONT) %>% 
mutate(share = n / sum(n)) %>% 
View()


# Quel niveau de diplômes à l'embauche ? 

