library(tidyverse)
library(haven)
library(janitor)

setwd("C:/Users/marti/OneDrive/Plocha/research_projects/nrega_and_deforest")

library(readxl)
dist_phases <- read_excel('data/MNREGA_district_phases.xlsx') %>% 
  clean_names() %>% 
  dplyr::select(state_name, phase_1 = district_name_in_phase_i, 
                phase_2 = district_name_in_phase_ii, phase_3 = district_name_in_phase_iii) %>% 
  pivot_longer(-state_name, values_to = 'district_name', names_to = 'phase') %>% 
  filter(!is.na(district_name)) %>% 
  mutate(state_name = str_to_lower(state_name),
         district_name = str_to_lower(district_name))

dist_phases$district_name


district_keys <- read_stata('data/shrug-v1.5.samosa-keys-dta/shrug_ec05_district_key.dta') %>% 
  distinct(ec05_state_id, ec05_state_name, ec05_district_id, ec05_district_name)

district_keys_2 <- read_stata('data/shrug-v1.5.samosa-keys-dta/shrug_ec13_district_key.dta') %>% 
  distinct(ec13_state_id, ec13_state_name, ec13_district_id, ec13_district_name)



district_keys %>% 
  filter(ec05_district_name != '') %>% 
  left_join(dist_phases, by = c('ec05_state_name' = 'state_name', 'ec05_district_name' = 'district_name')) %>% 
  filter(is.na(phase)) %>% 
  arrange(ec05_state_name, ec05_district_name) %>% 
  View()


dist_phases %>% 
  left_join(district_keys, by = c('state_name' = 'ec05_state_name', 'district_name' = 'ec05_district_name')) %>% 
  filter(is.na(ec05_state_id)) 



district_keys_2 %>% 
  left_join(dist_phases, by = c('ec13_state_name' = 'state_name', 'ec13_district_name' = 'district_name')) %>% 
  filter(is.na(phase))
