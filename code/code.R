library(tidyverse)
library(haven)
library(janitor)
library(estimatr)
library(collapse)

setwd("C:/Users/marti/OneDrive/Plocha/research_projects/nrega_and_deforestation")

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

district_keys <- district_keys %>% 
  filter(ec05_district_name != '') %>% 
  left_join(dist_phases, by = c('ec05_state_name' = 'state_name', 'ec05_district_name' = 'district_name')) %>% 
  filter(!is.na(phase))
  


village_keys <- read_stata('data/shrug-v1.5.samosa-keys-dta/shrug_ec05_district_key.dta')

village_keys <- village_keys %>% 
  dplyr::select(shrid, ec05_state_id, ec05_district_id) %>% 
  left_join(district_keys, by = c('ec05_state_id', 'ec05_district_id')) %>% 
  filter(!is.na(phase))


district_keys %>% 
  filter(ec05_district_name != '') %>% 
  left_join(dist_phases, by = c('ec05_state_name' = 'state_name', 'ec05_district_name' = 'district_name')) %>% 
  filter(is.na(phase)) %>% 
  arrange(ec05_state_name, ec05_district_name) %>% 
  View()


dist_phases %>% 
  left_join(district_keys, by = c('state_name' = 'ec05_state_name', 'district_name' = 'ec05_district_name')) %>% 
  filter(is.na(ec05_state_id)) %>% View('dists')

district_keys_2 %>% 
  left_join(dist_phases, by = c('ec13_state_name' = 'state_name', 'ec13_district_name' = 'district_name')) %>% 
  filter(is.na(phase))




forest_cover <- read_stata('data/shrug-v1.5.samosa-vcf-dta/shrug_vcf_wide.dta') 


main_data <- forest_cover %>% 
  dplyr::select(shrid, num_cells, starts_with('total_forest')) %>% 
  pivot_longer(-c(shrid, num_cells), names_to = 'year', values_to = 'total_forest') %>% 
  mutate(year = str_sub(year, start = 13) %>% as.numeric())

main_data <- main_data %>% 
  left_join(village_keys, by = 'shrid') %>% 
  filter(!is.na(phase))


main_data <- main_data %>% 
  arrange(shrid, year) %>% 
  mutate(
    treatment = case_when(
      phase == 'phase_1' & year >= 2006 ~ 1,
      phase == 'phase_2' & year >= 2007 ~ 1,
      year >= 2008 ~ 1,
      TRUE ~ 0),
    year_factor = as.factor(year),
    state_district = str_c(ec05_state_id, ec05_district_id, sep = '-'),
    log_total_forest = log(1 + total_forest),
    avg_forest = total_forest/num_cells
    )


treatment_dummy_shift <- function(phase, year, lag){
  output <- case_when(
    phase == 'phase_1' & year == 2006 + lag  ~ 1,
    phase == 'phase_2' & year == 2007 + lag ~ 1,
    phase == 'phase_3' & year == 2008 + lag ~ 1,
    TRUE ~ 0)
  return(output)
}


main_data  <- main_data %>% 
  mutate(
    treat_dummy = treatment_dummy_shift(phase, year, 0),
    treat_lead_1 = treatment_dummy_shift(phase, year, -1),
    treat_lead_2 = treatment_dummy_shift(phase, year, -2),
    treat_lead_3 = treatment_dummy_shift(phase, year, -3),
    treat_lead_4 = treatment_dummy_shift(phase, year, -4),
    treat_lead_5 = treatment_dummy_shift(phase, year, -5),
    treat_lag_1 = treatment_dummy_shift(phase, year, 1)
    )

main_data  <- main_data %>% 
  #group_by(shrid, year) %>% 
  mutate(treat_lead_1 = dplyr::lead(treatment, 1),
         ifelse(is.na(treat_lead_1), 1, treat_lead_1),
         treat_lead_2 = dplyr::lead(treatment, 2),
         ifelse(is.na(treat_lead_2), 1, treat_lead_2)) %>% 
  ungroup()

settransform(main_data, treat_long_term_lag_1 = flag(treatment, 1, shrid, year))
settransform(main_data, treat_long_term_lag_1 = ifelse(is.na(treat_long_term_lag_1), 0, treat_long_term_lag_1))

settransform(main_data, treat_lag_2 = flag(treatment, 2, shrid, year))

settransform(main_data, treat_lead_1 = flag(treatment, -1, shrid, year))
settransform(main_data, treat_lead_1 = flag(treatment, -1, shrid, year))




basic_spec <- lm_robust(log_total_forest ~ treatment, fixed_effects = ~ year_factor + state_district,
                        clusters = state_district, data = main_data)


library(fixest)

basic_spec <- feols(log_total_forest ~ treatment|state_district + year_factor, main_data)

avg_forest_spec <- feols(avg_forest ~ treatment|state_district + year_factor, main_data)

dynamic_spec <- feols(log_total_forest ~ treat_dummy + treat_lead_1 + treat_lead_2
                       + treat_lead_3 + treat_long_term_lag_1|state_district + year_factor, main_data)



test_pretrends_spec <- feols(log_total_forest ~ treat_lead_1 + treat_lead_2
                      + treat_lead_3 + treat_lead_4|state_district + year_factor, main_data %>% filter(treatment == 0))


fitstat(test_pretrends_spec,
        ~ f)


test_pretrends_spec$ssr_null
test_pretrends_spec$ssr_fe_only
test_pretrends_spec$sigma2

1- sum(test_pretrends_spec$residuals^2)/test_pretrends_spec$ssr_null 

r_sq <- 1- ( sum(test_pretrends_spec$residuals^2))/test_pretrends_spec$ssr_fe_only 

N <- test_pretrends_spec$nobs
N <- 620
p <- test_pretrends_spec$nparams - sum(test_pretrends_spec$fixef_sizes)

F_stat <- (N-p)/(p-1) * 1/(1-r_sq)

pf(F_stat, df1 = N, df2 = p)
pf(F_stat, df2 = N, df1 = p)



# cluster FE by organization
#event_reg_lenient_summary <- summary(event_reg_lenient, cluster = ~ro_INN)

