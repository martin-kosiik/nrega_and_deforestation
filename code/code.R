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



district_keys <- read_stata('data/shrug-v1.5.samosa-keys-dta/shrug_ec05_district_key.dta') %>% 
  distinct(ec05_state_id, ec05_state_name, ec05_district_id, ec05_district_name)

#district_keys_2 <- read_stata('data/shrug-v1.5.samosa-keys-dta/shrug_ec13_district_key.dta') %>% 
#  distinct(ec13_state_id, ec13_state_name, ec13_district_id, ec13_district_name)

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
    treat_lead_6 = treatment_dummy_shift(phase, year, -6),
    treat_lag_1 = treatment_dummy_shift(phase, year, 1)
    )


median_log_total_forest <- main_data %>% 
  filter(year == 2000) %>% 
  summarise(median_log_total_forest = median(log_total_forest)) %>% 
  pull(median_log_total_forest)

main_data  <- main_data %>% 
  group_by(shrid) %>% 
  mutate(log_total_forest_2000 = log_total_forest[[1]]) %>% # the obs. are order by shrid and year
  ungroup() %>%                                             # so if I take the first element I will take data
  mutate(high_forest = (log_total_forest_2000 >= median_log_total_forest)*1)                                                        # from the year 2000

#main_data  <- main_data %>% 
  #group_by(shrid, year) %>% 
#  mutate(treat_lead_1 = dplyr::lead(treatment, 1),
#         ifelse(is.na(treat_lead_1), 1, treat_lead_1),
#         treat_lead_2 = dplyr::lead(treatment, 2),
#         ifelse(is.na(treat_lead_2), 1, treat_lead_2)) %>% 
#  ungroup()

settransform(main_data, treat_long_term_lag_1 = flag(treatment, 1, shrid, year))
settransform(main_data, treat_long_term_lag_1 = ifelse(is.na(treat_long_term_lag_1), 0, treat_long_term_lag_1))

#settransform(main_data, treat_lag_2 = flag(treatment, 2, shrid, year))

#settransform(main_data, treat_lead_1 = flag(treatment, -1, shrid, year))
#settransform(main_data, treat_lead_1 = flag(treatment, -1, shrid, year))



basic_spec <- lm_robust(log_total_forest ~ treatment, fixed_effects = ~ year_factor + state_district,
                        clusters = state_district, data = main_data)


library(fixest)

basic_spec <- feols(log_total_forest ~ treatment|shrid + year_factor, cluster = ~state_district, main_data)

village_spec_time_trends <- feols(log_total_forest ~ treatment|shrid + year_factor + shrid[year], cluster = ~state_district, main_data)


het_by_baseline_forest <- feols(log_total_forest ~ treatment + log_total_forest_2000:treatment|shrid + year_factor,
                                cluster = ~state_district, main_data %>% filter(year != 2000))

het_by_high_forest <- feols(log_total_forest ~ treatment + high_forest:treatment|shrid + year_factor,
                                cluster = ~state_district, main_data %>% filter(year != 2000))



avg_forest_spec <- feols(avg_forest ~ treatment|shrid + year_factor, cluster = ~state_district, main_data)

#main_effect_two_way_clust <- 
mean(main_data$log_total_forest_2000) * (-0.086754)
median(main_data$log_total_forest_2000) * (-0.086754)

  
basic_spec <- feols(log_total_forest ~ treatment|shrid + year_factor, cluster = ~state_district,
                    main_data %>% filter(year <= 2008))



dynamic_spec <- feols(log_total_forest ~ treat_dummy + treat_lead_1 + treat_lead_2
                       + treat_lead_3 + treat_long_term_lag_1|state_district + year_factor, main_data)





# Pre-trends
test_pretrends_spec_2 <- feols(log_total_forest ~ treat_lead_1 + treat_lead_2
                      + treat_lead_3 + treat_lead_4 + treat_lead_5|shrid + year_factor,
                      cluster = ~state_district, main_data %>% filter(treatment == 0))

test_pretrends_spec_3 <- feols(log_total_forest ~ treat_lead_1 + treat_lead_2
                             + treat_lead_3 + treat_lead_4|shrid + year_factor,
                             cluster = ~state_district, main_data %>% filter(treatment == 0))

test_pretrends_spec <- feols(log_total_forest ~ treat_lead_1 + treat_lead_2
                               + treat_lead_3 + treat_lead_4 + treat_lead_5 + treat_lead_6|shrid + year_factor,
                               cluster = ~state_district, main_data %>% filter(treatment == 0))


test_pretrends_spec_avg <- feols(avg_forest ~ treat_lead_1 + treat_lead_2
                             + treat_lead_3 + treat_lead_4 + treat_lead_5 + treat_lead_6|shrid + year_factor,
                             cluster = ~state_district, main_data %>% filter(treatment == 0))



high_forest_pretrends_formula <- as.formula(str_c('log_total_forest ~', str_c('treat_lead_', 1:5, collapse = '+'),
                                                  '+', str_c('treat_lead_', 1:5, ':high_forest', collapse = '+'),
                                                  '|shrid + year_factor'))

test_pretrends_spec_high_forest <- feols(high_forest_pretrends_formula,
                                         cluster = ~state_district, main_data %>% filter(treatment == 0))



fitstat(test_pretrends_spec,~ f)
fitstat(test_pretrends_spec,~ wf)

df2 = x$nobs - x$nparams
stat = ((x$ssr_fe_only - x$ssr) / df1) / (x$ssr / df2)
p = pf(stat, df1, df2, lower.tail = FALSE)
vec = list(stat = stat, p = p, df1 = df1, df2 = df2)
res_all[[type]] = set_value(vec, value)



ssr_unr <- sum(test_pretrends_spec$residuals^2)
ssr_r <- test_pretrends_spec$ssr_fe_only
p <- test_pretrends_spec$nparams - sum(test_pretrends_spec$fixef_sizes) + 1

test_pretrends_spec$nparams
n_clusters <- test_pretrends_spec$fixef_sizes[['state_district']]

sum(test_pretrends_spec$residuals^2)/test_pretrends_spec$ssr_null 
all_params <- test_pretrends_spec$nparams


F_stat <- (N-all_params)/(p) * (ssr_r - ssr_unr)/ssr_unr
F_stat <- (n_clusters)/(p) * (ssr_r - ssr_unr)/ssr_unr

p_val = pf(F_stat, df1 = p, df2 = n_clusters, lower.tail = FALSE)


test_pretrends_spec$ssr_null
test_pretrends_spec$ssr_fe_only
test_pretrends_spec$sigma2

1- sum(test_pretrends_spec$residuals^2)/test_pretrends_spec$ssr_null 

r_sq <- 1- ( sum(test_pretrends_spec$residuals^2))/test_pretrends_spec$ssr_fe_only 

N <- test_pretrends_spec$nobs
N <- 620
p <- test_pretrends_spec$nparams - sum(test_pretrends_spec$fixef_sizes) + 1

F_stat <- (N-p)/(p-1) * 1/(1-r_sq)

pf(F_stat, df1 = N, df2 = p)
pf(F_stat, df2 = N, df1 = p)


library(stargazer)
stargazer(test_pretrends_spec)

table_dict = c(log_total_forest = 'Log of total forest cover', shrid = 'Village',
               state_dist = 'District', year_factor = "Year",
               treat_lead_1 = '$\\mathds{1}\\left\\{t = E_{d} +1\\right\\}$',
               treat_lead_2 = '$\\mathds{1}\\left\\{t = E_{d} +2\\right\\}$',
               treat_lead_3 = '$\\mathds{1}\\left\\{t = E_{d} +3\\right\\}$',
               treat_lead_4 = '$\\mathds{1}\\left\\{t = E_{d} +4\\right\\}$',
               treat_lead_5 = '$\\mathds{1}\\left\\{t = E_{d} +5\\right\\}$',
               treat_lead_6 = '$\\mathds{1}\\left\\{t = E_{d} +6\\right\\}$'
               )


etable(test_pretrends_spec, test_pretrends_spec_2, test_pretrends_spec_3, file = "tables/pre_trends_test.tex", 
       replace = TRUE, title = "Pre-trends regressions", label = 'tab:pre-trends_tests', 
       style.tex = style.tex("base"), dict = table_dict,
       fixef_sizes = TRUE, notes = c('The standard errors in parentheses are clustered on a district level.'))


etable(test_pretrends_spec, test_pretrends_spec_2, title = "Pre-trends")


library(broom)
pre_trends_coef_plot <- tidy(test_pretrends_spec_2, conf.int = TRUE) %>% 
  mutate(term = str_sub(term, start = -1),
         term = as.numeric(term) * (-1)) %>% 
  ggplot(mapping = aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, group = 1))+ 
  #geom_vline(xintercept= 1932.5, col = "red", linetype = "dashed", size = 1)+
  geom_pointrange()+  geom_hline(yintercept= 0) +
  theme_minimal() + 
  theme(axis.line = element_line(size = 1), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        text = element_text(size=14),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(x = "Lead of the dummy for NREGA introduction (number of years)", 
       #caption = "error bars show 95% confidence intervals \n 
       #                      SE are based on the cluster-robust estimator by Pustejovsky and Tipton (2018)", 
       y = "Coefficient") 

pre_trends_coef_plot
ggsave('figures/pre_trends_coef_plot.pdf')

