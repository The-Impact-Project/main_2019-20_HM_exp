library(here)
library(civis)
library(tidyverse)
library(causalToolbox)

raw_pennsylvania_dat <- readRDS(here("output", "raw_pennsylvania_dat.Rds"))
raw_florida_dat <- readRDS(here("output", "raw_florida_dat.Rds"))
raw_michigan_dat <- readRDS(here("output", "raw_michigan_dat.Rds"))
raw_arizona_dat <- readRDS(here("output", "raw_arizona_dat.Rds"))
raw_maine_dat <- readRDS(here("output", "raw_maine_dat.Rds"))
raw_nevada_dat <- readRDS(here("output", "raw_nevada_dat.Rds"))
raw_northcarolina_dat <- readRDS(here("output", "raw_northcarolina_dat.Rds"))
raw_colorado_dat <- readRDS(here("output", "raw_colorado_dat.Rds"))



full_model <- readRDS(here("output", "full_cate_model.Rds"))

score_df_cates <- function(df, model) {
  processed_dat <- df %>% 
    mutate(threeway_party = if_else(vb_vf_party == "Democrat", "Democrat",
                                    if_else(vb_vf_party=="Republican", "Republican", "Other")),
           threeway_race = fct_recode(vb_voterbase_race,
                                      Other = "African-American",
                                      Other = "Asian",
                                      Other = "Native American",
                                      Other = "Uncoded")) %>%
    mutate(mixed_party_hh = length(unique(unlist(strsplit(toString(enh_tsmart_enhanced_party_code), split = '')))) > 1) %>%
    mutate(mixed_party_hh = if_else(mixed_party_hh==TRUE, "mixed", "not_mixed")) %>%
    mutate(years_since_reg = as.numeric(lubridate::interval(lubridate::ymd(vb_vf_earliest_registration_date), lubridate::ymd("20191224")))) %>%
    select(vb_voterbase_id,
           threeway_party,
           threeway_race,
           mixed_party_hh,
           college,
           vb_voterbase_gender,
           vb_voterbase_age,
           ts_tsmart_urbanicity,
           years_since_reg,
           ts_tsmart_presidential_general_turnout_score,
           ts_tsmart_partisan_score,
           ts_tsmart_teaparty_score,
           ts_tsmart_evangelical_raw_score,
           ts_tsmart_catholic_raw_score,
           ts_tsmart_otherchristian_raw_score,
           ts_tsmart_ideology_score,
           ts_tsmart_evangelical_raw_score,
           ts_tsmart_otherchristian_raw_score,
           ts_tsmart_nonchristian_raw_score,
           ts_tsmart_prochoice_score,
           ts_tsmart_path_to_citizen_score,
           ts_tsmart_college_funding_score,
           ts_tsmart_climate_change_score,
           ts_tsmart_gun_control_score,
           ts_tsmart_paid_leave_score,
           ts_tsmart_minimum_wage_score,
           ts_tsmart_govt_privacy_score,
           ts_tsmart_campaign_finance_score,
           ts_tsmart_tax_on_wealthy_score,
           ts_tsmart_working_class_score,
           ts_tsmart_activist_score,
           ts_tsmart_trump_resistance_score,
           ts_tsmart_trump_support_score,
           ts_tsmart_gunowner_score,
           ts_tsmart_veteran_score,
           predictwise_authoritarianism_score,
           predictwise_compassion_score,
           predictwise_economic_populism_score,
           predictwise_environmentalism_score,
           predictwise_free_trade_score,
           predictwise_globalism_score,
           predictwise_guns_score,
           predictwise_healthcare_women_score,
           predictwise_healthcare_score,
           predictwise_immigrants_score,
           predictwise_military_score,
           predictwise_populism_score,
           predictwise_poor_score,
           predictwise_presidential_score,
           predictwise_racial_resentment_score,
           predictwise_regulation_score,
           predictwise_religious_freedom_score,
           predictwise_taxes_score,
           predictwise_traditionalism_score,
           predictwise_trust_in_institutions_score,
           vb_personal_voice_social_networker_demi_decile,
           vb_professional_social_networker_demi_decile,
           vb_purely_social_networker_demi_decile,
           vb_social_networker_demi_decile,
           enh_tsmart_enhanced_hh_size,
           enh_tsmart_enhanced_hh_num_males,
           enh_tsmart_enhanced_hh_num_females,
           enh_tsmart_enhanced_hh_num_registered,
           enh_tsmart_enhanced_hh_num_unregistered,
           enh_tsmart_enhanced_hh_num_dems,
           enh_tsmart_enhanced_hh_num_reps,
           enh_tsmart_enhanced_hh_num_others,
           nm_score
    ) %>%
    mutate(nm_score = if_else(is.na(nm_score), mean(.$nm_score, na.rm=T), nm_score),
           dem = 0) %>%
    filter(complete.cases(.)) %>%
    select(dem, everything()) %>%
    mutate(threeway_party = as_factor(threeway_party),
           mixed_party_hh = as.factor(mixed_party_hh))
  
  processed_dat$cate <- processed_dat %>%
    select(-vb_voterbase_id) %>%
    EstimateCate(theObject = mod)
  
  processed_dat %>%
    as_tibble() %>%
    return()
}

tictoc::tic()
pa_cates <- score_df_cates(df = raw_pennsylvania_dat, model = full_model) %>%
  mutate(state = "PA")
tictoc::toc()

tictoc::tic()
fl_cates <- score_df_cates(df = raw_florida_dat, model = full_model) %>%
  mutate(state = "FL")
tictoc::toc()

tictoc::tic()
mi_cates <- score_df_cates(df = raw_michigan_dat, model = full_model) %>%
  mutate(state = "MI")
tictoc::toc()

tictoc::tic()
az_cates <- score_df_cates(df = raw_arizona_dat, model = full_model) %>%
  mutate(state = "AZ")
tictoc::toc()

tictoc::tic()
me_cates <- score_df_cates(df = raw_maine_dat, model = full_model) %>%
  mutate(state = "ME")
tictoc::toc()

tictoc::tic()
nv_cates <- score_df_cates(df = raw_nevada_dat, model = full_model) %>%
  mutate(state = "NV")
tictoc::toc()

tictoc::tic()
co_cates <- score_df_cates(df = raw_colorado_dat, model = full_model) %>%
  mutate(state = "CO")
tictoc::toc()

tictoc::tic()
nc_cates <- score_df_cates(df = raw_northcarolina_dat, model = full_model) %>%
  mutate(state = "NC")
tictoc::toc()

vf_with_cates <- bind_rows(pa_cates, 
                           fl_cates,
                           mi_cates,
                           az_cates,
                           me_cates,
                           nv_cates,
                           co_cates,
                           nc_cates)

saveRDS(object = vf_with_cates, file = here("output", "vf_with_cates.Rds"))

vf_with_cates %>%
  ggplot(aes(x=cate, fill = state)) +
  geom_density(alpha=.3)

