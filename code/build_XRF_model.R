
# Load Packages -----------------------------------------------------------
library(TIPtools)
library(tidyverse)
library(here)
library(causalToolbox)
library(civis)
library(causalToolbox)

# Load Modeling Data Frame ------------------------------------------------
mod_df <- readRDS(here("output", "model_df.Rds"))

mod_df <- filter(mod_df, dem==0)

set.seed(32)

mod_df <- mod_df[sample(1:nrow(mod_df)), ]

# Set Up Cross Validation -------------------------------------------------
n <- nrow(mod_df)
folds <- 6
fold_size <- floor(n / folds)
mod_df <- mod_df[1:(folds*fold_size),]

mod_df$fold <- rep(1:folds, fold_size)
mod_df$cate <- NA

# Run Models --------------------------------------------------------------
non_feature_vars <- c("assignment", 
                      "Qfav_flipped_continous", 
                      "Qfav", 
                      "fold", 
                      "cate", 
                      "ts_score",
                      "vb_voterbase_id",
                      "weight")

for (this_fold in 1:folds) {
  
  print(this_fold)
  print(Sys.time())
  
  fold_df <- mod_df %>%
    filter(fold != this_fold)
  
  features <- fold_df %>%
    select(-non_feature_vars)
  w <- if_else(fold_df$assignment == "control", 0, 1)
  yobs <- if_else(grepl(pattern = "Unfav", x = fold_df$Qfav), 1, 0)
  
  tictoc::tic()
  mod <- X_RF(feat = features, tr = w, yobs = yobs)
  tictoc::toc()
  
  cates <- mod_df %>%
    filter(fold == this_fold) %>%
    select(-non_feature_vars) %>%
    EstimateCate(theObject = mod)
  
  mod_df$cate[mod_df$fold == this_fold] <- cates
  
  BRRR::skrrrahh(23)
}

saveRDS(object = mod_df, file = here("output", "df_with_cates.Rds"))


# Print Table of Cate Outputs ---------------------------------------------
mod_df %>%
  mutate(cate_cat = cut(cate, 
                        breaks = quantile(cate, probs = c(0,.2, .8, 1)),
                        labels = c("low", "mid", "high"),
                        include.lowest = T)) %>%
  filter(cate_cat == "low") %>%
  select(Qfav_flipped_continous, assignment) %>%
  table() %>%
  prop.table(margin=2) %>%
  round(digits = 3)

# Build Overall Model -----------------------------------------------------


full_features <- mod_df %>%
  select(-non_feature_vars)
full_w <- if_else(mod_df$assignment == "control", 0, 1)
full_yobs <- if_else(grepl(pattern = "Unfav", x = mod_df$Qfav), 1, 0)

set.seed(32)
tictoc::tic()
full_mod <- X_RF(feat = full_features, tr = full_w, yobs = full_yobs)
tictoc::toc()

# for some reason, re-opening this does not seem to work, but I'm saving it anyway
saveRDS(object = full_mod, file = here("output", "full_cate_model.Rds"))


# Get Target Smart Data from Civis ----------------------------------------
query <- sql("SELECT 
  vb_voterbase_id,
  vb_vf_party,
  vb_voterbase_race,
  vb_education,
  vb_voterbase_gender,
  vb_voterbase_age,
  ts_tsmart_urbanicity,
  vb_vf_earliest_registration_date,
  ts_tsmart_presidential_general_turnout_score,
  ts_tsmart_partisan_score,
  ts_tsmart_teaparty_score,
  ts_tsmart_evangelical_raw_score,
  ts_tsmart_catholic_raw_score,
  ts_tsmart_otherchristian_raw_score,
  ts_tsmart_ideology_score,
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
  enh_tsmart_enhanced_party_code,
  nm_score
FROM ts.ntl_current
	LEFT JOIN impactproject.segment_scores USING (vb_voterbase_id)
WHERE ((vb_tsmart_state = 'AZ' AND vb_tsmart_hd IN ('006', '017', '020', '027', '028', '029'))
      OR (vb_tsmart_state = 'CO' AND vb_tsmart_sd IN ('016', '019', '025', '026'))
      OR (vb_tsmart_state = 'FL' AND vb_tsmart_hd IN ('058', '060', '065', '067', '115', '116', '118', '119'))
      OR (vb_tsmart_state = 'ME' AND vb_tsmart_sd IN ('013', '014', '015', '016'))
      OR (vb_tsmart_state = 'MI' AND vb_tsmart_hd IN ('019', '035', '039', '042', '071', '110'))
      OR (vb_tsmart_state = 'NV' AND ( vb_tsmart_hd IN ('004', '027', '029', '037') OR vb_tsmart_sd IN ('006', '008', '009')))
      OR (vb_tsmart_state = 'PA' AND vb_tsmart_hd IN ('029', '094', '100', '105', '131', '144', '168', '178')))
  AND vb_voterbase_age > 17
  AND vb_voterbase_age < 100
  AND vb_voterbase_deceased_flag IS NULL
  AND vb_voterbase_registration_status = 'Registered';")

raw_vf <- read_civis(query, database = "TMC") %>%
  as_tibble()

# Process Raw TargetSmart Data --------------------------------------------
processed_vf <- raw_vf %>%
  mutate(dem=0) %>%
  mutate(mixed_party_hh = length(unique(unlist(strsplit(toString(enh_tsmart_enhanced_party_code), split = '')))) > 1) %>%
  mutate(mixed_party_hh = if_else(mixed_party_hh==TRUE, "mixed", "not_mixed")) %>%
  mutate(years_since_reg = as.numeric(lubridate::interval(lubridate::ymd(vb_vf_earliest_registration_date), lubridate::ymd("20191224")))) %>%
  mutate(threeway_party = if_else(vb_vf_party == "Democrat", "Democrat", 
                                  if_else(vb_vf_party=="Republican", "Republican", "Other")),
         threeway_race = fct_recode(vb_voterbase_race,
                                    Other = "African-American",
                                    Other = "Asian",
                                    Other = "Native American",
                                    Other = "Uncoded")) %>%
  mutate(college = if_else(vb_education %in% c(3,4), 1, 0)) %>%
  mutate(threeway_party = as_factor(threeway_party),
         mixed_party_hh = as.factor(mixed_party_hh)) %>%
  select(dem,
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
         nm_score,
         vb_voterbase_id)

# impute mean
processed_vf$ts_tsmart_tax_on_wealthy_score[is.na(processed_vf$ts_tsmart_tax_on_wealthy_score)] <- mean(processed_vf$ts_tsmart_tax_on_wealthy_score, na.rm=T)
processed_vf$ts_tsmart_evangelical_raw_score[is.na(processed_vf$ts_tsmart_evangelical_raw_score)] <- mean(processed_vf$ts_tsmart_evangelical_raw_score, na.rm=T)
processed_vf$ts_tsmart_catholic_raw_score[is.na(processed_vf$ts_tsmart_catholic_raw_score)] <- mean(processed_vf$ts_tsmart_catholic_raw_score, na.rm=T)
processed_vf$ts_tsmart_nonchristian_raw_score[is.na(processed_vf$ts_tsmart_nonchristian_raw_score)] <- mean(processed_vf$ts_tsmart_nonchristian_raw_score, na.rm=T)
processed_vf$ts_tsmart_minimum_wage_score[is.na(processed_vf$ts_tsmart_minimum_wage_score)] <- mean(processed_vf$ts_tsmart_minimum_wage_score, na.rm=T)
processed_vf$ts_tsmart_paid_leave_score[is.na(processed_vf$ts_tsmart_paid_leave_score)] <- mean(processed_vf$ts_tsmart_paid_leave_score, na.rm=T)
processed_vf$vb_personal_voice_social_networker_demi_decile[is.na(processed_vf$vb_personal_voice_social_networker_demi_decile)] <- mean(processed_vf$vb_personal_voice_social_networker_demi_decile, na.rm=T)
processed_vf$ts_tsmart_otherchristian_raw_score[is.na(processed_vf$ts_tsmart_otherchristian_raw_score)] <- mean(processed_vf$ts_tsmart_otherchristian_raw_score, na.rm=T)
processed_vf$vb_professional_social_networker_demi_decile[is.na(processed_vf$vb_professional_social_networker_demi_decile)] <- mean(processed_vf$vb_professional_social_networker_demi_decile, na.rm=T)
processed_vf$vb_purely_social_networker_demi_decile[is.na(processed_vf$vb_purely_social_networker_demi_decile)] <- mean(processed_vf$vb_purely_social_networker_demi_decile, na.rm=T)
processed_vf$vb_social_networker_demi_decile[is.na(processed_vf$vb_social_networker_demi_decile)] <- mean(processed_vf$vb_social_networker_demi_decile, na.rm=T)
processed_vf$nm_score[is.na(processed_vf$nm_score)] <- mean(processed_vf$nm_score, na.rm=T)
processed_vf$predictwise_presidential_score[is.na(processed_vf$predictwise_presidential_score)] <- mean(processed_vf$predictwise_presidential_score, na.rm=T)
processed_vf$ts_tsmart_college_funding_score[is.na(processed_vf$ts_tsmart_college_funding_score)] <- mean(processed_vf$ts_tsmart_college_funding_score, na.rm=T)
processed_vf$ts_tsmart_govt_privacy_score[is.na(processed_vf$ts_tsmart_govt_privacy_score)] <- mean(processed_vf$ts_tsmart_govt_privacy_score, na.rm=T)
processed_vf$ts_tsmart_campaign_finance_score[is.na(processed_vf$ts_tsmart_campaign_finance_score)] <- mean(processed_vf$ts_tsmart_campaign_finance_score, na.rm=T)

# remove rows that still have missing data
processed_vf <- filter(processed_vf, complete.cases(processed_vf))

# Calculate Scores --------------------------------------------------------
cates <- EstimateCate(theObject = full_mod, feature_new = select(processed_vf, -vb_voterbase_id))
vf_with_cates <- processed_vf
vf_with_cates$cate <- cates


# Save cates DF -----------------------------------------------------------

vf_with_cates %>%
  select(vb_voterbase_id, cate) %>% 
  write_csv(here("output", "cates_for_upload.csv"))

