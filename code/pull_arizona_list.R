
# Load Packages -----------------------------------------------------------
library(TIPtools)
library(tidyverse)
library(civis)
library(here)
library(purrr)

raw_az_dat <- "SELECT 
  vb_voterbase_id,
  vb_voterid,
  vb_tsmart_first_name,
  vb_tsmart_middle_name,
  vb_tsmart_last_name,
  vb_tsmart_name_suffix,
  vb_tsmart_full_address,
  vb_tsmart_city,
  vb_tsmart_state,
  vb_tsmart_zip,
  vb_tsmart_zip4,
  vb_tsmart_street_number,
  vb_tsmart_pre_directional,
  vb_tsmart_street_name,
  vb_tsmart_street_suffix,
  vb_tsmart_post_directional,
  vb_tsmart_unit_designator,
  vb_tsmart_secondary_number,
  vb_tsmart_effective_date,
  vb_tsmart_address_usps_address_code,
  vb_voterbase_phone,
  vb_voterbase_phone_type,
  vb_voterbase_phone_wireless,
  vb_voterbase_registration_status,
  vb_voterbase_dob,
  vb_voterbase_age,
  vb_tsmart_dob,
  vb_voterbase_gender,
  vb_voterbase_race,
  vb_voterbase_marital_status,
  vb_vf_voter_status,
  vb_vf_registration_date,
  vb_vf_earliest_registration_date,
  vb_vf_party,
  vb_tsmart_county_name,
  vb_tsmart_cd,
  vb_tsmart_sd,
  vb_tsmart_hd,
  vb_tsmart_address_deliverability_indicator,
  vb_voterbase_mailable_flag,
  vb_voterbase_general_votes,
  vb_voterbase_primary_votes,
  vb_voterbase_voter_score,
  vb_vf_g2020,
  vb_vf_g2019,
  vb_vf_g2018,
  vb_vf_g2017,
  vb_vf_g2016,
  vb_vf_g2015,
  vb_vf_g2014,
  vb_vf_g2013,
  vb_vf_g2012,
  vb_vf_g2011,
  vb_vf_g2010,
  vb_family_composition_code,
  vb_number_of_adults_in_houshold,
  vb_presence_of_children_in_household,
  vb_number_of_children_in_household,
  vb_homeowner_indicator,
  vb_household_income_amount,
  vb_household_income_range,
  vb_household_net_worth,
  vb_education,
  vb_personal_voice_social_networker_demi_decile,
  vb_professional_social_networker_demi_decile,
  vb_purely_social_networker_demi_decile,
  vb_social_networker_demi_decile,
  vb_license_hunting,
  vb_license_fishing,
  enh_tsmart_enhanced_hh_size,
  enh_tsmart_enhanced_hh_code,
  enh_tsmart_enhanced_reg_code,
  enh_tsmart_enhanced_party_code,
  enh_tsmart_enhanced_hh_num_males,
  enh_tsmart_enhanced_hh_num_females,
  enh_tsmart_enhanced_hh_num_registered,
  enh_tsmart_enhanced_hh_num_unregistered,
  enh_tsmart_enhanced_hh_num_dems,
  enh_tsmart_enhanced_hh_num_reps,
  enh_tsmart_enhanced_hh_num_others,
  cell_tsmart_wireless_phone,
  cell_tsmart_wireless_confidence_score,
  cell_tsmart_wireless_source,
  cell_tsmart_wireless_multisource_count,
  cell_tsmart_match_level,
  cell_tsmart_address_level_match,
  ts_tsmart_partisan_score,
  ts_tsmart_presidential_general_turnout_score,
  ts_tsmart_midterm_general_turnout_score,
  ts_tsmart_midterm_general_enthusiasm_score,
  ts_tsmart_offyear_general_turnout_score,
  ts_tsmart_presidential_primary_turnout_score,
  ts_tsmart_non_presidential_primary_turnout_score,
  ts_tsmart_local_voter_score,
  ts_tsmart_teaparty_score,
  ts_tsmart_ideology_score,
  ts_tsmart_moral_authority_score,
  ts_tsmart_moral_care_score,
  ts_tsmart_moral_equality_score,
  ts_tsmart_moral_equity_score,
  ts_tsmart_moral_loyalty_score,
  ts_tsmart_moral_purity_score,
  ts_tsmart_children_present_score,
  ts_tsmart_college_graduate_score,
  ts_tsmart_high_school_only_score,
  ts_tsmart_marriage_score,
  ts_tsmart_income_rank_score,
  ts_tsmart_catholic_raw_score,
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
  ts_tsmart_urbanicity,
  ts_tsmart_urbanicity_rank,
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
  nm_score,
  os_score,
  lfll_score,
  am_score,
  hl_score,
  mr_score,
  voterbase_email,
  catalistmodel_ticket_splitter
FROM ts.ntl_current
  LEFT JOIN impactproject.ticketsplitter_matching_matched 
       ON ticketsplitter_matching_matched.voterbase_id = ntl_current.vb_voterbase_id
  LEFT JOIN impactproject.segment_scores USING (vb_voterbase_id)
  LEFT JOIN tmc.email_current ON ntl_current.vb_voterbase_id = email_current.voterbase_id
WHERE ts_tsmart_presidential_general_turnout_score > 40
  AND vb_tsmart_state = 'AZ'
  AND vb_tsmart_hd IN ('006', '017', '020', '027', '028', '029')
  AND vb_voterbase_age > 17
  AND vb_voterbase_age < 100
  AND vb_voterbase_deceased_flag IS NULL" %>%
  sql() %>%
  read_civis(database = "TMC")

raw_az_dat %>%
  mutate(vb_tsmart_zip = str_pad(vb_tsmart_zip, width = 5, pad = "0")) %>%
  mutate(vb_tsmart_zip4 = str_pad(vb_tsmart_zip4, width = 4, pad = "0")) %>%
  mutate(college = if_else(vb_education %in% c(3,4), 1, 0)) %>%
  saveRDS(here("output", "raw_arizona_dat.Rds"))

raw_az_dat <- readRDS(here("output", "raw_arizona_dat.Rds"))

arizona_dat <- raw_az_dat %>%
  mutate(HHID = paste(vb_tsmart_full_address, vb_tsmart_city)) %>%
  add_count(HHID, name = "household_size") %>% 
  add_count(vb_voterbase_phone, name = "vb_voterbase_phone_count") %>%
  filter(vb_voterbase_registration_status == 'Registered') %>%
  filter(vb_voterbase_age < 95) %>%
  filter(!is.na(vb_voterbase_phone)) %>%
  filter(ts_tsmart_presidential_general_turnout_score > 40) %>%
  filter(household_size < 7) %>%
  filter(vb_voterbase_mailable_flag == "Yes") %>%
  filter(str_length(vb_tsmart_first_name) > 1) %>%
  filter((!is.na(catalistmodel_ticket_splitter)) | nm_score >32.5) %>% #################
  add_count(vb_tsmart_full_address, name = "targets_in_hh") %>%
  mutate(in_experiment = 1) %>%
  mutate(in_experiment = if_else(is.na(vb_voterbase_phone), 0, in_experiment)) %>%
  mutate(in_experiment = if_else(vb_voterbase_phone_count >= 10, 0, in_experiment)) %>%
  mutate(in_experiment = if_else(vb_voterbase_phone_count > 3 & vb_voterbase_phone_type=="Wireless", 0, in_experiment)) %>%
  filter(vb_tsmart_hd != 27) # messed up and we don't have ticketsplitter for these folks yet.

# Output for Phone Screen -------------------------------------------------
arizona_dat %>%
  filter(in_experiment==1) %>%
  select(vb_voterbase_phone) %>%
  unique() %>%
  write_csv(here("output", paste0("arizona_numbers_for_screen_", Sys.Date(), ".csv")))
