
# Load Packages -----------------------------------------------------------
library(TIPtools)
library(tidyverse)
library(civis)
library(here)
library(purrr)

raw_maine_dat <- "SELECT 
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
  ticketsplitter
FROM ts.ntl_current
  LEFT JOIN impactproject.segment_scores USING (vb_voterbase_id)
  LEFT JOIN impactproject.me_ticketsplitter USING (vb_voterid)
  LEFT JOIN tmc.email_current ON ntl_current.vb_voterbase_id = email_current.voterbase_id
WHERE vb_tsmart_state = 'ME'
  AND vb_tsmart_sd IN ('013', '014', '015', '016')
  AND vb_voterbase_deceased_flag IS NULL" %>%
  sql() %>%
  read_civis(database = "TMC")

raw_maine_dat %>%
  mutate(vb_tsmart_zip = str_pad(vb_tsmart_zip, width = 5, pad = "0")) %>%
  mutate(vb_tsmart_zip4 = str_pad(vb_tsmart_zip4, width = 4, pad = "0")) %>%
  mutate(college = if_else(vb_education %in% c(3,4), 1, 0)) %>%
  saveRDS(here("output", "raw_maine_dat.RDS"))

raw_maine_dat <- readRDS(here("output", "raw_maine_dat.RDS"))

maine_dat <- raw_maine_dat %>%
  mutate(ticketsplitter = na_if(ticketsplitter, "")) %>%
  mutate(ticketsplitter = fct_drop(ticketsplitter)) %>%
  mutate(HHID = paste(vb_tsmart_full_address, vb_tsmart_city)) %>%
  add_count(vb_tsmart_full_address, name = "household_size") %>% # this should have pasted city with address
  add_count(vb_voterbase_phone, name = "vb_voterbase_phone_count") %>%
  filter(vb_voterbase_registration_status == 'Registered') %>%
  filter(vb_voterbase_age < 95) %>%
  filter(nm_score > 29.2 | ticketsplitter %in% c("highest", "middle", "lowest")) %>% 
  filter(ts_tsmart_presidential_general_turnout_score > 40) %>%
  filter(household_size < 7) %>%
  filter(vb_voterbase_mailable_flag == "Yes") %>%
  filter(str_length(vb_tsmart_first_name) > 1) %>%
  add_count(vb_tsmart_full_address, name = "targets_in_hh") %>%
  mutate(in_experiment = 1) %>%
  mutate(in_experiment = if_else(is.na(vb_voterbase_phone), 0, in_experiment)) %>%
  mutate(in_experiment = if_else(vb_voterbase_phone_count >= 10, 0, in_experiment)) %>%
  mutate(in_experiment = if_else(vb_voterbase_phone_count > 3 & vb_voterbase_phone_type=="Wireless", 0, in_experiment))
  
# Check names for matching ------------------------------------------------
maine_dat %>%
  select(vb_voterid, vb_tsmart_first_name, vb_tsmart_last_name) %>%
  head(10)

# Output for Phone Screen -------------------------------------------------
maine_dat %>%
  filter(in_experiment==1) %>%
  select(vb_voterbase_id, vb_voterbase_phone) %>%
  write_csv(here("output", paste0("maine_numbers_for_screen_", Sys.Date(), ".csv")))

# Load and rematch phone screen results -----------------------------------
screen_dat <- read_csv(here("data", "TMC-ME-Screen.csv")) %>%
  select(result, number) %>%
  rename(screen_result = "result") %>%
  rename(vb_voterbase_phone = "number") %>%
  mutate(passed_phone_screen = fct_collapse(screen_result,
                                            good_number = c("Answering Machine", "Busy", "Live Person", "No Answer", "Wireless"),
                                            bad_number = c("Fast Busy", "FAX", "Operator", "Problem")))

maine_dat <- maine_dat %>%
  left_join(y = screen_dat, by = "vb_voterbase_phone") %>%
  mutate(in_experiment = if_else(passed_phone_screen %in% c("bad_number", NA), 0, in_experiment)) %>%
  mutate(screened_phone = ifelse(passed_phone_screen == "good_number", vb_voterbase_phone, NA))

# 6400 is double the number of people we want to contact in SD14
num_in_sd14 <- maine_dat %>% filter(vb_tsmart_sd==14, in_experiment==1) %>% nrow()
maine_dat$random_num <- rbinom(n = nrow(maine_dat), prob = 6400 / num_in_sd14, size = 1)

maine_dat <- maine_dat %>%
  mutate(in_experiment = if_else(vb_tsmart_sd == 14 & random_num == 0, 0, in_experiment))

maine_dat <- maine_dat %>%
  mutate(HH_in_exp = if_else(vb_voterbase_phone %in% maine_dat$vb_voterbase_phone[maine_dat$in_experiment==1], 1, 0)) %>% # doesn't share Address or phone with exp
  mutate(phone_in_exp = if_else(HHID %in% maine_dat$HHID[maine_dat$in_experiment==1], 1, 0)) %>% # doesn't share phone with exp
  mutate(in_exp_HH_or_phone = if_else(HH_in_exp==1, 1, if_else(phone_in_exp==1, 1, 0)))

randomized_dat <- maine_dat %>%
  filter(in_experiment==1) %>%
  balance_randomization(block_vars = c("vb_tsmart_sd"),
                        cluster_vars = c("vb_tsmart_full_address", "vb_tsmart_city"),
                        n_treatment = 2,
                        balance_vars =c("os_score",
                                        "am_score",
                                        "ts_tsmart_offyear_general_turnout_score",
                                        "ts_tsmart_evangelical_raw_score",
                                        "ts_tsmart_otherchristian_raw_score",
                                        "ts_tsmart_prochoice_score",
                                        "ts_tsmart_path_to_citizen_score",
                                        "ts_tsmart_ideology_score"),
                        attempts = 50)

# check if clustering was succesful. this should return character(0)
intersect(randomized_dat$HHID[randomized_dat$assignment=="control"],
randomized_dat$HHID[randomized_dat$assignment=="treatment"])

randomized_dat %>% 
  select(assignment, vb_tsmart_sd) %>%
  table()

district_target_counts <- data.frame(vb_tsmart_sd = c(13, 14, 15, 16),
                                     target_goal = c(5300, 3200, 7300, 6800))

# see how many more we need in each SD
counts_df <- randomized_dat %>%
  filter(assignment == "treatment") %>%
  group_by(vb_tsmart_sd) %>%
  summarise(targeted_in_experiment_so_far = n()) %>%
  ungroup() %>%
  full_join(district_target_counts, by = "vb_tsmart_sd") %>%
  mutate(more_needed = target_goal - targeted_in_experiment_so_far) %>%
  mutate(more_needed = if_else(more_needed > 0, more_needed, 0)) %>%
  full_join(filter(maine_dat, in_exp_HH_or_phone==0) %>% 
              group_by(vb_tsmart_sd) %>% 
              summarise(number_available_to_add = n()), 
            by = "vb_tsmart_sd") %>%
  mutate(records_being_added = pmin(more_needed, number_available_to_add))

# add the number of non experiment people needed in each district.
randomized_dat <- maine_dat %>%
  filter(in_exp_HH_or_phone==0) %>%
  group_by(vb_tsmart_sd) %>% 
  nest() %>%            
  ungroup() %>% 
  arrange(vb_tsmart_sd) %>%
  left_join(counts_df, by = "vb_tsmart_sd") %>%
  mutate(samp = map2(data, records_being_added, sample_n)) %>% 
  unnest(samp) %>% 
  mutate(assignment = "not_in_experiment") %>%
  bind_rows(randomized_dat) %>%
  select(-c(targeted_in_experiment_so_far, target_goal, more_needed, number_available_to_add, records_being_added))

table(randomized_dat$assignment, useNA = 'always')
table(randomized_dat$vb_tsmart_sd, randomized_dat$assignment, useNA = 'always')
table(randomized_dat$vb_tsmart_sd, randomized_dat$in_experiment, useNA = 'always')
table(randomized_dat$passed_phone_screen, randomized_dat$assignment)

# Data Checks -------------------------------------------------------------
# Check that balance was correct
randomized_dat %>%
  filter(in_experiment == 1) %>%
  select(assignment, vb_voterbase_gender) %>%
  table() %>%
  prop.table(margin = 1)

randomized_dat %>%
  filter(in_experiment == 1) %>%
  select(assignment, college) %>%
  table() %>%
  prop.table(margin = 1)

randomized_dat %>%
  filter(in_experiment == 1) %>%
  mutate(party = fct_lump(vb_vf_party)) %>%
  select(assignment, party) %>%
  table() %>%
  prop.table(margin = 1) %>%
  round(digits = 2)

randomized_dat %>%
  filter(in_experiment == 1) %>%
  select(assignment, vb_education) %>%
  table() %>%
  prop.table(margin = 1) %>%
  round(digits = 2)

randomized_dat %>%
  filter(in_experiment == 1) %>%
  group_by(assignment) %>%
  summarise(partisan = mean(ts_tsmart_partisan_score),
            ideology = mean(ts_tsmart_ideology_score),
            os = mean(os_score, na.rm = T),
            am = mean(am_score, na.rm = T),
            turnout = mean(ts_tsmart_offyear_general_turnout_score),
            evangelical = mean(ts_tsmart_evangelical_raw_score, na.rm = T),
            otherchristian = mean(ts_tsmart_otherchristian_raw_score, na.rm = T),
            citizenship = mean(ts_tsmart_path_to_citizen_score),
            prochoice = mean(ts_tsmart_prochoice_score))

# Check that standard data issues are not present
length(unique(randomized_dat$vb_voterbase_id)) == nrow(randomized_dat)
sum(str_length(randomized_dat$vb_tsmart_first_name) == 1) == 0
sum(randomized_dat$vb_voterbase_mailable_flag=="Yes") == nrow(randomized_dat)

randomized_dat %>%
  ggplot(aes(x=ts_tsmart_presidential_general_turnout_score)) +
  geom_histogram()

randomized_dat %>%
  ggplot(aes(x=nm_score)) +
  geom_histogram()



# Save randomized results -------------------------------------------------
# save full results as RDS
saveRDS(randomized_dat, here("output", paste0("maine_randomized_dat", Sys.Date(), ".Rds")))

# save dataset for vendors as RDS and CSV
maine_data_for_vendors <- randomized_dat %>%
  filter(assignment %in% c("not_in_experiment", "treatment")) %>%
  select(vb_voterbase_id,
         vb_voterid,
         vb_tsmart_sd,
         vb_tsmart_first_name,
         vb_tsmart_middle_name,
         vb_tsmart_last_name,
         vb_tsmart_name_suffix,
         vb_tsmart_full_address,
         vb_tsmart_city,
         vb_tsmart_state,
         vb_tsmart_zip,
         vb_tsmart_zip4,
         screened_phone,
         voterbase_email,
         assignment)

saveRDS(me_data_for_vendors, here("output", paste0("maine_data_for_vendors", Sys.Date(), ".Rds")))
write_csv(me_data_for_vendors, here("output", paste0("maine_data_for_vendors"), Sys.Date(), ".csv"))


