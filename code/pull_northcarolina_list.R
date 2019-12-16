
# Load Packages -----------------------------------------------------------
library(TIPtools)
library(tidyverse)
library(civis)
library(here)
library(purrr)

raw_nc_dat <- "SELECT 
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
FROM impactproject.ticketsplitter_matching_nc_matched 
  LEFT JOIN ts.ntl_current
       ON ticketsplitter_matching_nc_matched.voterbase_id = ntl_current.vb_voterbase_id
  LEFT JOIN impactproject.segment_scores USING (vb_voterbase_id)
  LEFT JOIN tmc.email_current ON ntl_current.vb_voterbase_id = email_current.voterbase_id
WHERE ts_tsmart_presidential_general_turnout_score > 40
  AND vb_tsmart_state = 'NC'
  -- AND vb_tsmart_hd IN ('082', '083', '098', '045', '053', '005')
  AND vb_voterbase_age > 17
  AND vb_voterbase_age < 100
  AND vb_voterbase_deceased_flag IS NULL" %>%
  sql() %>%
  read_civis(database = "TMC")

raw_nc_dat %>%
  mutate(vb_tsmart_zip = str_pad(vb_tsmart_zip, width = 5, pad = "0")) %>%
  mutate(vb_tsmart_zip4 = str_pad(vb_tsmart_zip4, width = 4, pad = "0")) %>%
  mutate(college = if_else(vb_education %in% c(3,4), 1, 0)) %>%
  saveRDS(here("output", "raw_northcarolina_dat.Rds"))

raw_nc_dat <- readRDS(here("output", "raw_northcarolina_dat.Rds"))



##################################
# Everything below here is unedited
##################################
michigan_dat <- raw_mi_dat %>%
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
  filter((!is.na(catalistmodel_ticket_splitter)) | nm_score >28) %>%
  add_count(vb_tsmart_full_address, name = "targets_in_hh") %>%
  mutate(in_experiment = 1) %>%
  mutate(in_experiment = if_else(is.na(vb_voterbase_phone), 0, in_experiment)) %>%
  mutate(in_experiment = if_else(vb_voterbase_phone_count >= 10, 0, in_experiment)) %>%
  mutate(in_experiment = if_else(vb_voterbase_phone_count > 3 & vb_voterbase_phone_type=="Wireless", 0, in_experiment))


# Output for Phone Screen -------------------------------------------------
michigan_dat %>%
  filter(in_experiment==1) %>%
  select(vb_voterbase_phone) %>%
  unique() %>% 
  write_csv(here("output", paste0("michigan_numbers_for_screen_", Sys.Date(), ".csv")))

# Load and rematch phone screen results -----------------------------------
screen_dat <- read_csv(here("data", "TMC-MI-Screen.csv")) %>%
  select(result, number) %>%
  rename(screen_result = "result") %>%
  rename(vb_voterbase_phone = "number") %>%
  mutate(passed_phone_screen = fct_collapse(screen_result,
                                            good_number = c("Answering Machine", "Busy", "Live Person", "No Answer", "Wireless"),
                                            bad_number = c("Fast Busy", "FAX", "Operator", "Problem", "Route Unavailable")))

michigan_dat <- michigan_dat %>%
  left_join(y = screen_dat, by = "vb_voterbase_phone") %>%
  mutate(in_experiment = if_else(passed_phone_screen %in% c("bad_number", NA), 0, in_experiment)) %>%
  mutate(screened_phone = ifelse(passed_phone_screen == "good_number", vb_voterbase_phone, NA))

# check the people not in experiment to see if they share a household or phone with someone in the experiment
michigan_dat <- michigan_dat %>%
  mutate(HH_in_exp = if_else(vb_voterbase_phone %in% michigan_dat$vb_voterbase_phone[michigan_dat$in_experiment==1], 1, 0)) %>% # doesn't share Address or phone with exp
  mutate(phone_in_exp = if_else(HHID %in% michigan_dat$HHID[michigan_dat$in_experiment==1], 1, 0)) %>% # doesn't share phone with exp
  mutate(in_exp_HH_or_phone = if_else(HH_in_exp==1, 1, if_else(phone_in_exp==1, 1, 0)))

# limit who is in experiment in smaller districts
michigan_dat <- michigan_dat %>%
  group_by(vb_tsmart_hd, in_experiment) %>%
  mutate(id = row_number()) %>%
  ungroup %>%
  mutate(in_experiment = if_else(vb_tsmart_hd %in% c(35, 42) & in_experiment==1 & id > 2730, 0, in_experiment)) %>%
  select(-id) %>%
  add_count(vb_voterbase_id) %>%
  filter(n==1) %>%
  select(-n)

proportion_in_treatment <- 0.55

randomized_dat <- michigan_dat %>%
  filter(in_experiment==1) %>%
  balance_randomization(block_vars = c("vb_tsmart_hd"), 
                        cluster_vars = "HHID",
                        n_treatment = 2,
                        two_arm_treat_prob = proportion_in_treatment,
                        balance_vars =c("os_score",
                                        "am_score",
                                        "ts_tsmart_offyear_general_turnout_score",
                                        "ts_tsmart_evangelical_raw_score",
                                        "ts_tsmart_otherchristian_raw_score",
                                        "ts_tsmart_prochoice_score",
                                        "ts_tsmart_path_to_citizen_score"),
                        attempts = 50)

# check if clustering was succesful. this should return character(0)
intersect(randomized_dat$HHID[randomized_dat$assignment=="control"],
          randomized_dat$HHID[randomized_dat$assignment=="treatment"])

randomized_dat %>% 
  select(assignment, vb_tsmart_hd) %>%
  table()

district_target_counts <- data.frame(vb_tsmart_hd = c(19, 35, 39, 42, 71, 110),
                                     target_goal = c(11000, 1500, 11000, 1500, 11000, 11000))

# see how many more we need in each District
counts_df <- randomized_dat %>%
  filter(assignment == "treatment") %>%
  group_by(vb_tsmart_hd) %>%
  summarise(targeted_in_experiment_so_far = n()) %>%
  ungroup() %>%
  full_join(district_target_counts, by = "vb_tsmart_hd") %>%
  mutate(more_needed = target_goal - targeted_in_experiment_so_far) %>%
  mutate(more_needed = if_else(more_needed > 0, more_needed, 0)) %>%
  full_join(filter(michigan_dat, in_exp_HH_or_phone==0) %>% 
              group_by(vb_tsmart_hd) %>% 
              summarise(number_available_to_add = n()), 
            by = "vb_tsmart_hd") %>%
  mutate(records_being_added = pmin(more_needed, number_available_to_add))

# add the number of non experiment people needed in each district.
randomized_dat <- michigan_dat %>%
  filter(in_exp_HH_or_phone==0) %>%
  group_by(vb_tsmart_hd) %>% 
  nest() %>%            
  ungroup() %>% 
  arrange(vb_tsmart_hd) %>%
  left_join(counts_df, by = "vb_tsmart_hd") %>%
  mutate(samp = map2(data, records_being_added, sample_n)) %>% 
  unnest(samp) %>% 
  mutate(assignment = "not_in_experiment") %>%
  bind_rows(randomized_dat) %>%
  select(-c(targeted_in_experiment_so_far, target_goal, more_needed, number_available_to_add, records_being_added))

table(randomized_dat$assignment != "control", randomized_dat$vb_tsmart_hd)

# if necessary subtract extras from treatment groups
randomized_dat <- randomized_dat %>%
  sample_n(size = nrow(.), replace = FALSE) %>%
  mutate(get_treated = assignment != "control") %>%
  group_by(vb_tsmart_hd, get_treated) %>%
  mutate(id = row_number()) %>%
  ungroup() %>%
  filter(id <=11000 | get_treated == FALSE) %>%
  filter(vb_tsmart_hd %in% c(19, 71, 39, 110) | id <=1500 | get_treated == FALSE) %>%
  select(-id)

# check randomization and adding non-experiment people seems right
table(randomized_dat$assignment, useNA = 'always')
table(randomized_dat$assignment != "control", useNA = 'always')
table(randomized_dat$vb_tsmart_hd, randomized_dat$assignment, useNA = 'always')
table(randomized_dat$vb_tsmart_hd, randomized_dat$in_experiment, useNA = 'always')
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
  round(digits = 3)

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
            ideology = mean(ts_tsmart_ideology_score, na.rm = T),
            os = mean(os_score, na.rm = T),
            am = mean(am_score, na.rm = T),
            turnout = mean(ts_tsmart_offyear_general_turnout_score),
            evangelical = mean(ts_tsmart_evangelical_raw_score, na.rm = T),
            otherchristian = mean(ts_tsmart_otherchristian_raw_score, na.rm = T),
            citizenship = mean(ts_tsmart_path_to_citizen_score, na.rm = T),
            prochoice = mean(ts_tsmart_prochoice_score, na.rm = T))

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

randomized_dat %>%
  ggplot(aes(x=catalistmodel_ticket_splitter)) +
  geom_histogram()

# Save randomized results -------------------------------------------------
# save full results as RDS
saveRDS(randomized_dat, here("output", paste0("michigan_randomized_dat", Sys.Date(), ".Rds")))

# save dataset for vendors as RDS and CSV
michigan_data_for_vendors <- randomized_dat %>%
  filter(assignment %in% c("not_in_experiment", "treatment")) %>%
  select(vb_voterbase_id,
         vb_voterid,
         vb_tsmart_hd,
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
         vb_voterbase_gender,
         vb_voterbase_dob,
         vb_voterbase_age,
         voterbase_email,
         assignment,
         screen_result)

saveRDS(michigan_data_for_vendors, 
        here("output", paste0("michigan_data_for_vendors", Sys.Date(), ".Rds")))
write_csv(michigan_data_for_vendors, 
          here("output", paste0("michigan_data_for_vendors", Sys.Date(), ".csv")))

# create audience_report
randomized_dat %>%
  filter(assignment != "control") %>%
  mutate(vb_voterbase_deceased_flag = NA) %>%
  TIPtools::audience_document(output_directory = here("output", "audience_reports"), 
                              output_title = "Michigan HM Audience",
                              refresh_list = FALSE,
                              district_cross = "hd")