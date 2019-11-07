# This gets data for people in LD 27. This district is not in the experiment. But we are contacting 5K people with high ticketsplitter scores.

# Load Packages -----------------------------------------------------------
library(TIPtools)
library(tidyverse)
library(civis)
library(here)
library(purrr)

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
  add_count(vb_tsmart_full_address, name = "targets_in_hh") %>%
  mutate(in_experiment = 0) %>%
  filter(vb_tsmart_hd == 27) %>%
  select(-catalistmodel_ticket_splitter)

# get ticketsplitter scores
vb_ids <- paste(arizona_dat$vb_voterbase_id, collapse = "', '")
query <- sql(sprintf("
SELECT 
  voterbase_id AS vb_voterbase_id,
  catalistmodel_ticket_splitter
FROM impactproject.ticketsplitter_matching_matched
WHERE voterbase_id IN ('%s')", vb_ids))

ticketsplitter_dat <- read_civis(query, database = "TMC")

# get 5,000 but only 1 in each household
arizona_dat <- arizona_dat %>%
  left_join(ticketsplitter_dat, by = "vb_voterbase_id") %>%
  mutate(priority = coalesce(catalistmodel_ticket_splitter, as.integer(0))) %>%
  arrange(-priority) %>%
  group_by(HHID) %>%
  mutate(rownum = row_number()) %>%
  ungroup() %>%
  filter(rownum==1) %>%
  head(5000) %>%
  select(-c(priority, rownum))

# Check that standard data issues are not present
length(unique(arizona_dat$vb_voterbase_id)) == nrow(arizona_dat)
sum(str_length(arizona_dat$vb_tsmart_first_name) == 1) == 0
sum(arizona_dat$vb_voterbase_mailable_flag=="Yes") == nrow(arizona_dat)

arizona_dat %>%
  ggplot(aes(x=ts_tsmart_presidential_general_turnout_score)) +
  geom_histogram()

arizona_dat %>%
  ggplot(aes(x=nm_score)) +
  geom_histogram()

arizona_dat %>%
  ggplot(aes(x=catalistmodel_ticket_splitter)) +
  geom_histogram()


# Save randomized results -------------------------------------------------
# save full results as RDS
saveRDS(arizona_dat, here("output", paste0("arizona_ld27_list", Sys.Date(), ".Rds")))

# save dataset for vendors as RDS and CSV
arizona_data_for_vendors <- arizona_dat %>%
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
         vb_voterbase_phone,
         vb_voterbase_gender,
         vb_voterbase_dob,
         vb_voterbase_age,
         voterbase_email)

saveRDS(arizona_data_for_vendors, 
        here("output", paste0("arizona_ld27_data_for_vendors", Sys.Date(), ".Rds")))
write_csv(arizona_data_for_vendors, 
          here("output", paste0("arizona_ld27_data_for_vendors", Sys.Date(), ".csv")))

