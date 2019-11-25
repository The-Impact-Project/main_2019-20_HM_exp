# Load Packages -----------------------------------------------------------
library(TIPtools)
library(tidyverse)
library(civis)
library(here)

amm_results <- read_csv(here("data", "TMC_ME_Return_1122_1124.csv")) %>%
  filter(FDISP=="01") %>%
  as_tibble() %>%
  select(DATE, TIME, duration, starts_with("Q"), vb_voterbase_id, assignment)

randomized_dat <- readRDS(here("output", "all_randomized_dat.Rds"))

# merge to voter file
survey_results <- amm_results %>%
  left_join(randomized_dat, by = "vb_voterbase_id") %>%
  mutate(Qgender = recode(as.character(Q2), 
                                   "1" = "Male",
                                   "2" = "Female"),
         year_difference = (floor(vb_voterbase_dob/10000)) - Q8) %>%
  filter(vb_voterbase_gender == "Unknown" | (vb_voterbase_gender == Qgender)) %>%
  filter(Q8 > 5000 | abs(year_difference < 3))
  
survey_results %>%
  filter(Q8 < 5000) %>%
  mutate(vf_dob = floor(vb_voterbase_dob/10000)) %>%
  mutate(year_difference = vf_dob - Q8) %>%
  pull(year_difference) %>% table()
  ggplot(aes(x=year_difference)) +
  geom_histogram()
  ggplot(aes(x=jitter(Q8, amount = 1), y=jitter(vf_dob, amount=1))) +
  geom_point()

# combine q3 and q4
# recode factors and order them
# weight data
# make graphs
