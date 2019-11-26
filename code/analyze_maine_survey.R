# Load Packages -----------------------------------------------------------
library(TIPtools)
library(tidyverse)
library(civis)
library(here)

amm_results <- read_csv(here("data", "TMC_ME_Return_1122_1124.csv")) %>%
  filter(FDISP=="01") %>%
  as_tibble() %>%
  select(DATE, TIME, duration, starts_with("Q"), vb_voterbase_id)

randomized_dat <- readRDS(here("output", "all_randomized_dat.Rds")) %>%
  filter(vb_tsmart_state == "ME") %>%
  filter(assignment %in% c("treatment", "control")) %>%
  mutate(maine_ticket_splitter = fct_explicit_na(maine_ticket_splitter))

# merge to voter file
survey_results <- amm_results %>%
  left_join(randomized_dat, by = "vb_voterbase_id") %>%
  mutate(Qgender = recode(as.character(Q2), 
                                   "1" = "Male",
                                   "2" = "Female"),
         year_difference = (floor(vb_voterbase_dob/10000)) - Q8,
         Qfav_binary = recode(as.character(Q3), 
                       "1" = "Favorable",
                       "2" = "Unfavorable",
                       "7777" = "Never Heard",
                       "8888" = "Don't Know"),
         Qfav_modifier = recode(as.character(Q4),
                       "1" = "Strongly",
                       "2" = "Somewhat",
                       "8888" = "Slightly"),
         Qfav = as_factor(str_remove(string = paste(Qfav_modifier, Qfav_binary), pattern = "NA ")),
         Qfav = fct_relevel(Qfav, 
                            "Strongly Favorable",
                            "Somewhat Favorable",
                            "Slightly Favorable",
                            "Never Heard",
                            "Don't Know",
                            "Slightly Unfavorable",
                            "Somewhat Unfavorable",
                            "Strongly Unfavorable"),
         Qmaine_issue = recode(as.character(Q5),
                              "1" = "Strongly Support",
                              "2" = "Somewhat Support",
                              "3" = "Somewhat Oppose",
                              "4" = "Strongly Oppose",
                              "8888" = "Don't Know"),
         Qapproach = recode(as.character(Q6),
                            "1" = "Keeping taxes low",
                            "2" = "Investing in schools",
                            "3" = "Both",
                            "4" = "Something else",
                            "8888" = "Don't Know"),
         Qrecall = recode(as.character(Q7),
                          "1" = "Yes",
                          "2" = "No",
                          "8888" = "Don't Know")) %>%
  filter(vb_voterbase_gender == "Unknown" | (vb_voterbase_gender == Qgender)) %>%
  filter(Q8 > 5000 | abs(year_difference < 3)) %>%
  mutate(Qgender = factor(Qgender),
         Qmaine_issue = factor(Qmaine_issue, levels = c("Strongly Support", "Somewhat Support", "Don't Know", "Somewhat Oppose", "Strongly Oppose")),
         Qapproach = factor(Qapproach, levels = c("Investing in schools", "Both", "Don't Know", "Something Else", "Keeping taxes low")),
         Qrecall = factor(Qrecall, levels = c("Yes", "Don't Know", "No"))) %>%
  weight_data(universe_dataset = randomized_dat,
              max_weight = 3,
              assignment,
              college,
              maine_ticket_splitter,
              vb_voterbase_gender)

# make graphs





  