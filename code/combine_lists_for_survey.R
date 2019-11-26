# This will take all the randomized files and combine them into one big one,
# Then it creates a list to give to the survey vendor, AMM

# Load Packages -----------------------------------------------------------
library(TIPtools)
library(tidyverse)
library(civis)
library(here)

az <- readRDS(here("output", "arizona_randomized_dat2019-11-05.Rds")) %>%
  mutate(cate = NA,
         maine_ticket_splitter = NA)
co <- readRDS(here("output", "colorado_randomized_dat2019-10-25.Rds")) %>%
  select(-c(n, get_treated)) %>%
  mutate(catalistmodel_ticket_splitter = NA,
         cate = NA,
         maine_ticket_splitter = NA)
fl <- readRDS(here("output", "florida_randomized_dat2019-10-21.Rds")) %>%
  mutate(maine_ticket_splitter = NA,
         catalistmodel_ticket_splitter = NA)
me <- readRDS(here("output", "maine_randomized_dat2019-09-30.Rds")) %>%
  select(-random_num) %>%
  rename(maine_ticket_splitter = "ticketsplitter") %>%
  mutate(catalistmodel_ticket_splitter = NA,
         cate = NA)
mi <- readRDS(here("output", "michigan_randomized_dat2019-10-24.Rds")) %>%
  select(-get_treated) %>%
  mutate(cate = NA,
         maine_ticket_splitter = NA)
nv <- readRDS(here("output", "nevada_randomized_dat2019-10-24.Rds")) %>%
  select(-c(combined_districts, get_treated)) %>%
  mutate(catalistmodel_ticket_splitter = NA,
         cate = NA,
         maine_ticket_splitter = NA)
pa <- readRDS(here("output", "pennsylvania_randomized_dat2019-10-29.Rds")) %>%
  mutate(cate = NA,
         maine_ticket_splitter = NA)


setdiff(names(co),  names(az))
setdiff(names(fl),  names(az))
setdiff(names(me),  names(az))
setdiff(names(mi),  names(az))
setdiff(names(nv),  names(az))
setdiff(names(pa),  names(az))

setdiff(names(az),  names(co))
setdiff(names(az),  names(fl))
setdiff(names(az),  names(me))
setdiff(names(az),  names(mi))
setdiff(names(az),  names(nv))
setdiff(names(az),  names(pa))

all <- bind_rows(az,
                 co,
                 fl,
                 me,
                 mi,
                 nv,
                 pa)
all %>%
  saveRDS(here("output", "all_randomized_dat.Rds"))

state_goals <- tibble(vb_tsmart_state = c("AZ", "CO", "FL", "ME", "MI", "NV", "PA"),
                      goal = c(2000, 1500, 1500, 875, 2000, 1500, 1500)) %>%
  mutate(goal = round(goal/2))

goals_df <- all %>%
  filter(assignment %in% c("control", "treatment")) %>%
  group_by(assignment, vb_tsmart_state) %>%
  summarise(n = n(),
            phones = length(unique(screened_phone))) %>%
  left_join(state_goals) %>%
  mutate(rr_by_n = scales::percent(goal/n),
         rr_by_phones = scales::percent(goal/phones))

write_csv(goals_df, "/Users/andyzack/Desktop/survey_goals.csv")

# add do not call list
do_not_call <- read_csv(here("data", "do_not_call.csv"))


all %>%
  filter(assignment %in% c("control", "treatment")) %>% 
  filter(!screened_phone %in% do_not_call$screened_phone) %>% 
  filter(vb_tsmart_state == 'ME') %>%
  select(vb_voterbase_id,
         screened_phone,
         vb_tsmart_first_name,
         vb_tsmart_last_name,
         vb_tsmart_state,
         vb_tsmart_sd,
         vb_tsmart_hd,
         assignment,
         screen_result) %>%
  write_csv(here("output", "maine_list_for_AMM.csv"))

