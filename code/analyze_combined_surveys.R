# Load Packages -----------------------------------------------------------
library(TIPtools)
library(tidyverse)
library(civis)
library(here)
library(lme4)

# Load Data ---------------------------------------------------------------
raw_az <- read_csv(here("data", "AZ_RETURN.txt")) %>%
  filter(FDISP=="01") %>%
  filter(vb_voterbase_id != "AZ-4557896") %>%
  filter(vb_voterbase_id != "AZ-4866224") %>%
  as_tibble() %>% 
  select(DATE, TIME, duration, starts_with("Q"), vb_voterbase_id) %>%
  mutate(state = "AZ")

raw_co <- read_csv(here("data", "CO_RETURN.txt")) %>%
  filter(FDISP=="01") %>%
  as_tibble() %>% 
  select(DATE, TIME, duration, starts_with("Q"), vb_voterbase_id) %>%
  mutate(state = "CO")

raw_fl <- read_csv(here("data", "FL_RETURN.txt")) %>%
  filter(FDISP=="01") %>%
  as_tibble() %>%
  select(DATE, TIME, duration, starts_with("Q"), vb_voterbase_id) %>%
  mutate(state = "FL")

raw_me <- read_csv(here("data", "ME_RETURN.txt")) %>%
  filter(FDISP=="01") %>%
  as_tibble() %>%
  select(DATE, TIME, duration, starts_with("Q"), vb_voterbase_id) %>%
  mutate(state = "ME")
  
raw_mi <- read_csv(here("data", "MI_RETURN.txt")) %>%
  filter(FDISP=="01") %>%
  as_tibble() %>%
  select(DATE, TIME, duration, starts_with("Q"), vb_voterbase_id) %>%
  mutate(state = "MI")

randomized_dat <- readRDS(here("output", "all_randomized_dat.Rds")) %>%
  filter(assignment %in% c("treatment", "control")) %>%
  mutate(maine_ticket_splitter = fct_explicit_na(maine_ticket_splitter),
         dem = 0)

randomized_dat$dem[randomized_dat$vb_tsmart_state == "MI" & randomized_dat$vb_tsmart_hd %in% c(19, 35, 71)] <- 1
randomized_dat$dem[randomized_dat$vb_tsmart_state == "ME" & randomized_dat$vb_tsmart_hd %in% c(14)] <- 1
randomized_dat$dem[randomized_dat$vb_tsmart_state == "CO" & randomized_dat$vb_tsmart_hd %in% c(16, 19, 26)] <- 1


# Combine Surveys Across States -------------------------------------------
raw_combined <- bind_rows(
  select(raw_az, vb_voterbase_id, DATE, TIME, duration, state, Q2, Q3=Q3A, Q4=Q4A, Q6, Q7, Q8),
  select(raw_co, vb_voterbase_id, DATE, TIME, duration, state, Q2, Q3, Q4, Q6, Q7, Q8),
  select(raw_fl, vb_voterbase_id, DATE, TIME, duration, state, Q2, Q3, Q4, Q6, Q7, Q8),
  select(raw_me, vb_voterbase_id, DATE, TIME, duration, state, Q2, Q3, Q4, Q6, Q7, Q8),
  select(raw_mi, vb_voterbase_id, DATE, TIME, duration, state, Q2, Q3=Q3B, Q4=Q4B, Q6, Q7, Q8)
)

# Merge to Voter File -----------------------------------------------------
survey_results <- raw_combined %>%
  left_join(randomized_dat, by = "vb_voterbase_id") %>%
  mutate(Qgender = recode(as.character(Q2), 
                          "1" = "Male",
                          "2" = "Female",
                          "8888" = "Unknown"),
         year_difference = (floor(vb_voterbase_dob/10000)) - Q8,
         Qfav_binary = recode(as.character(Q3), 
                              "1" = "Favorable",
                              "2" = "Unfavorable",
                              "7777" = "Never Heard",
                              "8888" = "Don't Know"),
         Qfav_modifier = recode(as.character(Q4),
                                "1" = "Strongly",
                                "2" = "Somewhat",
                                "8888" = "Somewhat"),
         Qfav = as_factor(str_remove(string = paste(Qfav_modifier, Qfav_binary), pattern = "NA ")),
         Qfav = fct_relevel(Qfav, 
                            "Strongly Favorable",
                            "Somewhat Favorable",
                            "Never Heard",
                            "Don't Know",
                            "Somewhat Unfavorable",
                            "Strongly Unfavorable"),
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
  filter(vb_voterbase_gender == "Unknown" | 
           Qgender == "Unknown" | 
           vb_voterbase_gender == Qgender) %>%
  filter(Q8 > 5000 | abs(year_difference < 3)) %>%
  mutate(Qgender = factor(Qgender),
         Qapproach = factor(Qapproach, levels = c("Investing in schools", "Both", "Don't Know", "Something else", "Keeping taxes low")),
         Qrecall = factor(Qrecall, levels = c("Yes", "Don't Know", "No"))) %>%
  mutate(Qfav_flipped = if_else(dem == 0,
                                fct_recode(Qfav,
                                           `Strongly Favorable` = "Strongly Unfavorable",
                                           `Somewhat Favorable` = "Somewhat Unfavorable",
                                           `Strongly Unfavorable` = "Strongly Favorable",
                                           `Somewhat Unfavorable` = "Somewhat Favorable"),
                                Qfav),
         Qfav_flipped_model = if_else(Qfav_flipped %in% c("Strongly Favorable", "Somewhat Favorable"), 1, 0),
         Qunfav_flipped_model = if_else(Qfav_flipped %in% c("Strongly Unfavorable", "Somewhat Unfavorable"), 1, 0),
         Qfav_flipped_continous = as.numeric(fct_collapse(Qfav_flipped, mid = c("Never Heard", "Don't Know")))) %>%
  select(-Qfav_modifier)


# Weight Data -------------------------------------------------------------
az_weighted <- weight_data(survey_dataset = survey_results[survey_results$state=='AZ',],
            universe_dataset = randomized_dat[randomized_dat$vb_tsmart_state == 'AZ',],
            max_weight = 3,
            assignment,
            college,
            catalistmodel_ticket_splitter,
            vb_voterbase_gender,
            vb_tsmart_hd)

co_weighted <- weight_data(survey_dataset = survey_results[survey_results$state=='CO',],
                           universe_dataset = randomized_dat[randomized_dat$vb_tsmart_state == 'CO',],
                           max_weight = 3,
                           assignment,
                           college,
                           catalistmodel_ticket_splitter,
                           vb_voterbase_gender,
                           vb_tsmart_sd)

fl_weighted <- weight_data(survey_dataset = survey_results[survey_results$state=='FL',],
                           universe_dataset = randomized_dat[randomized_dat$vb_tsmart_state == 'FL',],
                           max_weight = 3,
                           assignment,
                           college,
                           catalistmodel_ticket_splitter,
                           vb_voterbase_gender,
                           vb_tsmart_hd)

me_weighted <- weight_data(survey_dataset = survey_results[survey_results$state=='ME',],
                           universe_dataset = randomized_dat[randomized_dat$vb_tsmart_state == 'ME',],
                           max_weight = 3,
                           assignment,
                           college,
                           maine_ticket_splitter,
                           vb_voterbase_gender,
                           vb_tsmart_sd)

mi_weighted <- weight_data(survey_dataset = survey_results[survey_results$state=='MI',],
                           universe_dataset = randomized_dat[randomized_dat$vb_tsmart_state == 'MI',],
                           max_weight = 3,
                           assignment,
                           college,
                           catalistmodel_ticket_splitter,
                           vb_voterbase_gender,
                           vb_tsmart_hd)

combined_weighted <- bind_rows(az_weighted,
                               co_weighted,
                               fl_weighted,
                               me_weighted,
                               mi_weighted)

combined_weighted %>%
  group_by(state, assignment) %>%
  summarise(fav = mean(Qfav_flipped_model)) %>%
  spread(assignment, fav)


combined_weighted %>%
  select(Qfav, assignment, dem) %>% 
  table() %>%
  prop.table(margin=2) %>%
  round(3)


# Run some models ---------------------------------------------------------
combined_weighted %>%
  filter(dem==0) %>%
  mutate(fav_model = if_else(Qfav %in% c("Strongly Favorable", "Somewhat Favorable"), 1, 0)) %>%
  glm(formula = fav_model~assignment, family = "binomial", weights = weight) %>%
  summary()

combined_weighted %>%
  filter(dem==1) %>%
  lm(formula = Qfav_flipped_continous~assignment, weights = weight) %>%
  summary()


combined_weighted %>%
  filter(dem==0) %>%
  filter(state %in% c("MI")) %>%
  lm(formula = Qfav_flipped_model~assignment, weights = weight) %>%
  summary()

combined_weighted %>%
  filter(dem == 0) %>%
  select(Qfav,Qfav_flipped_continous) %>%
  table()

# we decrease favorability from republicans

  
# Trying mixed model for MRP
mixed.lmer <- combined_weighted %>%
  mutate(fav_model = as.factor(if_else(Qfav_flipped %in% c("Strongly Favorable", "Somewhat Favorable"), 1, 0))) %>%
  glmer(formula = fav_model ~ assignment + college + vb_voterbase_gender + (1|vb_tsmart_state) + (1|dem), family = "binomial")

summary(mixed.lmer)

predict(mixed.lmer, 
        randomized_dat %>% mutate(assignment="treatment"), 
        type = "response",
        allow.new.levels=T) %>% 
  mean()

combined_weighted %>% 
  #filter(state == "MI", vb_tsmart_hd==71) %>% 
  mutate(outcome = if_else(Qfav %in% c("Strongly Favorable", "Somewhat Favorable"), 1, 0)) %>% 
  group_by(state) %>%
  summarize(mn = mean(outcome))
            pull(outcome) %>% 
  mean()

19 35, 71

# get ticketsplitter scores for everyone


combined_weighted %>%
  filter(dem==0) %>%
  group_by(dem, assignment, Qfav) %>%
  summarize(n = sum(weight)) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x=assignment, y = freq, fill = Qfav)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  scale_fill_manual(values = c("#d01c8b",
                               "#f1b6da",
                               "#d5d5d5",
                               "#bababa",
                               "#b8e186",
                               "#4dac26")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position="top",
        legend.title = element_blank()) +
  geom_text(aes(label = if_else(freq >= 0.02, 
                                scales::percent(freq, accuracy = 1),
                                "")), 
            position = position_stack(vjust = 0.5, reverse = T)) +
  labs(x = "", y = "")
  