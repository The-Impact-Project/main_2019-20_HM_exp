# Load Packages -----------------------------------------------------------
library(TIPtools)
library(tidyverse)
library(civis)
library(here)

maine_dems <- c(14)

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
                       "8888" = "Somewhat"),
         Qfav = as_factor(str_remove(string = paste(Qfav_modifier, Qfav_binary), pattern = "NA ")),
         Qfav = fct_relevel(Qfav, 
                            "Strongly Favorable",
                            "Somewhat Favorable",
                            "Never Heard",
                            "Don't Know",
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
         Qapproach = factor(Qapproach, levels = c("Investing in schools", "Both", "Don't Know", "Something else", "Keeping taxes low")),
         Qrecall = factor(Qrecall, levels = c("Yes", "Don't Know", "No"))) %>%
  mutate(Qfav_flipped = if_else(!vb_tsmart_sd %in% maine_dems,
                                fct_recode(Qfav,
                                          `Strongly Favorable` = "Strongly Unfavorable",
                                          `Somewhat Favorable` = "Somewhat Unfavorable",
                                          `Strongly Unfavorable` = "Strongly Favorable",
                                          `Somewhat Unfavorable` = "Somewhat Favorable"),
                                Qfav)) %>%
  weight_data(universe_dataset = randomized_dat,
              max_weight = 3,
              assignment,
              college,
              maine_ticket_splitter,
              vb_voterbase_gender) %>%
  select(-c(Qfav_binary, Qfav_modifier))

# make graphs
survey_results %>%
  group_by(assignment, Qfav_flipped) %>%
  summarise(n = sum(weight)) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x=assignment, fill=Qfav_flipped, y = freq)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("#d01c8b",
                               "#f1b6da",
                               "#d5d5d5",
                               "#bababa",
                               "#b8e186",
                               "#4dac26")) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position="top",
        legend.title = element_blank()) +
  geom_text(aes(label = if_else(freq >= 0.05, 
                                scales::percent(freq, accuracy = 1),
                                "")), 
            position = position_stack(vjust = 0.5, 
                                      reverse = TRUE))
  
survey_results %>%
  group_by(assignment, Qmaine_issue) %>%
  summarise(n = sum(weight)) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x=assignment, fill=Qmaine_issue, y = freq)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("#d01c8b",
                               "#f1b6da",
                               "#bababa",
                               "#b8e186",
                               "#4dac26")) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position="top",
        legend.title = element_blank()) +
  geom_text(aes(label = if_else(freq >= 0.05, 
                                scales::percent(freq, accuracy = 1),
                                "")), 
            position = position_stack(vjust = 0.5, 
                                      reverse = TRUE))

survey_results %>%
  mutate(Qapproach = fct_recode(Qapproach,
                                `Both/Don't Know/Something Else` = "Both",
                                `Both/Don't Know/Something Else` = "Don't Know",
                                `Both/Don't Know/Something Else` = "Something else")) %>%
  group_by(assignment, Qapproach) %>%
  summarise(n = sum(weight)) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x=assignment, fill=Qapproach, y = freq)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("#d01c8b",
                               "#bababa",
                               "#4dac26")) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position="top",
        legend.title = element_blank()) +
  geom_text(aes(label = if_else(freq >= 0.05, 
                                scales::percent(freq, accuracy = 1),
                                "")), 
            position = position_stack(vjust = 0.5, 
                                      reverse = TRUE))

survey_results %>%
  group_by(assignment, Qrecall) %>%
  summarise(n = sum(weight)) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x=assignment, fill=Qrecall, y = freq)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("#d01c8b",
                               "#bababa",
                               "#4dac26")) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position="top",
        legend.title = element_blank()) +
  geom_text(aes(label = if_else(freq >= 0.05, 
                                scales::percent(freq, accuracy = 1),
                                "")), 
            position = position_stack(vjust = 0.5, 
                                      reverse = TRUE))

# Regressions
survey_results %>%
  mutate(Qfav_binary = if_else(grepl(pattern = " Favorable", Qfav_flipped), 1, 0)) %>%
  select(Qfav_binary, assignment, vb_voterbase_gender, vb_tsmart_sd, college, maine_ticket_splitter) %>% 
  glm(formula = Qfav_binary~., family = "binomial") %>%
  summary()

survey_results %>%
  mutate(approach = if_else(Qapproach=="Investing in schools", 1, 0)) %>%
  select(approach, assignment, vb_voterbase_gender, vb_tsmart_sd, college, maine_ticket_splitter) %>% 
  glm(formula = approach~., 
      family = "binomial") %>%
  summary()

survey_results %>%
  mutate(maine_issue = if_else(grepl(pattern = " Support", Qmaine_issue), 1, 0)) %>%
  select(maine_issue, assignment, vb_voterbase_gender, vb_tsmart_sd, college, maine_ticket_splitter) %>% 
  glm(formula = maine_issue~., family = "binomial") %>%
  summary()

survey_results %>%
  mutate(recall = if_else(Qrecall == "Yes", 1, 0)) %>%
  select(recall, assignment, vb_voterbase_gender, vb_tsmart_sd, college, maine_ticket_splitter) %>% 
  glm(formula = recall~., family = "binomial") %>%
  summary()



  