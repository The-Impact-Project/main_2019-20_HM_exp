## This is to see if any of the dumb targeting options could work

# Get Peoria Data ---------------------------------------------------------
peoria_raw <- read_tsv(here("data", "list_for_peoria_match_appended.txt"), 
                       col_types = cols(
                         TMC_ID = col_character(),
                         CONFIDENCE = col_double(),
                         ZIP9 = col_character(),
                         `__LINE_NUMBER` = col_double(),
                         FIRST_NAME = col_character(),
                         MIDDLE_NAME = col_character(),
                         LAST_NAME = col_character(),
                         ADDRESS = col_character(),
                         ZIP = col_double(),
                         STATE = col_character(),
                         AGE = col_double(),
                         LINE_NUMBER = col_double(),
                         catalistmodel_peoria_values_cluster_2_0 = col_factor()
                       ))

peoria <- peoria_raw %>%
  select(FIRST_NAME,
         MIDDLE_NAME,
         LAST_NAME,
         peoria_cluster = catalistmodel_peoria_values_cluster_2_0)

# make DF -----------------------------------------------------------------
dumb_df <- combined_weighted %>%
  select(vb_voterbase_id,
         vb_tsmart_first_name,
         vb_tsmart_middle_name,
         vb_tsmart_last_name,
         Qfav,
         Q3,
         vb_vf_party,
         ts_tsmart_ideology_score,
         ts_tsmart_partisan_score,
         ts_tsmart_working_class_score,
         nm_score,
         vb_tsmart_state,
         dem,
         vb_voterbase_gender,
         assignment,
         college,
         predictwise_populism_score,
         predictwise_economic_populism_score,
         ts_tsmart_urbanicity,
         predictwise_compassion_score,
         nm_score,
         weight) %>%
  mutate(dem = if_else(dem==1, "supportive", "unsupportive")) %>%
  mutate(Qfav_simple = fct_collapse(Qfav, 
                                    favorable = c("Strongly Favorable", "Somewhat Favorable"),
                                    unfavorable = c("Strongly Unfavorable", "Somewhat Unfavorable"),
                                    neutral = c("Don't Know", "Never Heard"))) %>%
  mutate(noncollege_women = if_else(college == 0 & vb_voterbase_gender == "Female", "noncollege_women", "other")) %>%
  mutate(middle_ideology = if_else(ts_tsmart_ideology_score < 80 & ts_tsmart_ideology_score > 20, "middle_ideology", "extreme")) %>%
  mutate(working_class = if_else(ts_tsmart_working_class_score > 50, "working_class", "other")) %>%
  mutate(working_women = if_else(ts_tsmart_working_class_score > 50 & vb_voterbase_gender == "Female", "working_women", "other")) %>%
  mutate(working_men = if_else(ts_tsmart_working_class_score > 50 & vb_voterbase_gender == "Male", "working_men", "other")) %>%
  mutate(middle_ideology_women = if_else(ts_tsmart_ideology_score < 80 & ts_tsmart_ideology_score > 20 & vb_voterbase_gender == "Female", "middle_ideo_women", "other")) %>%
  mutate(middle_ideology_men = if_else(ts_tsmart_ideology_score < 80 & ts_tsmart_ideology_score > 20 & vb_voterbase_gender == "Male", "middle_ideo_men", "other")) %>%
  mutate(economic_populist = if_else(predictwise_economic_populism_score > 40, "econ_populist", "other")) %>%
  mutate(populist = if_else(predictwise_populism_score > 32, "populist", "other")) %>%
  mutate(working_econ_populist = if_else(predictwise_economic_populism_score > 30 
                                         & ts_tsmart_working_class_score > 40 
                                         & vb_voterbase_gender == "Female", "working_populist", "other")) %>%
  mutate(compassion = if_else(predictwise_compassion_score > 55, "high", "low")) %>%
  mutate(high_nm = if_else(nm_score > 33, "high", "low")) %>%
  mutate(low_nm = if_else(nm_score < 25, "low", "high")) %>%
  left_join(peoria, by = c("vb_tsmart_first_name" = "FIRST_NAME", "vb_tsmart_middle_name" = "MIDDLE_NAME", "vb_tsmart_last_name" = "LAST_NAME"))



make_effect_table <- function(data, target_var) {
  
  target_var <- enquo(target_var)
  
  data %>%
    group_by(!!target_var, dem, assignment, Qfav_simple) %>%
    summarise(n = sum(weight)) %>%
    mutate(freq = n / sum(n)) %>%
    ungroup() %>%
    select(-n) %>%
    spread(assignment, freq) %>%
    mutate(effect = treatment - control) %>%
    select(-c(control, treatment)) %>%
    filter(Qfav_simple != "neutral") %>%
    spread(!!target_var, effect) %>%
    return()
  
}

dumb_df %>%


dumb_df %>%
  make_effect_table(noncollege_women) %>%
  mutate(multi = noncollege_women * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(middle_ideology) %>%
  mutate(multi = middle_ideology * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(working_class) %>%
  mutate(multi = working_class * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(working_women) %>%
  mutate(multi = working_women * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(working_men) %>%
  mutate(multi = working_men * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(vb_voterbase_gender) %>%
  mutate(multi = Female * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(vb_voterbase_gender) %>%
  mutate(multi = Male * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(middle_ideology_women) %>%
  mutate(multi = middle_ideo_women * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(middle_ideology_men) %>%
  mutate(multi = middle_ideo_men * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(populist) %>%
  mutate(multi = populist * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(working_econ_populist) %>%
  mutate(multi = working_populist * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(economic_populist) %>%
  mutate(multi = econ_populist * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(compassion) %>%
  mutate(multi = high * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(high_nm) %>%
  mutate(multi = high * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(low_nm) %>%
  mutate(multi = low * c(1, -1, -1, 1)) %>%
  pull(multi) %>%
  sum()

dumb_df %>%
  make_effect_table(peoria_cluster) %>%
  mutate(multi = `Civic Faithful` * c(1, -1, -1, 1)) %>% 
  pull(multi) %>%
  sum()


# Graph All Effects
all_effect_df <- tibble(targets = c("Men",
                                    "Women",
                                    "Non-College Women",
                                    "Middle Ideology",
                                    "Middle Ideology Men",
                                    "Middle Ideology Women",
                                    "Working Class",
                                    "Working Class Men",
                                    "Working Class Women",
                                    "Populists",
                                    "Economic Populists",
                                    "Working Econ. Populists",
                                    "Compassion",
                                    "High New Middle",
                                    "Low New Middle"),
                        effect = c(0.04022165,
                                    0.0416806,
                                    0.06758357,
                                    0.04022188,
                                    0.03950804,
                                    0.03950804,
                                    0.04781648,
                                    0.05499204,
                                    0.05499204,
                                    0.02312261,
                                    0.07992882,
                                    0.07315188,
                                    0.04020778,
                                    0.01916734,
                                    0.05456553))

all_effect_df %>%
  ggplot(aes(x=reorder(targets, effect), y = effect)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x="", y = "Effect")


# Make a graph of the one that works --------------------------------------
dumb_df %>%
  filter(economic_populist=="econ_populist") %>%
  group_by(dem, assignment, Qfav_simple) %>%
  summarise(n = sum(weight)) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x=assignment, y = freq, fill = Qfav_simple)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  facet_wrap(~dem) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  scale_fill_manual(values = c("#e853af",
                               "#d5d5d5",
                               "#7fda5a")) +
  theme(legend.position="top",
        legend.title = element_blank()) +
  geom_text(aes(label = if_else(freq >= 0.02, 
                                scales::percent(freq, accuracy = .1),
                                "")), 
            position = position_stack(vjust = 0.5, reverse = T)) +
  labs(x = "", y = "")
  ggplot(aes())



# Evaluate effect on issues -----------------------------------------------
issue_df <- bind_rows(
    select(combined_weighted, vb_tsmart_first_name, vb_tsmart_middle_name, vb_tsmart_last_name, college, vb_voterbase_gender, predictwise_economic_populism_score, state, assignment, weight, Q5) %>% filter(!is.na(Q5)) %>% mutate(issue = "A"),
    select(combined_weighted, vb_tsmart_first_name, vb_tsmart_middle_name, vb_tsmart_last_name, college, vb_voterbase_gender, predictwise_economic_populism_score, state, assignment, weight, Q5=Q5A) %>% filter(!is.na(Q5)) %>% mutate(issue = "A")) %>%
    left_join(peoria, by = c("vb_tsmart_first_name" = "FIRST_NAME", 
                             "vb_tsmart_middle_name" = "MIDDLE_NAME", 
                             "vb_tsmart_last_name" = "LAST_NAME")) %>%
    mutate(Qissue = recode(as.character(Q5),
                           "1" = "Strongly Support",
                           "2" = "Somewhat Support",
                           "3" = "Somewhat Oppose",
                           "4" = "Strongly Oppose",
                           "8888" = "No Opinion")) %>%
    mutate(Qissue = factor(Qissue, levels = c("Strongly Support", 
                                              "Somewhat Support", 
                                              "No Opinion", 
                                              "Somewhat Oppose", 
                                              "Strongly Oppose"))) %>%
    filter(state != "AZ") %>%
    mutate(economic_populist = if_else(predictwise_economic_populism_score > 40, "econ_populist", "other"))

issue_df %>%
  filter(!is.na(economic_populist)) %>%
  group_by(assignment, economic_populist, Qissue) %>%
  summarize(n = sum(weight)) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x=assignment, y = freq, fill = Qissue)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  facet_wrap(~economic_populist) +
  scale_fill_manual(values = c("#e66101",
                               "#fdb863",
                               "#f7f7f7",
                               "#b2abd2",
                               "#5e3c99")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position="top",
        legend.title = element_blank()) +
  labs(x = "", y = "") +
  geom_text(aes(label = if_else(freq >= 0.02, 
                                scales::percent(freq, accuracy = 1),
                                "")), 
            position = position_stack(vjust = 0.5, reverse = T))


issue_df %>%
  mutate(noncollege_women = if_else(college ==0 & vb_voterbase_gender == "Female", "nc_w", "other")) %>%
  group_by(assignment, noncollege_women, Qissue) %>%
  summarize(n = sum(weight)) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x=assignment, y = freq, fill = Qissue)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  scale_fill_manual(values = c("#e66101",
                               "#fdb863",
                               "#f7f7f7",
                               "#b2abd2",
                               "#5e3c99")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  facet_wrap(~noncollege_women) +
  theme(legend.position="top",
        legend.title = element_blank()) +
  labs(x = "", y = "") +
  geom_text(aes(label = if_else(freq >= 0.02, 
                                scales::percent(freq, accuracy = 1),
                                "")), 
            position = position_stack(vjust = 0.5, reverse = T))

issue_df %>%
  filter(peoria_cluster=="Civic Faithful") %>%
  group_by(assignment, peoria_cluster, Qissue) %>%
  summarize(n = sum(weight)) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x=assignment, y = freq, fill = Qissue)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  scale_fill_manual(values = c("#e66101",
                               "#fdb863",
                               "#f7f7f7",
                               "#b2abd2",
                               "#5e3c99")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position="top",
        legend.title = element_blank()) +
  labs(x = "", y = "") +
  geom_text(aes(label = if_else(freq >= 0.02, 
                                scales::percent(freq, accuracy = 1),
                                "")), 
            position = position_stack(vjust = 0.5, reverse = T))

# Output to send to Catalist ----------------------------------------------
combined_weighted %>% 
  select(first_name = vb_tsmart_first_name, 
         middle_name = vb_tsmart_middle_name, 
         last_name = vb_tsmart_last_name, 
         address = vb_tsmart_full_address, 
         zip = vb_tsmart_zip, 
         state = vb_tsmart_state, 
         age = vb_voterbase_age) %>%
  write_csv(here("output", "list_for_peoria_match.csv"))
  





