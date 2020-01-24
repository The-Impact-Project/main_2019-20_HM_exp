
score_effect_df <- tibble(cutoff = seq(.01, .99, .01),
                          nm_score_effect = NA,
                          cate_score_effect = NA,
                          ts_score_effect = NA,
                          econ_pop_effect = NA)

for (percentile_cutoff in score_effect_df$cutoff) {
  print(percentile_cutoff)
  
  score_effect_df$nm_score_effect[score_effect_df$cutoff == percentile_cutoff] <- cate_df %>%
    mutate(unfav = if_else(grepl(pattern = "Unfav", x = Qfav), 1, 0)) %>%
    mutate(nm_cat = cut(nm_score, 
                        breaks = quantile(nm_score, probs = c(0, percentile_cutoff, 1)), 
                        include.lowest = T, 
                        labels = c("low","high"))) %>%
    select(unfav, assignment, nm_cat, weight) %>%
    unite(col = nm_assignment, nm_cat, assignment) %>%
    group_by(nm_assignment, unfav) %>%
    summarise(n = sum(weight)) %>%
    mutate(freq = n / sum(n)) %>%
    filter(unfav==1) %>%
    select(-c(n, unfav)) %>%
    spread(nm_assignment, freq) %>%
    mutate(score_effect = high_treatment - high_control) %>%
    pull(score_effect)
  
  score_effect_df$econ_pop_effect[score_effect_df$cutoff == percentile_cutoff] <- cate_df %>%
    mutate(unfav = if_else(grepl(pattern = "Unfav", x = Qfav), 1, 0)) %>%
    mutate(econ_cat = cut(predictwise_economic_populism_score, 
                        breaks = quantile(predictwise_economic_populism_score, probs = c(0, percentile_cutoff, 1)), 
                        include.lowest = T, 
                        labels = c("low","high"))) %>%
    select(unfav, assignment, econ_cat, weight) %>%
    unite(col = econ_assignment, econ_cat, assignment) %>%
    group_by(econ_assignment, unfav) %>%
    summarise(n = sum(weight)) %>%
    mutate(freq = n / sum(n)) %>%
    filter(unfav==1) %>%
    select(-c(n, unfav)) %>%
    spread(econ_assignment, freq) %>%
    mutate(score_effect = high_treatment - high_control) %>%
    pull(score_effect)
  
  score_effect_df$cate_score_effect[score_effect_df$cutoff == percentile_cutoff] <- cate_df %>%
    mutate(unfav = if_else(grepl(pattern = "Unfav", x = Qfav), 1, 0)) %>%
    mutate(cate_cat = cut(cate, 
                          breaks = quantile(cate, probs = c(0, percentile_cutoff, 1)), 
                          include.lowest = T, 
                          labels = c("low","high"))) %>%
    select(unfav, assignment, cate_cat, weight) %>%
    unite(col = cate_assignment, cate_cat, assignment) %>%
    group_by(cate_assignment, unfav) %>%
    summarise(n = sum(weight)) %>%
    mutate(freq = n / sum(n)) %>%
    filter(unfav==1) %>%
    select(-c(n, unfav)) %>%
    spread(cate_assignment, freq) %>%
    mutate(score_effect = high_treatment - high_control) %>%
    pull(score_effect)
  
  if (percentile_cutoff > (sum(cate_df$ts_score == 0) / nrow(cate_df)) &
      percentile_cutoff < (sum(cate_df$ts_score < 100) / nrow(cate_df))) {
    score_effect_df$ts_score_effect[score_effect_df$cutoff == percentile_cutoff] <- cate_df %>%
      mutate(unfav = if_else(grepl(pattern = "Unfav", x = Qfav), 1, 0)) %>%
      mutate(ts_cat = cut(ts_score, 
                          breaks = quantile(ts_score, probs = c(0, percentile_cutoff, 1)), 
                          include.lowest = T, 
                          labels = c("low","high"))) %>%
      select(unfav, assignment, ts_cat, weight) %>%
      unite(col = ts_assignment, ts_cat, assignment) %>%
      group_by(ts_assignment, unfav) %>%
      summarise(n = sum(weight)) %>%
      mutate(freq = n / sum(n)) %>%
      filter(unfav==1) %>%
      select(-c(n, unfav)) %>%
      spread(ts_assignment, freq) %>%
      mutate(score_effect = high_treatment - high_control) %>%
      pull(score_effect)
  }
}

score_effect_df %>%
  mutate(cutoff = cutoff*100) %>%
  rename(`New Middle` = "nm_score_effect",
         TicketSplitter = "ts_score_effect",
         `TIP Persuasion Model` = "cate_score_effect") %>%
  gather(score, effect, `New Middle`:econ_pop_effect) %>%
  ggplot(aes(x=cutoff, y = effect, color = score)) +
  geom_hline(yintercept = 0, 
             color='darkgrey') +
  geom_hline(yintercept = score_effect_df$nm_score_effect[1],
             color='grey', 
             linetype=2) +
  geom_line(size=1) +
  theme_minimal() +
  labs(y = "Program Effect (pp)",
       x = "Score Percentile Cutoff") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
