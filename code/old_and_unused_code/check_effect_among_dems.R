ids <- combined_weighted %>% filter(dem==1) %>% pull(vb_voterbase_id) %>% paste(collapse = "', '")

query <- sql(paste0("select vb_voterbase_id, cate 
             FROM impactproject.persuasion_scores 
             WHERE vb_voterbase_id IN ('", ids, "')"))

dem_scores <- read_civis(query, database = "TMC") %>%
  as_tibble()

beepr::beep(sound=5)

dem_data <- combined_weighted %>%
  select(vb_voterbase_id, assignment, Qfav, weight) %>%
  inner_join(dem_scores)

dem_data %>%
  mutate(cate_cat = cut(cate, 
                        breaks = quantile(dem_data$cate, probs = c(0, .3, .7, 1)), 
                        include.lowest = TRUE, 
                        labels = c("low", "mid", "high"))) %>%
  mutate(Qfav = fct_collapse(Qfav, 
                             fav = c("Strongly Favorable", "Somewhat Favorable"),
                             unfav = c("Strongly Unfavorable", "Somewhat Unfavorable"),
                             dk = c("Don't Know", "Never Heard"))) %>%
  group_by(assignment, cate_cat, Qfav) %>%
  summarize(n = sum(weight)) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x=assignment, y = freq, fill = Qfav)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position="top",
        legend.title = element_blank()) +
  facet_wrap(~cate_cat) +
  geom_text(aes(label = if_else(freq >= 0.02, 
                                scales::percent(freq, accuracy = 1),
                                "")), 
            position = position_stack(vjust = 0.5, reverse = T)) +
  labs(x = "", y = "")
