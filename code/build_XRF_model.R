
# Load Packages -----------------------------------------------------------
library(TIPtools)
library(tidyverse)
library(here)
library(causalToolbox)

# Load Modeling Data Frame ------------------------------------------------
mod_df <- readRDS(here("output", "model_df.Rds"))

mod_df <- filter(mod_df, dem==0)

set.seed(32)

mod_df <- mod_df[sample(1:nrow(mod_df)), ]

# Set Up Cross Validation -------------------------------------------------
n <- nrow(mod_df)
folds <- 6
fold_size <- floor(n / folds)
mod_df <- mod_df[1:(folds*fold_size),]

mod_df$fold <- rep(1:folds, fold_size)
mod_df$cate <- NA

# Run Models --------------------------------------------------------------
non_feature_vars <- c("assignment", 
                      "Qfav_flipped_continous", 
                      "Qfav", 
                      "fold", 
                      "cate", 
                      "ts_score",
                      "vb_voterbase_id",
                      "weight")

for (this_fold in 1:folds) {
  
  print(this_fold)
  print(Sys.time())
  
  fold_df <- mod_df %>%
    filter(fold != this_fold)
  
  features <- fold_df %>%
    select(-non_feature_vars)
  w <- if_else(fold_df$assignment == "control", 0, 1)
  yobs <- if_else(grepl(pattern = "Unfav", x = fold_df$Qfav), 1, 0)
  
  tictoc::tic()
  mod <- X_RF(feat = features, tr = w, yobs = yobs)
  tictoc::toc()
  
  cates <- mod_df %>%
    filter(fold == this_fold) %>%
    select(-non_feature_vars) %>%
    EstimateCate(theObject = mod)
  
  mod_df$cate[mod_df$fold == this_fold] <- cates
  
  BRRR::skrrrahh(23)
}

saveRDS(object = mod_df, file = here("output", "df_with_cates.Rds"))


# Print Table of Cate Outputs ---------------------------------------------
mod_df %>%
  mutate(cate_cat = cut(cate, 
                        breaks = quantile(cate, probs = c(0,.2, .8, 1)),
                        labels = c("low", "mid", "high"),
                        include.lowest = T)) %>%
  filter(cate_cat == "low") %>%
  select(Qfav_flipped_continous, assignment) %>%
  table() %>%
  prop.table(margin=2) %>%
  round(digits = 3)