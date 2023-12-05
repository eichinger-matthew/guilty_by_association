# -------------------- matching models for GUILTY BY ASSOCIATION --------------------

# setwd
setwd("C:/Users/eichi/Downloads/guilty_by_association_paper_copy")

# remove objects
rm(list = ls())


# ----- load libraries

# load libraries
library(tidyverse)
library(readxl)
library(countrycode)
library(broom)
library(MatchIt)
library(WeightIt)
library(marginaleffects)
library(tidymodels)
library(rpart)
library(rpart.plot)
library(caret)
library(vip)
library(ipred)
library(ggcorrplot)
library(cobalt)
library(ggpubr)

select <- dplyr::select


# LOAD DATA -------------------------------------------------------------

# load ess
ess <- 
  read_rds("ess_2014_factor_analysis_and_populism_info.rds") %>%
  mutate(ches_party = case_when(pop.b == 1 & auth.b == 1 ~ "authoritarian and populist",
                                pop.b == 1 & auth.b == 0 ~ "populist",
                                pop.b == 0 & auth.b == 1 ~ "authoritarian",
                                pop.b == 0 & auth.b == 0 ~ "mainstream"),
         c_auth_populist = ifelse(ches_party == "authoritarian and populist", 1, 0),
         c_populist = ifelse(ches_party == "populist", 1, 0)) %>%
  rename(income_decile = hinctnta)

# MATCHING LOOP --------------------------------------------

# ----- prepare different dataframes for modeling

# # auth
# auth.df <-
#   match_base_df %>%
#   select(auth.b, rand_id, cntry, edu_cat, agea, lrscale, log_lr.impexp.rat,
#          been_unemployed, occ_treat, imm_econ, imm_ethnic, hinctnta) %>%
#   rename(treat = occ_treat,
#          dv = auth.b) %>%
#   mutate(agea = as.numeric(scale(agea))) %>%
#   drop_na()
# 
# # pop df
# pop.df <-
#   match_base_df %>%
#   select(pop.b, rand_id, cntry, edu_cat, agea, lrscale, log_lr.impexp.rat,
#          been_unemployed, occ_treat, imm_econ, imm_ethnic, hinctnta) %>%
#   rename(treat = occ_treat,
#          dv = pop.b) %>%
#   mutate(agea = as.numeric(scale(agea))) %>%
#   drop_na()

# imm econ df
econ.df <-
  ess %>%
  select(ches_party, auth_liberal, pop_plural,
         rand_id, cntry, edu_cat, agea, lrscale, log_lr.impexp.rat,
         been_unemployed, occ_treat, imm_econ, imm_ethnic, income_decile) %>%
  rename(treat = occ_treat, dv = imm_econ) %>%
  mutate(agea = as.numeric(scale(agea)),
         ches_party = factor(ches_party)) %>%
  drop_na()

# pop df
ethnic.df <-
  ess %>%
  select(ches_party, auth_liberal, pop_plural,
         rand_id, cntry, edu_cat, agea, lrscale, log_lr.impexp.rat,
         been_unemployed, occ_treat, imm_econ, imm_ethnic, income_decile) %>%
  rename(treat = occ_treat, dv = imm_ethnic) %>%
  mutate(agea = as.numeric(scale(agea)),
         ches_party = factor(ches_party)) %>%
  drop_na()

# write matching function
match_seq <- function(df){
  
  df <- df
  
  # match model
  m.mod <- matchit(treat ~ edu_cat + agea + lrscale + 
                     log_lr.impexp.rat + been_unemployed + income_decile + 
                     ches_party,
                   data = df, method = "nearest", distance = "mahalanobis",
                   exact = ~edu_cat + been_unemployed + ches_party,
                   ratio = 2)
  
  # add weights
  m.df <- match.data(m.mod)
  
  # do love plots
  l.plot <- 
    love.plot(x = m.mod, 
              stats = c("m", "variance.ratios"),
              thresholds = c("m" = 0.1, "variance.ratios" = 1.25),
              abs = TRUE, var.order = "unadjusted")
  
 # linear model
  m.mod <- lm(
    dv ~ cntry + treat +
      treat*(edu_cat + agea + lrscale +
               log_lr.impexp.rat + been_unemployed + income_decile + 
               ches_party),
              data = m.df, weights = weights)
    
  # tidy data
  coefs <- tidy(m.mod) %>%
    mutate(estimate = round(estimate, 2),
           signif = ifelse(p.value < 0.05, 1, 0))
  
  # make list
  temp <- list(
    "prematch_data" = df,
    "match_model" = m.mod,
    "match_df" = m.df,
    "love_plot" = l.plot,
    "treat_mod" = m.mod,
    "coef_table" = coefs
  )
  
  # return
  return(temp)
}

# run
econ_mod <-
  match_seq(df = econ.df)

ethnic_mod <-
  match_seq(df = ethnic.df)




  
# POSTESTIMATION --------------------------------------------

# ---------- 1) conditional contrast plots for ethnic models

# contrast for econ model
plot_cco_ethnic_ches <-
  plot_cco(ethnic_mod$treat_mod, effect = "treat", condition = "ches_party") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "type of party feels closest to", y = "contrast effect of occupational exposure",
       title = "Conditional Contrast Plot for Ethnic Immigration Preferences",
       subtitle = "Effect of occupational exposure, by party type feels closest to")

# contrast for left-right
plot_cco_ethnic_leftright <-
  plot_cco(ethnic_mod$treat_mod, effect = "treat", condition = "lrscale") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "left-right ideology", y = "contrast effect of occupational exposure",
       title = "Conditional Contrast Plot for Ethnic Immigration Preferences",
       subtitle = "Effect of occupational exposure, by left-right ideology")

# contrast for import-export ratio
plot_cco_ethnic_impexp <-
  plot_cco(ethnic_mod$treat_mod, effect = "treat", condition = "log_lr.impexp.rat") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "(log) long-run import-export ratio", y = "contrast effect of occupational exposure",
       title = "Conditional Contrast Plot for Ethnic Immigration Preferences",
       subtitle = "Effect of occupational exposure, by long-run import-export ratio")


# get list for ethnic model
list_ethnic <-
  list(
    "matching_models" = ethnic_mod,
    "plots" = list(
      "contrast_chesparty" = plot_cco_ethnic_ches,
      "contrast_leftright" = plot_cco_ethnic_leftright,
      "contrast_impexp" = plot_cco_ethnic_impexp
    )
  )


# ---------- 1) conditional contrast plots for econ models

# contrast for econ model
plot_cco_econ_ches <-
  plot_cco(econ_mod$treat_mod, effect = "treat", condition = "ches_party") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "type of party feels closest to", y = "contrast effect of occupational exposure",
       title = "Conditional Contrast Plot for Economic Immigration Preferences",
       subtitle = "Effects greater than zero indicate a positive treatment effect")

# contrast for left-right
plot_cco_econ_leftright <-
  plot_cco(econ_mod$treat_mod, effect = "treat", condition = "lrscale") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "left-right ideology", y = "contrast effect of occupational exposure",
       title = "Conditional Contrast Plot for Economic Immigration Preferences",
       subtitle = "Effect of occupational exposure, by left-right ideology")

# contrast for import-export ratio
plot_cco_econ_impexp <-
  plot_cco(econ_mod$treat_mod, effect = "treat", condition = "log_lr.impexp.rat") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "(log) long-run import-export ratio", y = "contrast effect of occupational exposure",
       title = "Conditional Contrast Plot for Economic Immigration Preferences",
       subtitle = "Effect of occupational exposure, by long-run import-export ratio")


# get list for ethnic model
list_econ <-
  list(
    "matching_models" = econ_mod,
    "plots" = list(
      "contrast_chesparty" = plot_cco_econ_ches,
      "contrast_leftright" = plot_cco_econ_leftright,
      "contrast_impexp" = plot_cco_econ_impexp
    )
  )



# SAVING -----------------------------------------



# make list of lists
matching_res <-
  list(
    "ethnic_models" = list_ethnic,
    "econ_models" = list_econ
  )


save(matching_res, file = "final_matching_models.RData")







