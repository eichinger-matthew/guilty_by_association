# -------------------- machine learning models for GUILTY BY ASSOCIATION --------------------

# setwd
setwd("C:/Users/eichi/Downloads/guilty_by_association_paper_copy")

# remove objects
rm(list = ls())


# ----- load libraries

# load libraries
library(tidyverse)
library(countrycode)
library(broom)
library(MatchIt)
library(marginaleffects)
library(tidymodels)
library(rpart)
library(rpart.plot)
library(caret)
library(vip)
library(ipred)
library(ggcorrplot)
library(cobalt)

# LOAD DATA -------------------------------------------------------------

# load ess
ess <- read_rds("./internal_data/ess_2014_factor_analysis_and_populism_info.rds") %>%
  mutate(trade_treat = 
           case_when(longrun_import_export_ratio > 3/2 ~ 1,
                     longrun_import_export_ratio <= 2/3 ~ 0),
         lrimpexp = log(longrun_import_export_ratio),
         country_full = countrycode(cntry, "iso2c", "country.name"),
         auth.b = ifelse(auth_liberal >= 75, 1, 0),
         pop.b = ifelse(pop_plural >= 75, 1, 0),
         gen_imm = general_imm_pref_by_country,
         econ_imm = acceptance_for_econ_qual_by_country,
         ethnic_imm = acceptance_for_ethnic_race_by_country) %>%
  rename(occ = major_occupation_label)

# MACHINE LEARNING MODELS --------------------------------------------------

set.seed(0924890)

# -------------------- classification trees

# predict several outcomes
# (1) whether an individual feels close to a party
# (2) given an individual feels close to a party, whether populist
# (3) given an individual feels close to a party, whether nationalist
# (4) economic immigration preferences
# (5) racial, ethnic, and religious immigration preferences


# ---------- authoritarian vote

auth_tree_sub_df <-
  ess %>%
  select(auth.b, gen_imm, econ_imm, ethnic_imm, agea, hinctnta, lrscale,
         occ, edu_cat, rlgdgr, contains("stf"),
         contains("trst"), ipcrtiv:iplylfr) %>%
  rename(age = agea,
         religious = rlgdgr,
         trust_parl = trstprl,
         trust_pols = trstplt,
         trust_legis = trstlgl,
         trust_prty = trstprt,
         trust_ppl = ppltrst,
         satisf_dem = stfdem,
         satisf_eco = stfeco,
         satisf_gov = stfgov,
         satisf_health = stfhlth,
         satisf_edu = stfedu) %>%
  data.frame()

# estimate tree
auth_sub_tree <-
  rpart(auth.b ~ .,
        data = auth_tree_sub_df,
        method = "class",
        control = rpart.control(cp = 0.0001))

# plot error as function of complexity param
plotcp(auth_sub_tree)

# find best complexity param
bestcp_auth_sub <- 
  auth_sub_tree$cptable[which.min(auth_sub_tree$cptable[,"xerror"]),"CP"]

# find visually useful complexity...
visuse_auth_sub <-
  auth_sub_tree$cptable[10,"CP"]

# get pruned tree
auth_sub_tree_pruned <- rpart::prune(auth_sub_tree, cp = visuse_auth_sub)

# plot pruned tree
rpart.plot(auth_sub_tree_pruned)


# get tree data
auth_tree_dat <-
  vip(auth_sub_tree,
      num_features = 15)$data %>%
  mutate(Importance = Importance/10000)

# plot tree
auth_tree_dat %>%
  ggplot(aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "steelblue") +
  labs(x = "importance",
       y = "",
       title = "Variable importance scores",
       subtitle = "Predicting authoritarianism of party choice") +
  theme_minimal() +
  theme(axis.line = element_line())





# ---------- populist vote

pop_tree_sub_df <-
  ess %>%
  select(pop.b, general_imm_pref_by_country, agea, hinctnta, lrscale,
         occ, edu_cat, rlgdgr, contains("stf"), contains("trst"), ipcrtiv:iplylfr) %>%
  rename(age = agea, religious = rlgdgr, trust_parl = trstprl,
         trust_pols = trstplt, trust_legis = trstlgl, trust_prty = trstprt,
         trust_ppl = ppltrst, satisf_dem = stfdem, satisf_eco = stfeco,
         satisf_gov = stfgov, satisf_health = stfhlth, satisf_edu = stfedu) %>%
  data.frame()

# estimate tree
pop_sub_tree <-
  rpart(pop.b ~ .,
        data = pop_tree_sub_df,
        method = "class",
        control = rpart.control(cp = 0.0001))


# plot error as function of complexity param
plotcp(pop_sub_tree)

# find best complexity param
bestcp_pop_sub <- 
  pop_sub_tree$cptable[which.min(pop_sub_tree$cptable[,"xerror"]),"CP"]

# find visually useful complexity...
visuse_pop_sub <-
  pop_sub_tree$cptable[10,"CP"]

# get pruned tree
pop_sub_tree_pruned <- rpart::prune(pop_sub_tree, cp = visuse_pop_sub)

# plot pruned tree
rpart.plot(pop_sub_tree_pruned)

# get tree data
pop_tree_dat <-
  vip(pop_sub_tree,
      num_features = 15)$data %>%
  mutate(Importance = Importance/10000)

# plot tree
pop_tree_dat %>%
  ggplot(aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "steelblue") +
  labs(x = "importance",
       y = "",
       title = "Variable importance scores",
       subtitle = "Predicting populism of party choice") +
  theme_minimal() +
  theme(axis.line = element_line())


# ---------- economic immigration preferences

# ----- regular variables

# prepare df
econ_tree_df <-
  ess %>%
  select(acceptance_for_econ_qual_by_country,
         cat_auth_liberal, cat_pop_plural,
         agea, hinctnta, lrscale,
         major_occupation_label,
         edu_cat, rlgdgr, uemp3m,
         log_import_exposure) %>%
  rename(econ_restrict = acceptance_for_econ_qual_by_country,
         authoritarianism = cat_auth_liberal,
         populism = cat_pop_plural,
         age = agea,
         income_dec = hinctnta,
         occ = major_occupation_label,
         edu = edu_cat,
         religious = rlgdgr,
         unemp_more_3m = uemp3m,
         imp_exposure = log_import_exposure) %>%
  data.frame()

# estimate tree
econ_tree <-
  rpart(econ_restrict ~ .,
        data = econ_tree_df,
        method = "anova",
        control = rpart.control(cp = 0.0001))

# ----- examine fit

# plot error as function of complexity param
plotcp(econ_tree)

# find best complexity param
bestcp <- econ_tree$cptable[which.min(econ_tree$cptable[,"xerror"]),"CP"]

# find visually useful complexity...
visuse_econ <-
  econ_tree$cptable[10,"CP"]

# get pruned tree
econ_tree_pruned <- rpart::prune(econ_tree, cp = visuse_econ)

# plot pruned tree
rpart.plot(econ_tree_pruned)


# get tree data
econ_tree_dat <-
  vip(econ_tree,
      num_features = 15)$data %>%
  mutate(Importance = Importance/10000)

# plot tree
econ_tree_dat %>%
  ggplot(aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "steelblue") +
  labs(x = "importance",
       y = "",
       title = "Variable importance scores",
       subtitle = "Predicting economic restrictionism") +
  theme_minimal() +
  theme(axis.line = element_line())







# ----- subjective perception variables

# prepare df
econ_tree_sub_df <-
  ess %>%
  select(acceptance_for_econ_qual_by_country,
         cat_auth_liberal, cat_pop_plural,
         agea, hinctnta, lrscale,
         major_occupation_label,
         edu_cat, rlgdgr, uemp3m,
         log_import_exposure,
         contains("stf"),
         contains("trst"),
         ipcrtiv:iplylfr) %>%
  rename(econ_restrict = acceptance_for_econ_qual_by_country,
         authoritarianism = cat_auth_liberal,
         populism = cat_pop_plural,
         age = agea,
         income_dec = hinctnta,
         occ = major_occupation_label,
         edu = edu_cat,
         religious = rlgdgr,
         unemp_more_3m = uemp3m,
         imp_exposure = log_import_exposure,
         trust_parl = trstprl,
         trust_pols = trstplt,
         trust_legis = trstlgl,
         trust_prty = trstprt,
         trust_ppl = ppltrst,
         trust_ep = trstep,
         satisf_dem = stfdem,
         satisf_eco = stfeco,
         satisf_gov = stfgov,
         satisf_health = stfhlth,
         satisf_edu = stfedu,
         imp_strong_gov = ipstrgv,
         imp_equal_opp = ipeqopt,
         imp_safe = impsafe) %>%
  data.frame()

# estimate tree
econ_tree_sub <-
  rpart(econ_restrict ~ .,
        data = econ_tree_sub_df,
        method = "anova",
        control = rpart.control(cp = 0.0001))



# ----- examine fit

# plot error as function of complexity param
plotcp(econ_tree_sub)

# find best complexity param
bestcp_sub <- econ_tree_sub$cptable[which.min(econ_tree_sub$cptable[,"xerror"]),"CP"]

# find visually useful complexity...
visuse_econ_sub <-
  econ_tree_sub$cptable[10,"CP"]

# get pruned tree
econ_tree_pruned_sub <- rpart::prune(econ_tree_sub, cp = visuse_econ_sub)

# plot pruned tree
rpart.plot(econ_tree_pruned_sub)

# get tree data
econ_tree_sub_dat <-
  vip(econ_tree_sub,
      num_features = 15)$data %>%
  mutate(Importance = Importance/10000)

# plot tree
econ_tree_sub_dat %>%
  ggplot(aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "steelblue") +
  labs(x = "importance",
       y = "",
       title = "Variable importance scores",
       subtitle = "Predicting economic restrictionism, with subjective beliefs") +
  theme_minimal() +
  theme(axis.line = element_line())



# ---------- racial, ethnic, and religious immigration preferences

# ----- regular variables

# prepare df
ethnic_tree_df <-
  ess %>%
  select(acceptance_for_ethnic_race_by_country,
         cat_auth_liberal, cat_pop_plural,
         agea, hinctnta, lrscale,
         major_occupation_label,
         edu_cat, rlgdgr, uemp3m,
         log_import_exposure) %>%
  rename(ethnic_restrict = acceptance_for_ethnic_race_by_country,
         authitarianism = cat_auth_liberal,
         populism = cat_pop_plural,
         age = agea,
         income_dec = hinctnta,
         occ = major_occupation_label,
         edu = edu_cat,
         religious = rlgdgr,
         unemp_more_3m = uemp3m,
         imp_exposure = log_import_exposure) %>%
  data.frame()

# estimate tree
ethnic_tree <-
  rpart(ethnic_restrict ~ .,
        data = ethnic_tree_df,
        method = "anova",
        control = rpart.control(cp = 0.0001))

# ----- examine fit

# plot error as function of complexity param
plotcp(ethnic_tree)

# find best complexity param
bestcp_ethnic <- ethnic_tree$cptable[which.min(ethnic_tree$cptable[,"xerror"]),"CP"]

# find visually useful complexity...
visuse_ethnic <-
  ethnic_tree$cptable[10,"CP"]

# get pruned tree
ethnic_tree_pruned <- rpart::prune(ethnic_tree, cp = visuse_ethnic)

# plot pruned tree
rpart.plot(ethnic_tree_pruned)


# get tree data
ethnic_tree_dat <-
  vip(ethnic_tree,
      num_features = 15)$data %>%
  mutate(Importance = Importance/10000)

# plot tree
ethnic_tree_dat %>%
  ggplot(aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "steelblue") +
  labs(x = "importance",
       y = "",
       title = "Variable importance scores",
       subtitle = "Predicting ethnic restrictionism") +
  theme_minimal() +
  theme(axis.line = element_line())




# ----- subjective perception variables

# prepare df
ethnic_tree_sub_df <-
  ess %>%
  select(acceptance_for_ethnic_race_by_country,
         cat_auth_liberal, cat_pop_plural,
         agea, hinctnta, lrscale,
         major_occupation_label,
         edu_cat, rlgdgr, uemp3m,
         log_import_exposure,
         contains("stf"),
         contains("trst"),
         ipcrtiv:iplylfr) %>%
  rename(ethnic_restrict = acceptance_for_ethnic_race_by_country,
         authoritarianism = cat_auth_liberal,
         populism = cat_pop_plural,
         age = agea,
         income_dec = hinctnta,
         occ = major_occupation_label,
         edu = edu_cat,
         religious = rlgdgr,
         unemp_more_3m = uemp3m,
         imp_exposure = log_import_exposure,
         trust_parl = trstprl,
         trust_pols = trstplt,
         trust_legis = trstlgl,
         trust_prty = trstprt,
         trust_ppl = ppltrst,
         trust_ep = trstep,
         satisf_dem = stfdem,
         satisf_eco = stfeco,
         satisf_gov = stfgov,
         satisf_health = stfhlth,
         satisf_edu = stfedu,
         imp_strong_gov = ipstrgv,
         imp_equal_opp = ipeqopt,
         imp_safe = impsafe) %>%
  data.frame()

# estimate tree
ethnic_tree_sub <-
  rpart(ethnic_restrict ~ .,
        data = ethnic_tree_sub_df,
        method = "anova",
        control = rpart.control(cp = 0.0001))



# ----- examine fit

# plot error as function of complexity param
plotcp(ethnic_tree_sub)

# find best complexity param
bestcp_ethnic_sub <- ethnic_tree_sub$cptable[which.min(ethnic_tree_sub$cptable[,"xerror"]),"CP"]

# find visually useful complexity...
visuse_ethnic_sub <-
  ethnic_tree_sub$cptable[10,"CP"]

# get pruned tree
ethnic_tree_pruned_sub <- rpart::prune(ethnic_tree_sub, cp = visuse_ethnic_sub)

# plot pruned tree
rpart.plot(ethnic_tree_pruned_sub)

# get tree data
ethnic_tree_sub_dat <-
  vip(ethnic_tree_sub,
      num_features = 15)$data %>%
  mutate(Importance = Importance/10000)

# plot tree
ethnic_tree_sub_dat %>%
  ggplot(aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "steelblue") +
  labs(x = "importance",
       y = "",
       title = "Variable importance scores",
       subtitle = "Predicting ethnic restrictionism, with subjective beliefs") +
  theme_minimal() +
  theme(axis.line = element_line())







# END ----------------------


