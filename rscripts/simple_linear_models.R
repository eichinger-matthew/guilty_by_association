# -------------------- simple linear models for GUILTY BY ASSOCIATION --------------------

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

# hard code select
select <- dplyr::select

# load ess
ess <- read_rds("./internal_data/ess_2014_factor_analysis_and_populism_info.rds") %>%
  mutate(ches_party = case_when(pop.b == 1 & auth.b == 1 ~ "authoritarian and populist",
                                pop.b == 1 & auth.b == 0 ~ "populist",
                                pop.b == 0 & auth.b == 1 ~ "authoritarian",
                                pop.b == 0 & auth.b == 0 ~ "mainstream"),
         c_auth_populist = ifelse(ches_party == "authoritarian and populist", 1, 0),
         c_populist = ifelse(ches_party == "populist", 1, 0))




# LINEAR REGRESSION MODELS ----------------------------------------

# -------------------- start simple, add more variables

# simple (1)
# descriptors (2)
# subjective prefs (3)

# ---------- simple

# authoritarian
simpleauth <-
  glm(c_auth_populist ~ cntry + imm_econ + imm_ethnic,
     data = ess, family = "binomial")

# populist
simplepop <-
  glm(c_populist ~ cntry + imm_econ + imm_ethnic,
     data = ess, family = "binomial")


# ---------- descriptors

# authoritarian
descriptorsauth <-
  glm(c_auth_populist ~ cntry + imm_econ + imm_ethnic + lrscale + agea +
        edu_cat + occ + hinctnta + log_lr.impexp.rat + offshorability +
        routine_task_intensity,
     data = ess, family = "binomial")

# populist
descriptorspop <-
  glm(c_populist ~ cntry + imm_econ + imm_ethnic + lrscale + agea +
       edu_cat + occ + hinctnta + log_lr.impexp.rat  + offshorability +
        routine_task_intensity,
     data = ess, family = "binomial")

# ---------- subjective beliefs

# authoritarian
subjectiveauth <-
  glm(c_auth_populist ~ cntry + imm_econ + imm_ethnic + lrscale + agea + 
        edu_cat + occ + hinctnta + log_lr.impexp.rat + offshorability +
        routine_task_intensity + stfdem + stfeco + trstplt,
     data = ess, family = "binomial")

# populist
subjectivepop <-
  glm(c_populist ~ cntry + imm_econ + imm_ethnic + lrscale + agea +
       edu_cat + occ + hinctnta + log_lr.impexp.rat + offshorability +
        routine_task_intensity + stfdem + stfeco + trstplt,
     data = ess, family = "binomial")

# SAVE RESULTS ---------------------------------

linmods <- list(
  "data" = ess,
  "simple_authoritarian" = simpleauth, 
  "simple_populist" = simplepop,
  "sociodem_authoritarian" = descriptorsauth, 
  "sociodem_populist" = descriptorspop,
  "subjective_authoritarian" = subjectiveauth, 
  "subjective_populist" = subjectivepop
)
save(linmods, file = "linear_model_results.RData")




# END ----------------------------------------

