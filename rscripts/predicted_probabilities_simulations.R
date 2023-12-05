# -------------------- simulations for GUILTY BY ASSOCIATION --------------------

# setwd
setwd("C:/Users/eichi/Downloads/guilty_by_association_paper")

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
ess <- read_rds("./internal_data/ess_2014_data_for_analysis.rds") %>%
  mutate(across(c(auth_liberal, pop_plural),
                ~as.numeric(.x)),
         trade_treat = 
           case_when(longrun_import_export_ratio > 3/2 ~ 1,
                     longrun_import_export_ratio <= 2/3 ~ 0),
         country_full = 
           countrycode(cntry,
                       "iso2c",
                       "country.name")) %>%
  group_by(cntry) %>%
  mutate(econ_restrict_bc_bin =
           ifelse(acceptance_for_econ_qual_by_country >=
                    quantile(acceptance_for_econ_qual_by_country,
                             probs = 0.75,
                             na.rm = TRUE),
                  1,
                  0),
         ethnic_restrict_bc_bin =
           ifelse(acceptance_for_ethnic_race_by_country >=
                    quantile(acceptance_for_ethnic_race_by_country,
                             probs = 0.75,
                             na.rm = TRUE),
                  1,
                  0)) %>%
  ungroup() %>%
  filter(cat_pop_plural != "0",
         major_occupation_label != "Armed Forces Occupations")









# SIMULATIONS ---------------------------------------------------


library(MASS)

# write function to get predicted values
pred_probs <- function(mod, n, df_stereo){
  
  # covariance matrix of parameters
  cov_mat <- vcov(mod)
  
  # point estimates for parameters
  mu_mat <- as.vector(matrix(mod$coefficients))
  
  # define storage object for results
  out_vec <- vector(length = n)
  
  # randomly sample n draws from multivariate normal
  coef_hat <- mvrnorm(n = n,
                      mu = mu_mat, 
                      Sigma = cov_mat)
  
  # calculate linear predictor
  # nxk times kx1
  lin_pred <-
    coef_hat %*% t(df_stereo) + 
    rnorm(n = n, mean = mean(mod$residuals),
          sd = sd(mod$residuals))
  
  if(class(mod)[1] == "glm"){
    
    # transform response to 0-1
    out_vec <- inv.logit(lin_pred)
    
  } else{
    
    out_vec <- lin_pred
    
  }
  
  # get estimate and conf intervals
  temp <- out_vec %>%
    as.data.frame() %>%
    rename(estimate = 1) %>%
    summarise(est_mean = mean(estimate),
              high_ci = est_mean + 1.96*(sd(estimate)/sqrt(n)),
              low_ci = est_mean - 1.96*(sd(estimate)/sqrt(n)))
  
  # calculate values of interest
  return(temp)
  
}


# ---------- trade exposure model

# compare sweden and britain

# vary the immigration variable
# begin from model data

britain_treat_yes <-
  data.frame("(Intercept)" = 1, "cntryBE" = 0, "cntryCH" = 0,                               
             "cntryDE" = 0, "cntryDK" = 0, "cntryES" = 0,                               
             "cntryFI" = 0, "cntryFR" = 0, "cntryGB" = 1,                               
             "cntryHU" = 0, "cntryIE" = 0, "cntryNL" = 0,                               
             "cntryNO" = 0, "cntryPL" = 0, "cntryPT" = 0,                               
             "cntrySE" = 0, "trade_treat" = 1, "lrscale_catliberal" = 0,                    
             "lrscale_catmoderate" = 0, "accept_econ_decile" = 4,                    
             "accept_ethnic_decile" = 4, "edu_catno college" = 1,                     
             "edu_catvocational" = 0, "major_occclerical support" = 0,             
             "major_occcraft and trades" = 1, "major_occelementary" = 0,                   
             "major_occmanagers" = 0, "major_occprofessionals" = 0,                
             "major_occservice and sales" = 0, "major_occtechs" = 0,                        
             "agea" = 50,  "hinctnta" = 5, "recent_unemployment" = 1,                   
             "trade_treat:lrscale_catliberal" = 0,        
             "trade_treat:lrscale_catmoderate" = 0,       
             "trade_treat:accept_econ_decile" = 4,        
             "trade_treat:accept_ethnic_decile" = 4,      
             "trade_treat:edu_catno college" = 1,         
             "trade_treat:edu_catvocational" = 0,         
             "trade_treat:major_occclerical support" = 0, 
             "trade_treat:major_occcraft and trades" = 1, 
             "trade_treat:major_occelementary" = 0,       
             "trade_treat:major_occmanagers" = 0,         
             "trade_treat:major_occprofessionals" = 0,    
             "trade_treat:major_occservice and sales" = 0,
             "trade_treat:major_occtechs" = 0,            
             "trade_treat:agea" = 50,                      
             "trade_treat:hinctnta" = 5,                  
             "trade_treat:recent_unemployment" = 1)

britain_treat_no <-
  data.frame("(Intercept)" = 1, "cntryBE" = 0, "cntryCH" = 0,                               
             "cntryDE" = 0, "cntryDK" = 0, "cntryES" = 0,                               
             "cntryFI" = 0, "cntryFR" = 0, "cntryGB" = 1,                               
             "cntryHU" = 0, "cntryIE" = 0, "cntryNL" = 0,                               
             "cntryNO" = 0, "cntryPL" = 0, "cntryPT" = 0,                               
             "cntrySE" = 0, "trade_treat" = 0, "lrscale_catliberal" = 0,                    
             "lrscale_catmoderate" = 0, "accept_econ_decile" = 4,                    
             "accept_ethnic_decile" = 4, "edu_catno college" = 1,                     
             "edu_catvocational" = 0, "major_occclerical support" = 0,             
             "major_occcraft and trades" = 1, "major_occelementary" = 0,                   
             "major_occmanagers" = 0, "major_occprofessionals" = 0,                
             "major_occservice and sales" = 0, "major_occtechs" = 0,                        
             "agea" = 50,  "hinctnta" = 5, "recent_unemployment" = 1,                   
             "trade_treat:lrscale_catliberal" = 0,        
             "trade_treat:lrscale_catmoderate" = 0,       
             "trade_treat:accept_econ_decile" = 0,        
             "trade_treat:accept_ethnic_decile" = 0,      
             "trade_treat:edu_catno college" = 0,         
             "trade_treat:edu_catvocational" = 0,         
             "trade_treat:major_occclerical support" = 0, 
             "trade_treat:major_occcraft and trades" = 0, 
             "trade_treat:major_occelementary" = 0,       
             "trade_treat:major_occmanagers" = 0,         
             "trade_treat:major_occprofessionals" = 0,    
             "trade_treat:major_occservice and sales" = 0,
             "trade_treat:major_occtechs" = 0,            
             "trade_treat:agea" = 0,                      
             "trade_treat:hinctnta" = 0,                  
             "trade_treat:recent_unemployment" = 0)


sweden_treat_yes <-
  data.frame("(Intercept)" = 1, "cntryBE" = 0, "cntryCH" = 0,                               
             "cntryDE" = 0, "cntryDK" = 0, "cntryES" = 0,                               
             "cntryFI" = 0, "cntryFR" = 0, "cntryGB" = 0,                               
             "cntryHU" = 0, "cntryIE" = 0, "cntryNL" = 0,                               
             "cntryNO" = 0, "cntryPL" = 0, "cntryPT" = 0,                               
             "cntrySE" = 1, "trade_treat" = 1, "lrscale_catliberal" = 0,                    
             "lrscale_catmoderate" = 0, "accept_econ_decile" = 4,                    
             "accept_ethnic_decile" = 4, "edu_catno college" = 1,                     
             "edu_catvocational" = 0, "major_occclerical support" = 0,             
             "major_occcraft and trades" = 1, "major_occelementary" = 0,                   
             "major_occmanagers" = 0, "major_occprofessionals" = 0,                
             "major_occservice and sales" = 0, "major_occtechs" = 0,                        
             "agea" = 50,  "hinctnta" = 5, "recent_unemployment" = 1,                   
             "trade_treat:lrscale_catliberal" = 0,        
             "trade_treat:lrscale_catmoderate" = 0,       
             "trade_treat:accept_econ_decile" = 4,        
             "trade_treat:accept_ethnic_decile" = 4,      
             "trade_treat:edu_catno college" = 1,         
             "trade_treat:edu_catvocational" = 0,         
             "trade_treat:major_occclerical support" = 0, 
             "trade_treat:major_occcraft and trades" = 1, 
             "trade_treat:major_occelementary" = 0,       
             "trade_treat:major_occmanagers" = 0,         
             "trade_treat:major_occprofessionals" = 0,    
             "trade_treat:major_occservice and sales" = 0,
             "trade_treat:major_occtechs" = 0,            
             "trade_treat:agea" = 50,                      
             "trade_treat:hinctnta" = 5,                  
             "trade_treat:recent_unemployment" = 1)

sweden_treat_no <-
  data.frame("(Intercept)" = 1, "cntryBE" = 0, "cntryCH" = 0,                               
             "cntryDE" = 0, "cntryDK" = 0, "cntryES" = 0,                               
             "cntryFI" = 0, "cntryFR" = 0, "cntryGB" = 0,                               
             "cntryHU" = 0, "cntryIE" = 0, "cntryNL" = 0,                               
             "cntryNO" = 0, "cntryPL" = 0, "cntryPT" = 0,                               
             "cntrySE" = 1, "trade_treat" = 0, "lrscale_catliberal" = 0,                    
             "lrscale_catmoderate" = 0, "accept_econ_decile" = 4,                    
             "accept_ethnic_decile" = 4, "edu_catno college" = 1,                     
             "edu_catvocational" = 0, "major_occclerical support" = 0,             
             "major_occcraft and trades" = 1, "major_occelementary" = 0,                   
             "major_occmanagers" = 0, "major_occprofessionals" = 0,                
             "major_occservice and sales" = 0, "major_occtechs" = 0,                        
             "agea" = 50,  "hinctnta" = 5, "recent_unemployment" = 1,                   
             "trade_treat:lrscale_catliberal" = 0,        
             "trade_treat:lrscale_catmoderate" = 0,       
             "trade_treat:accept_econ_decile" = 0,        
             "trade_treat:accept_ethnic_decile" = 0,      
             "trade_treat:edu_catno college" = 0,         
             "trade_treat:edu_catvocational" = 0,         
             "trade_treat:major_occclerical support" = 0, 
             "trade_treat:major_occcraft and trades" = 0, 
             "trade_treat:major_occelementary" = 0,       
             "trade_treat:major_occmanagers" = 0,         
             "trade_treat:major_occprofessionals" = 0,    
             "trade_treat:major_occservice and sales" = 0,
             "trade_treat:major_occtechs" = 0,            
             "trade_treat:agea" = 0,                      
             "trade_treat:hinctnta" = 0,                  
             "trade_treat:recent_unemployment" = 0)



# ----------- compute predicted values

# ----- populism

# britain treat
pred_populism_britain_treat <-
  pred_vals(mod = industry_treat_pop_out,
            n = 1000,
            df_stereo = britain_treat_yes) %>%
  mutate(treat = 1,
         dv = "populism",
         country = "britain")

# britain no treat
pred_populism_britain_treat_no <-
  pred_vals(mod = industry_treat_pop_out,
            n = 1000,
            df_stereo = britain_treat_no) %>%
  mutate(treat = 0,
         dv = "populism",
         country = "britain")


# sweden treat
pred_populism_sweden_treat <-
  pred_vals(mod = industry_treat_pop_out,
            n = 1000,
            df_stereo = sweden_treat_yes) %>%
  mutate(treat = 1,
         dv = "populism",
         country = "sweden")

# sweden no treat
pred_populism_sweden_treat_no <-
  pred_vals(mod = industry_treat_pop_out,
            n = 1000,
            df_stereo = sweden_treat_no) %>%
  mutate(treat = 0,
         dv = "populism",
         country = "sweden")


# group into df
pred_populism_df <-
  bind_rows(pred_populism_britain_treat,
            pred_populism_britain_treat_no,
            pred_populism_sweden_treat,
            pred_populism_sweden_treat_no)


# plot
pred_populism_df %>%
  ggplot(aes(x = as.factor(treat),
             y = est_mean,
             col = country,
             group = country)) +
  geom_point(aes(x = as.factor(treat),
                 y = est_mean)) +
  geom_errorbar(aes(x = as.factor(treat),
                    ymin = low_ci,
                    ymax = high_ci)) +
  labs(x = "trade exposure treat",
       y = "predicted ",
       title = "Predicted change preferred party populism",
       subtitle = "By country and left behind stereotype") +
  theme_minimal() +
  theme(axis.line = element_line())







# ----- nationalism

# britain treat
pred_nationalism_britain_treat <-
  pred_vals(mod = industry_treat_nat_out,
            n = 1000,
            df_stereo = britain_treat_yes) %>%
  mutate(treat = 1,
         dv = "nationalism")

# britain no treat
pred_nationalismm_britain_treat_no <-
  pred_vals(mod = industry_treat_nat_out,
            n = 1000,
            df_stereo = britain_treat_no) %>%
  mutate(treat = 0,
         dv = "nationalism")


# sweden treat
pred_nationalism_sweden_treat <-
  pred_vals(mod = industry_treat_nat_out,
            n = 1000,
            df_stereo = sweden_treat_yes) %>%
  mutate(treat = 1,
         dv = "nationalism")

# sweden no treat
pred_nationalism_sweden_treat_no <-
  pred_vals(mod = industry_treat_nat_out,
            n = 1000,
            df_stereo = sweden_treat_no) %>%
  mutate(treat = 0,
         dv = "nationalism")


# group into df
pred_nationalism_df <-
  bind_rows(pred_nationalism_britain_treat,
            pred_nationalism_britain_treat_no,
            pred_nationalism_sweden_treat,
            pred_nationalism_sweden_treat_no)














