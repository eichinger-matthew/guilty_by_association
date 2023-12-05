# ---------- data cleaning for GULITY BY ASSOCIATION ---------- #

# AUTHOR: A. Matthew Eichinger -----
# Date:                        ----- #
# Objective:                   ----- #




setwd("C:/Users/eichi/Downloads")


# ----- LIBRARIES
library(tidyverse)
library(readxl)
library(haven)
library(janitor)
library(MatchIt)
library(rpart)
library(rpart.plot)
library(caret)
library(mev)
library(boot)
library(countrycode)
library(clustMixType)
library(cobalt)
library(broom)
library(DiagrammeR)
library(marginaleffects)




# LOADING DATA ----------

  
# ---------- party populism data

# get list of parties in ess
parties_ess <- 
  read_csv("https://raw.githubusercontent.com/hdigital/partyfactsdata/master/import/essprtv/essprt-all.csv") %>%
  filter(country != "NIR")


# ---- ches

# norris and inglehart scores
ches_scores <-
  read_rds("pippa_inglehart_ches_party_scores_clean.rds") %>%
  mutate(ches_partyid = as.numeric(ches_partyid))


# external party ids
party_ids_ches <-
  read_csv("partyfacts-external-parties.csv") %>%
  filter(dataset_key == "ches") %>%
  mutate(dataset_party_id = as.numeric(dataset_party_id))
  
# join
ches_df <-
  party_ids_ches %>%
  left_join(ches_scores, by = c("dataset_party_id" = "ches_partyid"))


# merge ches and ess
ess_parties_with_ches <-
  parties_ess %>%
  left_join(ches_df, by = c("partyfacts_id")) %>%
  filter(str_detect(ess_variable, "prtcl")) %>%
  unite(col = "link_code",
        c(ess_cntry, essround, ess_party_id),
        sep = "_",
        remove = FALSE)





# ----- poppa

# poppa dataset
poppa <- read_rds("poppa_measures_of_eu_parties.rds") %>%
  mutate(nationalist = ifelse(median_nativism >= 7, 1, 0),
         middle_nationalist = ifelse(nationalist == 1 & 
                                       median_lroverall > 2 & 
                                       median_lroverall < 8, 1, 0),
         right_nationalist = ifelse(nationalist == 1 & median_lroverall >= 8, 1, 0),
         left_nationalist = ifelse(nationalist == 1 & median_lroverall <= 2, 1, 0),
         populist_not_nationalist = 
           ifelse(median_antielitism >= 7 & 
                    median_peoplecentrism >= 7 &
                    median_nativism < 7, 1, 0),
         populist_nationalist = 
           ifelse(median_antielitism >= 7 & 
                    median_peoplecentrism >= 7 &
                    median_nativism >= 7, 1, 0)) %>%
  rename(far_right_nationalist = right_nationalist,
         far_left_nationalist = left_nationalist)

  

# merge poppa and ess
ess_parties_with_poppa <-
  parties_ess %>%
  left_join(poppa, by = c("partyfacts_id"))


# filter to prtcl questions
ess_poppa_prtcl_questions <-
  ess_parties_with_poppa %>%
  filter(str_detect(ess_variable, "prtcl")) %>%
  unite(col = "link_code",
        c(ess_cntry, essround, ess_party_id),
        sep = "_",
        remove = FALSE) %>%
  select(link_code, ess_cntry, essround,
         ess_variable, ess_party_id,
         partyfacts_id, median_intradem:populist_nationalist)







# ----- trade and industry data

# ----- trade data

# one-digit industries
one_digit_trade <-
  read_rds("industry_exposure_un_comtrade_one_digit_industries.rds")  %>%
  mutate(eurostat_name = countrycode(reporter,
                                     origin = "country.name",
                                     destination = "eurostat"),
         eurostat_name = ifelse(eurostat_name == "UK",
                                "GB",
                                eurostat_name))


# two-digit industries
two_digit_trade <-
  read_rds("industry_exposure_un_comtrade_two_digit_industries.rds")  %>%
  mutate(eurostat_name = countrycode(reporter,
                                     origin = "country.name",
                                     destination = "eurostat"),
         eurostat_name = ifelse(eurostat_name == "UK",
                                "GB",
                                eurostat_name)) %>%
  select(-contains("baseline")) %>%
  ungroup() %>%
  filter(year >= 2000) %>%
  group_by(reporter, nace_two_digit_name) %>%
  mutate(industry_imports_baseline_2000 = 
           industry_imports[year == min(year, na.rm = TRUE)],
         industry_exports_baseline_2000 = 
           industry_exports[year == min(year, na.rm = TRUE)],
         import_export_ratio_2000 =
           industry_imports_baseline_2000/
           industry_exports_baseline_2000,
         import_export_ratio =
           industry_imports/
           industry_exports,
         longrun_import_export_ratio =
           import_export_ratio/
           import_export_ratio_2000) %>%
  mutate(longrun_trade_status = 
           case_when(longrun_import_export_ratio >= 2 |
                       is.infinite(longrun_import_export_ratio) ~ "very disadvantaged",
                     longrun_import_export_ratio >= 1 & 
                       longrun_import_export_ratio < 2 ~ "disadvantaged",
                     longrun_import_export_ratio >= 0.5 & 
                       longrun_import_export_ratio < 1 ~ "advantaged",
                     longrun_import_export_ratio >= 0 &
                       longrun_import_export_ratio < 0.5 ~ "very advantaged",
                     is.nan(longrun_import_export_ratio) &
                       industry_imports_baseline_2000 == 0 ~ "not imported",
                     is.nan(longrun_import_export_ratio) &
                       industry_exports_baseline_2000 == 0 ~ "not exported"),
         present_trade_status = 
           case_when(import_export_ratio >= 2 ~ "very disadvantaged",
                     import_export_ratio >= 1 & 
                       import_export_ratio < 2 ~ "disadvantaged",
                     import_export_ratio >= 0.5 &
                       import_export_ratio < 1 ~ "advantaged",
                     import_export_ratio >= 0 &
                       import_export_ratio < 0.5 ~ "very advantaged"),
         ordinal_longrun_trade_status = 
           case_when(longrun_trade_status == "very disadvantaged" ~ 1,
                     longrun_trade_status == "disadvantaged" ~ 2,
                     longrun_trade_status == "not imported" |
                       longrun_trade_status == "not exported" ~ 3,
                     longrun_trade_status == "advantaged" ~ 4,
                     longrun_trade_status == "very advantaged" ~ 5))
  


# ----- nuts3 variables
nuts3 <-
  read_rds("eurostat_nuts3_crime_population_employment_gdp.rds") %>%
  mutate(year = as.character(year),
         log_gdp = log(gdp),
         robberies_per_1000 = (1000*robbery)/population) %>%
  dplyr::select(-geo_label) %>%
  group_by(geo) %>%
  mutate(employment_2008 = employment_rate[year == "2008"],
         employment_change_since_2008 = employment_rate - employment_2008,
         year = as.numeric(year)) %>%
  ungroup() %>%
  dplyr::select(-c(intentional_homicide:theft_of_a_motorized_land_vehicle))



# ----- industry data

nace2_info <-
  read_xlsx("nace_revision_2.xlsx") %>%
  filter(str_detect(product_code, "\\.")) %>%
  separate(product_code,
           into = c("nace2_product_code",
                    "nace2_product_name"),
           sep = " ",
           extra = "merge") %>%
  filter(str_length(nace2_product_code) == 5) %>%
  mutate(nace2_product_name = 
           str_remove_all(nace2_product_name, "[0-9]"),
         nace_two_digit_name = 
           str_to_lower(nace_two_digit_name)) %>%
  dplyr::select(nace_two_digit_code, nace_two_digit_name) %>%
  distinct()




# ----- occupations data

# some double-ups that only get distinguished by label
# need to make distinct, label will get dropped anyway
occs <-
  read_csv("isco08_english.csv") %>%
  mutate(unit = as.character(unit)) %>%
  arrange(unit) %>%
  mutate(across(.cols = c(minor, unit),
                .fns = ~str_replace_all(.x, "[X,Y]", "0"))) %>%
  dplyr::select(major) %>%
  distinct()




# ----- nuts employment data

# nuts employment
nuts_emp <-
  read_csv("employment_by_industry_nuts2.csv") %>%
  filter(!str_detect(geo, "EU"),
         !str_detect(geo, "ZZ"),
         !str_detect(geo, "EA"),
         str_length(geo) == 4,
         year == 2000) %>%
  mutate(employees = as.numeric(employees),
         country = str_sub(geo, 1, 2)) %>%
  group_by(year, geo) %>%
  mutate(total_local_employment = employees[nace_r1 == "TOTAL"],
         share_of_total_local_employment = employees/total_local_employment) %>%
  filter(nace_r1 != "NRP",
         nace_r1 != "TOTAL") %>%
  group_by(year, country, nace_r1) %>%
  mutate(total_national_industry_employment = 
           sum(employees, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(nace_r1 == "A_B" |
           nace_r1 == "C-E" |
           nace_r1 == "F" |
           nace_r1 == "G-Q")

# un comtrade
trade_df_trade_exposure <-
  read_rds("industry_exposure_un_comtrade_one_digit_industries.rds") %>%
  mutate(eurostat_nacer2 = 
           case_when(nace_broad_category == "A" |
                       nace_broad_category == "B" ~ "A_B",
                     nace_broad_category == "C" |
                       nace_broad_category == "D" |
                       nace_broad_category == "E" ~ "C-E",
                     nace_broad_category == "F" ~ "F",
                     nace_broad_category == "J" |
                       nace_broad_category == "M" |
                       nace_broad_category == "R" |
                       nace_broad_category == "S" ~ "G-Q")) %>%
  filter(year == 2000 | year == 2014) %>%
  dplyr::select(year, reporter, eurostat_nacer2, 
                industry_imports, industry_exports) %>%
  group_by(year, reporter, eurostat_nacer2) %>%
  mutate(industry_imports = sum(industry_imports, na.rm = TRUE),
         industry_exports = sum(industry_exports, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct() %>%
  mutate(eurostat_id = countrycode(reporter,
                                   origin = "country.name",
                                   destination = "eurostat")) %>%
  filter(!is.na(eurostat_nacer2)) %>%
  pivot_wider(names_from = year,
              values_from = c(industry_imports, industry_exports)) %>%
  mutate(import_change = industry_imports_2014 - industry_imports_2000,
         export_change = industry_exports_2014 - industry_exports_2000)
  



# join nuts2 to trade
nuts_emp_trade <-
  nuts_emp %>%
  left_join(trade_df_trade_exposure,
            by = c("country" = "eurostat_id",
                   "nace_r1" = "eurostat_nacer2")) %>%
  mutate(import_exposure = 
           share_of_total_local_employment*
           (import_change/total_national_industry_employment)) %>%
  group_by(geo) %>%
  mutate(nuts2_import_exposure = sum(import_exposure, na.rm = TRUE),
         nuts2_import_exposure_per_local_worker = 
           nuts2_import_exposure/total_local_employment) %>%
  ungroup() %>%
  dplyr::select(country, geo, contains("nuts2")) %>%
  distinct() %>%
  filter(!is.na(nuts2_import_exposure_per_local_worker),
         nuts2_import_exposure_per_local_worker != 0) %>%
  mutate(log_import_exposure = 
           log(nuts2_import_exposure_per_local_worker),
         country = str_replace(country, "UK", "GB"),
         geo = str_replace(geo, "UK", "GB"))
        
  





# ---------- european social survey


# ess version meant for the ideal point model
ess_seventh_full <-
  read_dta("ESS7e02_2.dta")  %>%
  mutate(rand_id = row_number(),
         pplstrd = case_when(pplstrd > 0 & pplstrd < 7 ~ (1-pplstrd+5)),
         smegbli = case_when(smegbli == 2 ~ 0,
                             smegbli == 1 ~ 1),
         smegbhw = case_when(smegbhw == 2 ~ 0,
                             smegbhw == 1 ~ 1),
         smctmbe = case_when(smctmbe == 2 ~ 0,
                             smctmbe == 1 ~ 1),
         imdetbs = ifelse(imdetbs < 0 | imdetbs > 10, NA, imdetbs),
         imdetmr = ifelse(imdetmr < 0 | imdetmr > 10, NA, imdetmr),
         across(.cols = c(imbgeco, imueclt, imwbcnt, imtcjob, imbleco,
                          imwbcrm, lwdscwp, rlgueim),
                .fns = ~case_when(.x >= 0 & .x <= 10 ~ (.x - 10)*-1)),
         across(.cols = c(contains("stf"),
                          contains("trst")),
                .fns = ~ifelse(.x >= 0 & .x <= 10, .x, NA)),
         across(.cols = c(ipcrtiv:impfun),
                .fns = ~ifelse(.x < 1 | .x > 6, NA, .x)),
         across(.cols = c(imbgeco, imueclt, imwbcnt, imtcjob, imbleco,
                          imwbcrm, imdetbs, imdetmr, lwdscwp, rlgueim,
                          contains("qfim")),
                .fns = ~case_when(.x >= 0 & .x <= 4 ~ 0,
                                  .x == 5 ~ 1,
                                  .x >= 6 & .x <= 10 ~ 2),
                .names = "cat_{.col}"),
         across(.cols = c(imsmetn, imdfetn, impcntr, eimpcnt, aljewlv, almuslv, algyplv),
                .fns = ~case_when(.x == 1 ~ 0,
                                  .x == 2 | .x == 3 ~ 1,
                                  .x == 4 ~ 2),
                .names = "cat_{.col}"))
  


# merge with ess
# adjust the sums that get zero (they are really NAs)
ess_seventh_main <-
  read_dta("ESS7e02_2.dta")  %>%
  mutate(rand_id = row_number()) %>%
  dplyr::select(-contains("prvt")) %>%
  filter(clsprty == 1 | clsprty == 2) %>%
  mutate(party_closest_to = rowSums(across(contains("prtcl")), na.rm = TRUE)) %>%
  dplyr::select(-contains("prtcl")) %>%
  mutate(party_closest_to = 
           ifelse(party_closest_to == 66 |
                    party_closest_to == 77 |
                    party_closest_to == 88 |
                    party_closest_to == 99 |
                    party_closest_to == 0,
                  NA, party_closest_to)) %>%
  filter(!is.na(party_closest_to)) %>%
  unite(c(cntry, essround, party_closest_to),
        col = "link_code",
        sep = "_",
        remove = FALSE) %>%
  left_join(ess_poppa_prtcl_questions,
            by = c("link_code")) %>%
  left_join(ess_parties_with_ches,
            by = c("link_code")) %>%
  mutate(lrscale = ifelse(lrscale < 0 | lrscale > 10, NA, lrscale),
         agea = ifelse(agea < 18, NA, agea),
         hinctnta = ifelse(hinctnta < 1 | hinctnta > 10, NA, hinctnta),
         eisced = ifelse(eisced < 1 | eisced > 7, NA, eisced),
         ctzcntr = ifelse(ctzcntr < 1 | ctzcntr > 2, NA, ctzcntr),
         rlgdgr = ifelse(rlgdgr < 0 | rlgdgr > 10, NA, rlgdgr),
         uemp3m = ifelse(uemp3m < 1 | uemp3m > 2, NA, uemp3m),
         uemp12m = ifelse(uemp12m < 1 | uemp12m > 2, NA, uemp12m),
         uemp3m = ifelse(uemp3m == 2, 0, 1),
         uemp12m = ifelse(uemp12m == 2, 0, 1),
         uempla = ifelse(uempla == 1, 1, 0),
         blgetmg = ifelse(blgetmg > 2, NA, blgetmg),
         blgetmg = ifelse(blgetmg == 1, 1, 0),
         nacer2 = ifelse(nacer2 == 666 | nacer2 == 777 | nacer2 == 888,
                         NA,
                         nacer2),
         nacer2 = ifelse(str_length(nacer2) == 1,
                         str_pad(nacer2, width = 2,
                                 side = "left", pad = "0"),
                         nacer2),
         major_occupation_group = str_sub(isco08, start = 1, end = 1),
         sub_major_occupation_group = str_sub(isco08, start = 1, end = 2),
         minor_occupation_group = str_sub(isco08, start = 1, end = 3),
         sub_minor_occupation_group = str_sub(isco08, start = 1, end = 4),
         across(.cols = c(major_occupation_group, sub_major_occupation_group,
                          minor_occupation_group, sub_minor_occupation_group),
                .fns = ~as.character(.x)),
         major_occupation_label =
           case_when(major_occupation_group == "1" ~ "Managers",
                     major_occupation_group == "2" ~ "Professionals",
                     major_occupation_group == "3" ~ "Technicians and Associate Professionals",
                     major_occupation_group == "4" ~ "Clerical Support Workers",
                     major_occupation_group == "5" ~ "Service and Sales Workers",
                     major_occupation_group == "6" ~ "Skilled Agricultural, Forestry, and Fishery Workers",
                     major_occupation_group == "7" ~ "Craft and Related Trades Workers",
                     major_occupation_group == "8" ~ "Plant and Machine Operators, and Assemblers",
                     major_occupation_group == "9" ~ "Elementary Occupations",
                     major_occupation_group == "0" ~ "Armed Forces Occupations")) %>%
  mutate(year = 2014,
         lrscale_cat = case_when(lrscale >= 0 & lrscale < 5 ~ "liberal",
                                 lrscale == 5 ~ "moderate",
                                 lrscale > 5 ~ "conservative"),
         age_cat = case_when(agea >= 18 & agea <= 25 ~ "young",
                             agea >= 26 & agea <= 45 ~ "young adult",
                             agea >= 46 & agea <= 62 ~ "middle age",
                             agea >= 63 ~ "elderly"),
         hinctnta_cat = 
           case_when(hinctnta <= 2 ~ "lower 20",
                     hinctnta > 2 & hinctnta < 8 ~ "mid 60",
                     hinctnta >= 8 & hinctnta <= 10 ~ "upper 20"),
         eisced = ifelse(str_detect(eisced, "NA") |
                           str_detect(eisced, "55"),
                         NA,
                         eisced),
         eisced = as.numeric(eisced),
         edu_cat = case_when(eisced <= 4 ~ "no college",
                             eisced == 5 ~ "vocational",
                             eisced > 5 ~ "college"),
         religious_cat =
           case_when(rlgdgr < 5 ~ "not very religious",
                     rlgdgr  == 5 ~ "somewhat religious",
                     rlgdgr > 5 ~ "religious"),
         citizen = case_when(ctzcntr == 1 ~ "yes",
                             ctzcntr == 2 ~ "no"),
         freehms = ifelse(str_detect(freehms, "NA"),
                          NA,
                          freehms),
         freehms = as.integer(freehms),
         euftf = ifelse(str_detect(euftf, "NA"),
                        NA,
                        euftf),
         euftf = as.numeric(euftf),
         across(.cols = c(imbgeco, imueclt, imwbcnt,
                          imtcjob, imbleco,
                          imwbcrm, imdetbs, imdetmr,
                          lwdscwp, rlgueim),
                .fns = ~case_when(.x >= 0 & .x <= 10 ~ (.x - 10)*-1)),
         across(.cols = c(contains("stf"),
                          contains("trst")),
                .fns = ~ifelse(.x >= 0 & .x <= 10,
                               .x,
                               NA)),
         across(.cols = c(ipcrtiv:impfun),
                .fns = ~ifelse(.x < 1 | .x > 6,
                               NA,
                               .x)),
         across(.cols = c(imbgeco, imueclt, imwbcnt,
                          imtcjob, imbleco,
                          imwbcrm, imdetbs, imdetmr,
                          lwdscwp, rlgueim,
                          contains("qfim")),
                .fns = ~case_when(.x >= 0 & .x <= 4 ~ 0,
                                  .x == 5 ~ 1,
                                  .x >= 6 & .x <= 10 ~ 2),
                .names = "cat_{.col}"),
         across(.cols = c(imsmetn, imdfetn, impcntr,
                          eimpcnt, aljewlv, almuslv, algyplv),
                .fns = ~case_when(.x == 1 ~ 0,
                                  .x == 2 | .x == 3 ~ 1,
                                  .x == 4 ~ 2),
                .names = "cat_{.col}")) %>%
  zap_labels(.) %>%
  left_join(nuts3,
            by = c("region" = "geo",
                   "year" = "year")) %>%
  left_join(two_digit_trade,
            by = c("cntry" = "eurostat_name",
                   "nacer2" = "nace_two_digit_code",
                   "year" = "year")) %>%
  dplyr::select(-nace_two_digit_name) %>%
  left_join(occs,
            by = c("major_occupation_group" = "major")) %>%
  left_join(nace2_info,
            by = c("nacer2" = "nace_two_digit_code")) %>%
  left_join(nuts_emp_trade,
            by = c("cntry" = "country",
                   "region" = "geo"))




# CLEANING DATA ------------------------------------------------------------




# MODELING DATA ------------------------------------------------------------


# ---------------------------------------- FACTOR ANALYSIS


# factor analysis test
library(psych)
ess_seventh_imm <-
  ess_seventh_full %>%
  dplyr::select(rand_id, cntry, qfimedu:qfimcmt,
                pplstrd, gvrfgap, smegbli, smegbhw, smctmbe,
                imbgeco, imueclt, imwbcnt, imtcjob, imbleco,
                imwbcrm, imdetbs, imdetmr, lwdscwp, rlgueim)





# -------------------- all countries


# estimate
fanal_test <-
  fa(ess_seventh_imm[,-c(1,2)], nfactors = 4, rotate = "oblimin")

# check structure
fanal_test$Vaccounted
fanal_test$Structure
fanal_test$loadings

# get df of loadings
nquests <- ncol(ess_seventh_imm[,-c(1,2)])
num_factors <- 4

# make table of factor analysis results
factor_info <-
  as.data.frame(fanal_test$loadings[1:nquests,1:num_factors]) %>%
  rename(general_imm_pref = MR1,
         acceptance_for_econ_qual = MR2,
         acceptance_for_ethnic_race = MR3,
         racism = MR4) %>%
  mutate(across(.cols = everything(),
                .fns = ~ifelse(.x < 0.1 & .x > -0.1, 0, .x)),
         across(.cols = everything(),
                .fns = ~ifelse(.x == 0, NA, .x)),
         survey_question = row.names(.),
         across(.cols = everything())) %>%
  remove_rownames() %>%
  arrange(survey_question) %>%
  select(survey_question, everything())





# factor 1 is general approval
# factor 2 is acceptance, given good economic qualifications
# factor 3 is racial and ethnic control over immigrants
# factor 4 is general racism

# get scores
fscores <- as.data.frame(fanal_test$scores)

# assign to df
ess_seventh_fscores <-
  ess_seventh_full %>%
  bind_cols(fscores) %>%
  rename(general_imm_pref = MR1,
         acceptance_for_econ_qual = MR2,
         acceptance_for_ethnic_race = MR3,
         racism = MR4)



# make faceted figures of second and third factors, by country and political beliefs
# do not include austria, ess did not ask questions about boss and marriage
facet_plot_econ_factors <-
  ess_seventh_fscores %>%
  mutate(lrscale_cat = 
           case_when(lrscale < 5 ~ "liberal",
                     lrscale == 5 ~ "moderate",
                     lrscale > 5 ~ "conservative"),
         cntry_full = countrycode(cntry,
                                  "iso2c",
                                  "country.name")) %>%
  filter(lrscale_cat != "moderate",
         !is.na(lrscale_cat),
         cntry != "AT") %>%
  ggplot(aes(x = acceptance_for_econ_qual,
             group = lrscale_cat,
             fill = lrscale_cat)) +
  geom_density(alpha = 0.6) +
  scale_fill_discrete(name = "Left-Right") +
  lims(x = c(-3, 3)) +
  facet_wrap(~cntry_full)


# same, but for cultural
facet_plot_cultural_factors <-
  ess_seventh_fscores %>%
  mutate(lrscale_cat = 
           case_when(lrscale < 5 ~ "liberal",
                     lrscale == 5 ~ "moderate",
                     lrscale > 5 ~ "conservative"),
         cntry_full = countrycode(cntry,
                                  "iso2c",
                                  "country.name")) %>%
  filter(lrscale_cat != "moderate",
         !is.na(lrscale_cat),
         cntry != "AT") %>%
  ggplot(aes(x = acceptance_for_ethnic_race,
             group = lrscale_cat,
             fill = lrscale_cat)) +
  geom_density(alpha = 0.6) +
  scale_fill_discrete(name = "Left-Right") +
  lims(x = c(-3, 3)) +
  facet_wrap(~cntry_full)







# ----- loop for elbow plot chart

# loop to get explained variance as number of factors changes
fa_store <- list()
n_factors <- seq(1, 10, 1)
for(i in seq_along(n_factors)){
  
  # do model
  temp <- 
    fa(ess_seventh_imm[,-c(1,2)], nfactors = n_factors[i], rotate = "oblimin")
  
  
  # for when there is only one latent factor
  if(i == 1){
    
    var_exp <- max(temp$Vaccounted[2,])
    
    # store
    fa_store[[i]] <- var_exp
    
    
  } else{

  # get cumulative variance
  var_exp <- max(temp$Vaccounted[3,])
  
  # store
  fa_store[[i]] <- var_exp
  
  }
  
  
}


# unravel into df
fanal_elbow_df <-
  do.call(rbind, fa_store) %>%
  as.data.frame() %>%
  rename(explained_var = 1) %>%
  mutate(num_factors = seq(1, 10, 1),
         inv_explained_var = 1-explained_var)

# save elbow plot
fanal_elbow_plot <-
  fanal_elbow_df %>%
  ggplot(aes(x = as.integer(num_factors), y = explained_var)) +
  geom_point(col = "#1f77b4") +
  geom_line(col = "#1f77b4") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  labs(x = "number of factors",
       y = "share of total variance explaned",
       title = "Exploratory Factor Analysis",
       subtitle = "Testing for Best Representation of the Data") +
  theme_minimal() +
  theme(axis.line = element_line())





# ------------------------------- by country


# create loop index
countries <- unique(ess_seventh_imm$cntry)
fa_bycountry_store <- list()
for(i in seq_along(countries)){
  
  # get data
  tempdf <-
    ess_seventh_imm %>%
    filter(cntry == countries[i]) %>%
    remove_empty(which = "cols")
  
  # estimate model
  tempmod <- 
    fa(tempdf[-c(1,2)], nfactors = 4, rotate = "oblimin")
  
  # get scores
  tempscores <- as.data.frame(tempmod$scores) %>%
    rename(general_imm_pref = MR1,
           acceptance_for_econ_qual = MR2,
           acceptance_for_ethnic_race = MR3,
           racism = MR4)
  
  # construct tibble
  temptibble <-
    tibble(rand_id = tempdf$rand_id,
           cntry = tempdf$cntry) %>%
    bind_cols(tempscores)
  
  # store
  fa_bycountry_store[[i]] <- temptibble
  
}


# ----- organize

combined_fa_df <-
  do.call(rbind, fa_bycountry_store) %>%
  as.data.frame() %>%
  rename(general_imm_pref_by_country = general_imm_pref,
         acceptance_for_econ_qual_by_country = acceptance_for_econ_qual,
         acceptance_for_ethnic_race_by_country = acceptance_for_ethnic_race,
         racism_by_country = racism)


# join to original data
ess_seventh_fscores_full <-
  ess_seventh_fscores %>%
  left_join(combined_fa_df, by = c("rand_id", "cntry"))


# check
ess_seventh_fscores_full %>%
  select(cntry, general_imm_pref:racism_by_country) %>%
  view()








# -------------------- latent immigration preferences

# immigration-only df
# ess_seventh_imm <-
#   ess_seventh_full %>%
#   dplyr::select(rand_id, cntry, pplstrd, gvrfgap, 
#                 smegbli, smegbhw, smctmbe,
#                 contains("cat_"))
# 
# 
# 
# # --------------- country by country approach
# 
# library(mirt)
# 
# # index and storage
# cntry_index <-
#   unique(ess_seventh_imm$cntry)
# 
# 
# cntry_store <- list()
# 
# 
# 
# for(j in 1:length(cntry_index)){
# 
#   # data
#   temp_df <-
#     ess_seventh_imm %>%
#     filter(cntry == cntry_index[j]) %>%
#     remove_empty(which = "cols")
# 
#   # model
#   temp_mirt <-
#     mirt(data = temp_df[,c(3:28)],
#          model = 2,
#          itemtype = "gpcm",
#          verbose = TRUE)
# 
#   # gather parameter results
#   temp_param_ests <-
#     as.data.frame(temp_mirt@Fit$F) %>%
#     mutate(feature = row.names(.)) %>%
#     remove_rownames()
# 
# 
#   # get individual traits
#   temp_ind_traits <-
#     fscores(temp_mirt)
# 
# 
#   # add traits back
#   temp_df_with_traits <-
#     temp_df %>%
#     cbind(temp_ind_traits)
# 
# 
#   # store
#   cntry_store[[j]] <-
#     list(temp_df,
#          temp_mirt,
#          temp_param_ests,
#          temp_ind_traits,
#          temp_df_with_traits)
# 
# 
# 
# }
# 
# 
# 
# # unravel stored data
# df_store <- list()
# for(k in 1:length(cntry_store)){
# 
# 
#   store_temp <-
#     as.data.frame(cntry_store[[k]][5]) %>%
#     dplyr::select(cntry, rand_id, F1, F2)
# 
#   df_store[[k]] <- store_temp
# 
# 
# }
# 
# 
# get results
# res <-
#   as.data.frame(do.call(rbind, df_store)) %>%
#   rename(country_specific_F1 = F1,
#          country_specific_F2 = F2)
# 
# 
# 
# # join to ess main
# ess_seventh_by_country <-
#   ess_seventh_main %>%
#   left_join(res,
#             by = c("cntry", "rand_id")) %>%
#   mutate(country_specific_xeno = ifelse(country_specific_F2 > 1.5, 1, 0))
# 
# 
# mirt_res_by_country <-
#   list(cntry_store,
#        df_store,
#        res,
#        ess_seventh_full,
#        ess_seventh_main,
#        ess_seventh_by_country)
# 
# names(mirt_res_by_country) <-
#   c("cntry_store",
#     "df_store",
#     "res",
#     "ess_seventh_full",
#     "ess_seventh_main",
#     "ess_seventh_by_country")
# 
# save(mirt_res_by_country,
#      file = "mirt_res_by_country.RData")




# ---------- grouped together approach



# load mirt package
# library(mirt)
# 
# 
# # ---------- MIRT MODEL
# 
# # select only imm-related variables
# ess_seventh_imm <-
#   ess_seventh_full %>%
#   dplyr::select(rand_id, cntry, pplstrd, gvrfgap,
#                 smegbli, smegbhw, smctmbe,
#                 contains("cat_"))
# 
# #
# # # estimate model
# mirt_mod_seventh <-
#   mirt(data = ess_seventh_imm[,-c(1,2)],
#        model = 2,
#        itemtype = "gpcm",
#        SE = TRUE,
#        verbose = TRUE,
#        technical = list(NCYCLES = 5000))
# 
# 
# 
# # gather parameter results
# param_ests_seventh <-
#   as.data.frame(mirt_mod_seventh@Fit$F) %>%
#   mutate(feature = row.names(.)) %>%
#   remove_rownames()
# 
# 
# # get trait scores
# ind_traits_seventh <-
#   data.frame(fscores(mirt_mod_seventh)) %>%
#   mutate(rand_id = row_number())
# 
# #
# # join results to full data
# ess_seventh_full_with_traits_all_countries <-
#   ess_seventh_full %>%
#   left_join(ind_traits_seventh, by = c("rand_id")) %>%
#   mutate(xeno_decile_all_country = ntile(F2, n = 10),
#          econ_insec_decile_all_country = ntile(F1, n = 10))
# 
# # join results to main data with party info and purged nas
# ess_seventh_main_with_traits_all_countries <-
#   ess_seventh_main %>%
#   left_join(ind_traits_seventh,
#             by = c("rand_id")) %>%
#   mutate(xeno_decile_all_country = ntile(F2, n = 10),
#          econ_insec_decile_all_country = ntile(F1, n = 10))
# 
# 
# 
# 
# # ---------- save data
# 
# list objects
# mirt_res_all_countries <-
#   list(ess_seventh_full,
#        ess_seventh_imm,
#        mirt_mod_seventh,
#        ind_traits_seventh,
#        ess_seventh_full_with_traits_all_countries,
#        ess_seventh_main_with_traits_all_countries)
# 
# # rename
# names(mirt_res_all_countries) <-
#   c("ess_seventh_full",
#     "ess_seventh_imm",
#     "mirt_mod_seventh",
#     "ind_traits_seventh",
#     "ess_seventh_full_with_traits_all_countries",
#     "ess_seventh_main_with_traits_all_countries")
# 
# # save
# save(mirt_res_all_countries,
#      file = "mirt_model_results.RData")





# ---------- load results

# load("mirt_model_results.RData")
# load("mirt_res_by_country.RData")







# ----- visualize some results

# populist and nationalist plot by occupation
ess_seventh_by_country %>%
  group_by(major_occupation_label) %>%
  summarise(avg_pop = mean(populist, na.rm = TRUE),
            avg_nat = mean(nationalist, na.rm = TRUE),
            avg_imm_view = mean(country_specific_F2, na.rm = TRUE),
            avg_econ_view = mean(country_specific_F1, na.rm = TRUE),
            num_obs = n()) %>%
  arrange(desc(avg_pop)) %>%
  filter(!is.na(major_occupation_label),
         major_occupation_label != "Armed Forces Occupations") %>%
  view()


# same, but by industry
ess_seventh_by_country %>%
  group_by(nacer2, nace_two_digit_name) %>%
  summarise(avg_pop = mean(populist, na.rm = TRUE),
            avg_nat = mean(nationalist, na.rm = TRUE),
            avg_imm_view = mean(country_specific_F2, na.rm = TRUE),
            avg_econ_view = mean(country_specific_F1, na.rm = TRUE),
            num_obs = n()) %>%
  arrange(desc(avg_econ_view)) %>%
  filter(num_obs >= 50) %>%
  mutate(industry_type = ifelse(nacer2 <= 32,
                                "forestry, logging, or manufacturing",
                                "other")) %>%
  select(nacer2, industry_type, contains("avg"), num_obs) %>%
  view()




ess_seventh_main %>%
  mutate(main_country = 
           ifelse(cntry == "BE" | cntry == "EL" | cntry == "PT" | cntry == "ES" |
                    cntry == "DK" | cntry == "DE" | cntry == "FR" | cntry == "HU" |
                    cntry == "IT" | cntry == "AT" | cntry == "NL" | cntry == "FI" |
                    cntry == "SE" | cntry == "CH" | cntry == "NO" | cntry == "GB", 1, 0)) %>%
  filter(main_country == 1) %>%
  ggplot(aes(x = ordinal_longrun_trade_status)) +
  geom_bar() +
  facet_wrap(~cntry) +
  theme_minimal()






# VISUALIZATION --------------------------------------------------


# --------------------------------- IRT graphs
# library(mirt)

# get model objects
# irt_res <-
#   mirt_res_by_country$cntry_store
# 
# irt_res2 <-
#   mirt_res$mirt_mod_seventh
# 
# locs_res2 <-
#   irt_res2@Fit$F %>%
#   as.data.frame() %>%
#   mutate(item_name = row.names(.)) %>%
#   remove_rownames()
# 
# locs_res2 %>%
#   ggplot(aes(x = F1, y = F2, label = item_name)) +
#   geom_point() +
#   lims(x = c(-1, 1), y = c(-1, 1)) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = 0) +
#   geom_text() +
#   theme_minimal()
# 
#   
# 
# 
# # plot idcs
# test <- irt_res[[1]]
# locations <-
#   test[[2]]@Fit$F %>%
#   as.data.frame() %>%
#   mutate(item_name = row.names(test[[2]]@Fit$F)) %>%
#   remove_rownames()
# 
# locations %>%
#   ggplot(aes(x = F1, y = F2, label = item_name)) +
#   geom_point() +
#   lims(x = c(-1, 1), y = c(-1, 1)) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = 0) +
#   geom_text() +
#   theme_minimal()



# ---------- sf

ess_seventh_main_with_traits %>%
  ggplot(aes(y = F1, x = as.factor(populist_nationalist))) +
  geom_boxplot() +
  facet_wrap(~cntry) +
  theme_minimal()




# ---------- diagram of mediation

DiagrammeR::grViz("digraph {
  
graph[layout = dot, rankdir = TD]

node [shape = rectangle, width = 1.3]

a[label = 'globalization \n exposure']
b[label = 'mediator']
c[label = 'populist\n closeness']
d[label = 'exogenous\n factors']

a -> b
b -> c
a -> c
d -> a

{rank = same; a; c; d}
{rank = min; b}

}")




# ---------- main data
plot_df <-
  mirt_res_by_country$ess_seventh_by_country %>%
  mutate(across(.cols = c(ipcrtiv:impfun),
                .fns = ~ifelse(.x > 6 | .x < 1, NA, .x))) %>%
  group_by(cntry) %>%
  mutate(across(.cols = c(contains("trst"),
                          contains("stf"),
                          ipcrtiv:impfun),
                .fns = ~scale(.x, center = TRUE, scale = TRUE))) %>%
  ungroup()


# --------- figures of latent traits

plot_df %>%
  ggplot(aes(x = country_specific_F1,
             y = country_specific_F2)) +
  geom_point() +
  facet_wrap(~cntry) +
  theme_minimal()



# ---------- tables

# occupations
occ_table_by_anti_imm <-
  ess_seventh_fscores %>%
  group_by(major_occupation_label) %>%
  summarise(avg_populist = mean(populist, na.rm = TRUE),
            avg_nationalist = mean(nationalist, na.rm = TRUE),
            avg_anti_imm = mean(country_specific_F2, na.rm = TRUE),
            avg_anti_econ = mean(country_specific_F1, na.rm = TRUE),
            num_obs = n()) %>%
  arrange(desc(avg_anti_imm)) %>%
  filter(!is.na(major_occupation_label),
         major_occupation_label != "Armed Forces Occupations")

# industries
industry_table_by_anti_imm <-
  plot_df %>%
  group_by(nacer2, nace_two_digit_name) %>%
  summarise(avg_populist = mean(populist, na.rm = TRUE),
            avg_nationalist = mean(nationalist, na.rm = TRUE),
            avg_anti_imm = mean(country_specific_F2, na.rm = TRUE),
            avg_anti_econ = mean(country_specific_F1, na.rm = TRUE),
            num_obs = n()) %>%
  filter(num_obs >= 50,
         !is.na(nacer2)) %>%
  mutate(industry_type = ifelse(nacer2 <= 32,
                                "forestry, logging, or manufacturing",
                                "other")) %>%
  dplyr::select(nacer2, industry_type, contains("avg"), num_obs) %>%
  ungroup() %>%
  arrange(desc(avg_anti_imm)) %>%
  slice_head(n = 30)


  

# ----- distribution of populism and far-right nationalism by country

# populist
ess_seventh_with_traits %>%
  filter(!is.na(populist)) %>%
  ggplot(aes(x = as.factor(populist))) +
  geom_bar() +
  facet_wrap(~cntry) +
  theme_minimal()

# far right nationalism
ess_seventh_with_traits %>%
  filter(!is.na(farright)) %>%
  ggplot(aes(x = as.factor(farright))) +
  geom_bar() +
  facet_wrap(~cntry) +
  theme_minimal()



# ----- check distribution by country

# latent trait F1
ess_seventh_with_traits %>%
  ggplot(aes(x = F1)) +
  geom_density() +
  facet_wrap(~cntry) +
  theme_minimal()


# latent trait F2
ess_seventh_with_traits %>%
  ggplot(aes(x = F2)) +
  geom_density() +
  facet_wrap(~cntry) +
  theme_minimal()


# for interest, filter germany and check regions
ess_seventh_with_traits %>%
  filter(cntry == "DE") %>%
  ggplot(aes(x = F1)) +
  geom_density() +
  facet_wrap(~region) +
  theme_minimal()


# plot both traits, faceted by country
ess_seventh_with_traits %>%
  ggplot(aes(x = F1)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~cntry) +
  theme_minimal()

ess_seventh_with_traits %>%
  ggplot(aes(x = F2)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~cntry) +
  theme_minimal()


# ----- first immigration trait

# politics
ess_seventh_with_traits %>%
  filter(!is.na(lrscale_cat)) %>%
  ggplot(aes(x = as.factor(lrscale_cat), y = F1)) +
  geom_boxplot() +
  theme_minimal()

# education
ess_seventh_with_traits %>%
  filter(!is.na(edu_cat)) %>%
  ggplot(aes(x = as.factor(edu_cat), y = F1)) +
  geom_boxplot() +
  theme_minimal()

# income
ess_seventh_with_traits %>%
  filter(!is.na(hinctnta_cat)) %>%
  ggplot(aes(x = as.factor(hinctnta_cat), y = F1)) +
  geom_boxplot() +
  theme_minimal()

# voting
ess_seventh_with_traits %>%
  filter(!is.na(farright)) %>%
  ggplot(aes(x = as.factor(farright), y = F1)) +
  geom_boxplot() +
  theme_minimal()


# citizen
ess_seventh_with_traits %>%
  filter(!is.na(citizen)) %>%
  ggplot(aes(x = as.factor(citizen), y = F1)) +
  geom_boxplot() +
  theme_minimal()



# ----- second immigration trait

# politics
ess_seventh_with_traits %>%
  filter(!is.na(lrscale_cat)) %>%
  ggplot(aes(x = as.factor(lrscale_cat), y = F2)) +
  geom_boxplot() +
  theme_minimal()

# education
ess_seventh_with_traits %>%
  filter(!is.na(edu_cat)) %>%
  ggplot(aes(x = as.factor(edu_cat), y = F2)) +
  geom_boxplot() +
  theme_minimal()

# income
ess_seventh_with_traits %>%
  filter(!is.na(hinctnta_cat)) %>%
  ggplot(aes(x = as.factor(hinctnta_cat), y = F2)) +
  geom_boxplot() +
  theme_minimal()

# voting
ess_seventh_with_traits %>%
  filter(!is.na(farright)) %>%
  ggplot(aes(x = as.factor(farright), y = F2)) +
  geom_boxplot() +
  theme_minimal()

# citizen
ess_seventh_with_traits %>%
  filter(!is.na(citizen)) %>%
  ggplot(aes(x = as.factor(citizen), y = F2)) +
  geom_boxplot() +
  theme_minimal()





# -------------------- PLOTS THAT APPEAR IN PAPER

# average income by occupation
table_df <-
  main_df %>%
  group_by(cntry, major_occupation_label) %>%
  summarise(avg_wage = mean(hinctnta, na.rm = TRUE),
            avg_pop = mean(populist, na.rm = TRUE),
            avg_nat = mean(nationalist, na.rm = TRUE),
            avg_imm_view = mean(country_specific_F2, na.rm = TRUE),
            avg_econ_view = mean(country_specific_F1, na.rm = TRUE),
            num_obs = n()) %>%
  arrange(cntry, desc(avg_wage)) %>%
  ungroup()

# avg imm view as function of wage
table_df %>%
  ggplot(aes(x = avg_wage, avg_imm_view)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# avg pop as function of wage
table_df %>%
  ggplot(aes(x = avg_wage, avg_nat)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()


# table of most populist industries, ninth wave
ninth_wave_most_populist_nace_industries <-
  mirt_res_by_country$ess_seventh_by_country %>%
  group_by(nace_two_digit_name, nacer2) %>%
  summarise(share_pop_industry = 
              mean(populist, na.rm = TRUE),
            share_farright_industry = 
              mean(farright, na.rm = TRUE),
            num_obs = n()) %>%
  ungroup() %>%
  filter(!is.na(nacer2),
         num_obs >= 30) %>%
  arrange(desc(share_pop_industry)) %>%
  slice_head(n = 30)

# table of most populist industries, seventh wave
seventh_wave_most_populist_nace_industries <-
  ess_seventh_with_traits %>%
  group_by(nace_two_digit_name, nacer2) %>%
  summarise(share_pop_industry = 
              mean(populist, na.rm = TRUE),
            share_farright_industry = 
              mean(farright, na.rm = TRUE),
            num_obs = n()) %>%
  ungroup() %>%
  filter(!is.na(nacer2),
         num_obs >= 30) %>%
  arrange(desc(share_pop_industry)) %>%
  slice_head(n = 30)


# average belief about immigrant economic harm, by occupation
ess_seventh_by_country %>%
  group_by(cntry, major_occupation_label) %>%
  summarise(avg_imm_econ = mean(imbgeco, na.rm = TRUE),
            avg_imm_culture = mean(imueclt, na.rm = TRUE)) %>%
  arrange(cntry, desc(avg_imm_culture)) %>%
  view()





ess9 %>%
  group_by(cntry, major_occupation_label) %>%
  summarise(share_populist = mean(populist, na.rm = TRUE),
            share_farright = mean(farright, na.rm = TRUE),
            num_obs = n()) %>%
  arrange(cntry, desc(share_populist)) %>%
  view()


ess9 %>%
  filter(!is.na(edu_cat)) %>%
  group_by(cntry, edu_cat) %>%
  summarise(share_populist = mean(populist, na.rm = TRUE),
            share_farright = mean(farright, na.rm = TRUE),
            num_obs = n()) %>%
  arrange(cntry, desc(share_populist)) %>%
  view()



# plot of most populist and far right industries
ess_seventh_with_traits %>%
  select(cntry, populist, farright, 
         nacer2, nace_two_digit_name) %>%
  group_by(nace_two_digit_name, nacer2) %>%
  summarise(share_pop_industry = 
              mean(populist, na.rm = TRUE),
            share_farright_industry = 
              mean(farright, na.rm = TRUE)) %>%
  arrange(desc(share_pop_industry)) %>%
  view()


# satisfaction and populism by trade status
ess_seventh_with_traits %>%
  select(cntry, industry_trade_balance_cat,
         stfgov, stfdem, populist, farright) %>%
  group_by(cntry, industry_trade_balance_cat) %>%
  summarise(across(.cols = c(stfgov, stfdem,
                             populist, farright),
                   .fns = ~mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}")) %>%
  view()


# populism by occupation
ess_seventh_with_traits %>%
  select(cntry, major_occupation_label,
         stfgov, stfdem, populist, farright) %>%
  group_by(cntry, major_occupation_label) %>%
  summarise(share_populist = mean(populist, na.rm = TRUE),
            share_farright = mean(farright, na.rm = TRUE)) %>%
  arrange(cntry, desc(share_populist)) %>%
  view()



# see which countries had populism
ess_seventh_with_traits %>%
  ggplot(aes(x = as.factor(farright))) +
  geom_bar() +
  facet_wrap(~cntry) +
  theme_minimal()





# immigration prefs 1 by voting type, populist
ess_seventh_with_traits %>%
  filter(cntry != "ES",
         cntry != "PT",
         cntry != "IL",
         cntry != "BE",
         cntry != "EE",
         !is.na(populist),
         !is.na(F1)) %>%
  ggplot(aes(x = F1, col = as.factor(populist),
             group = as.factor(populist))) +
  geom_boxplot() +
  facet_wrap(~cntry) +
  theme_minimal()



# immigration prefs 2 by voting type, populist
ess_seventh_with_traits %>%
  filter(cntry != "ES",
         cntry != "PT",
         cntry != "IL",
         cntry != "BE",
         cntry != "EE",
         !is.na(populist),
         !is.na(F2)) %>%
  ggplot(aes(x = F2, col = as.factor(populist),
             group = as.factor(populist))) +
  geom_boxplot() +
  facet_wrap(~cntry) +
  theme_minimal()





# immigration prefs 1 by voting type, far right
ess_seventh_with_traits %>%
  filter(cntry != "ES",
         cntry != "PT",
         cntry != "IL",
         cntry != "BE",
         cntry != "EE",
         cntry != "IE",
         !is.na(farright),
         !is.na(F1)) %>%
  ggplot(aes(x = F1, col = as.factor(farright),
             group = as.factor(farright))) +
  geom_boxplot() +
  labs(x = "Economic dimension (standardized)",
       title = "Economic Immigration Preferences and Voting") +
  facet_wrap(~cntry) +
  theme_minimal() +
  theme(axis.text.y.left = element_blank())



# immigration prefs 2 by voting type, far right
ess_seventh_with_traits %>%
  filter(cntry != "ES",
         cntry != "PT",
         cntry != "IL",
         cntry != "BE",
         cntry != "EE",
         cntry != "IE",
         !is.na(farright),
         !is.na(F2)) %>%
  ggplot(aes(x = F2, col = as.factor(farright),
             group = as.factor(farright))) +
  geom_boxplot() +
  facet_wrap(~cntry) +
  labs(x = "Cultural dimension (standardized)",
       title = "Cultural Immigration Preferences and Voting") +
  theme_minimal() +
  theme(axis.text.y.left = element_blank())




ess_seventh_with_traits %>%
  filter(cntry != "ES",
         cntry != "PT",
         cntry != "IL",
         cntry != "BE",
         cntry != "EE",
         cntry != "IE",
         !is.na(populist)) %>%
  group_by(cntry, major_occupation_label,
           edu_cat) %>%
  summarise(share_pop = mean(populist, na.rm = TRUE),
            num_obs = n()) %>%
  filter(num_obs >= 10) %>%
  pivot_wider(id_cols = c(cntry, major_occupation_label),
              names_from = edu_cat,
              values_from = share_pop) %>%
  arrange(cntry, major_occupation_label) %>%
  view()
            






# MACHINE LEARNING MODELS --------------------------------------------------


set.seed(0924890)


# -------------------- classification trees
library(tidymodels)
library(rpart)
library(rpart.plot)
library(caret)
library(vip)
library(ipred)


# setup tree using only sociodemographics
trees_sociodem_df <-
  mirt_res_by_country$ess_seventh_by_country %>%
  dplyr::select(cntry, agea, hinctnta, lrscale,
                major_occupation_label, eisced,
                rlgdgr, brncntr, mbtru, gndr,
                populist, nationalist, country_specific_F1,
                country_specific_F2, aesfdrk,
                ppltrst, sclmeet, dscrgrp) %>%
  mutate(religiosity = ifelse(rlgdgr > 10 | rlgdgr < 0, NA, rlgdgr),
         mbtru = ifelse(mbtru >= 7, NA, mbtru),
         gndr = ifelse(gndr == 9, NA, gndr),
         aesfdrk = ifelse(aesfdrk >= 7, NA, aesfdrk),
         ppltrst = ifelse(ppltrst > 10, NA, ppltrst),
         sclmeet = ifelse(sclmeet > 7, NA, sclmeet),
         dscrgrp = ifelse(dscrgrp > 2, NA, dscrgrp)) %>%
  filter(major_occupation_label != "Armed Forces Occupations") %>%
  dplyr::select(-cntry) %>%
  data.frame()


trees_humanvals_df <-
  mirt_res_by_country$ess_seventh_by_country %>%
  dplyr::select(cntry, contains("trst"), contains("stf"),
         country_specific_F2, populist, nationalist,
         ipcrtiv:impfun, -marstfi) %>%
  mutate(across(.cols = c(ipcrtiv:impfun),
                .fns = ~ifelse(.x > 6 | .x < 1, NA, .x))) %>%
  group_by(cntry) %>%
  mutate(across(.cols = c(contains("trst"),
                          contains("stf"),
                          ipcrtiv:impfun),
                .fns = ~scale(.x, center = TRUE, scale = TRUE))) %>%
  ungroup() %>%
  dplyr::select(-cntry) %>%
  data.frame()

  


# -------------------- regression tree for continuous cultural imm pref

# tree model for immigration preferences
tree_sociodem_res_F2 <-
  rpart(country_specific_F2 ~ agea + 
          lrscale + hinctnta + gndr + mbtru +
          eisced + major_occupation_label +
          religiosity + populist + ppltrst +
          sclmeet + dscrgrp + aesfdrk,
        data = trees_sociodem_df,
        method = "anova",
        control = rpart.control(cp = 0.0001))



# ----- bagged version
# set number of trees
num_trees <- 10:30
rmse_vec <- vector(mode = "numeric", length = length(num_trees))

# estimate
for(i in seq_along(num_trees)){
  
  # estimate tree
  temp_bagged <-
    bagging(country_specific_F2 ~ agea + 
              lrscale + hinctnta +  gndr + mbtru +
              eisced + major_occupation_label +
              religiosity + populist + ppltrst +
              sclmeet + dscrgrp + aesfdrk,
            data = trees_df,
            coob = TRUE,
            nbagg = num_trees[i])
  
  # get error
  rmse_vec[i] <- temp_bagged$err

  
}

plot(num_trees, rmse_vec, type = "l", lwd = 2)
abline(v = 25, col = "red", lty = "dashed")



# ----- diagnostics for F2

# plot
rpart.plot(tree_sociodem_res_F2)

# plot error as function of complexity param
plotcp(tree_sociodem_res_F2)

# find best complexity param
bestcp <- tree_sociodem_res_F2$cptable[which.min(tree_sociodem_res_F2$cptable[,"xerror"]),"CP"]

# get pruned tree
tree.pruned.F2 <- rpart::prune(tree_sociodem_res_F2, cp = 0.004)

# plot pruned tree
rpart.plot(tree.pruned.F2)

# variable importance
vip_sociodem <-
  vip(tree.pruned.F2,
    num_features = 15)

df_vip_sociodem <-
  vip_sociodem$data

vip_plot_sociodem <-
  df_vip_sociodem %>%
  ggplot(aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "steelblue") +
  labs(x = "importance", y = "") +
  theme_minimal()




# ---------- regression tree for human values version

# tree model for immigration preferences
tree_humanvals_res_F2 <-
  rpart(country_specific_F2 ~ .,
        data = trees_humanvals_df,
        method = "anova",
        control = rpart.control(cp = 0.0001))




# ----- bagged version
# set number of trees
num_trees <- 10:30
rmse_vec_humanvals <- vector(mode = "numeric", length = length(num_trees))

# estimate
for(i in seq_along(num_trees)){
  
  # estimate tree
  temp_bagged <-
    bagging(country_specific_F2 ~ .,
            data = trees_humanvals_df,
            coob = TRUE,
            nbagg = num_trees[i])
  
  # get error
  rmse_vec_humanvals[i] <- temp_bagged$err
  
  
}

plot(num_trees, rmse_vec_humanvals, type = "l", lwd = 2)
abline(v = 25, col = "red", lty = "dashed")



# ----- diagnostics for F2

# plot
rpart.plot(tree_humanvals_res_F2)

# plot error as function of complexity param
plotcp(tree_humanvals_res_F2)

# find best complexity param
bestcp <- tree_humanvals_res_F2$cptable[which.min(tree_humanvals_res_F2$cptable[,"xerror"]),"CP"]

# get pruned tree
tree_pruned_humanvals <- rpart::prune(tree_humanvals_res_F2, cp = 0.004)

# plot pruned tree
rpart.plot(tree_pruned_humanvals)

# variable importance
vip_humanvals <-
  vip(tree_pruned_humanvals,
    num_features = 15)

df_vip_humanvals <-
  vip_humanvals$data

vip_plot_humanvals <-
  df_vip_humanvals %>%
  ggplot(aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "steelblue") +
  labs(x = "importance", y = "") +
  theme_minimal()





# -------------------- classification tree for populism

# tree model for populism
tree_sociodem_res_populism <-
  rpart(populist ~ agea + 
          lrscale + hinctnta +
          eisced + major_occupation_label +
          religiosity +
          country_specific_F2,
        data = trees_sociodem_df,
        method = "class",
        control = rpart.control(cp = 0.0001))


# ----- diagnostics for populism

# plot
rpart.plot(tree_sociodem_res_populism)

# plot error as function of complexity param
plotcp(tree_sociodem_res_populism)

# find best complexity param
bestcp_populism <- tree_sociodem_res_populism$cptable[which.min(tree_sociodem_res_populism$cptable[,"xerror"]),"CP"]

# get pruned tree
tree.pruned.populism <- rpart::prune(tree_sociodem_res_populism, cp = bestcp_populism)

# plot pruned tree
rpart.plot(tree.pruned.populism)

# variable importance
vip(tree.pruned.populism,
    num_features = 15)





# ---------- classification tree for populism with human values


# tree model for populism
tree_humanvals_res_populism <-
  rpart(populist ~ .,
        data = select(trees_humanvals_df,
                      -nationalist),
        method = "class",
        control = rpart.control(cp = 0.0001))


# ----- diagnostics for populism

# plot
rpart.plot(tree_humanvals_res_populism)

# plot error as function of complexity param
plotcp(tree_humanvals_res_populism)

# find best complexity param
bestcp_humanvals_populism <- tree_humanvals_res_populism$cptable[which.min(tree_humanvals_res_populism$cptable[,"xerror"]),"CP"]

# get pruned tree
tree_pruned_humanvals_populism <- rpart::prune(tree_humanvals_res_populism, cp = bestcp_humanvals_populism)

# plot pruned tree
rpart.plot(tree_pruned_humanvals_populism)

# variable importance
vip(tree_pruned_humanvals_populism,
    num_features = 15)













# 
# 
# # ---------- k-means by country to identify voter groups
# 
# # loop prelims
# cntrys <- unique(ess_seventh_with_traits$cntry)
# kvals <- 5:10
# kvals_store <- list()
# tibble_store <- list()
# kproto_store <- list()
# elbow_plot_store <- list()
# 
# 
# # loop
# for(i in 1:length(cntrys)){
#   
#   # data
#   temp_df <-
#     ess_seventh_with_traits %>%
#     filter(cntry == cntrys[i]) %>%
#     dplyr::select(age_cat, hinctnta,
#            lrscale_cat,
#            edu_cat, religious_cat,
#            log_gdp,
#            employment_change_since_2008) %>%
#     mutate(across(.cols = c(age_cat,
#                             lrscale_cat, edu_cat,
#                             religious_cat),
#                   .fns = ~as.factor(.x))) %>%
#     remove_empty(which = "cols")
#   
#   
#   
#   
#   for(k in 1:length(kvals)){
#     
#     # kproto
#     kproto_temp <-
#       kproto(x = temp_df,
#              k = kvals[k])
#     
#     # get tot within ss
#     tibble_temp <-
#       c(country = cntrys[i],
#         tot_within_ss = kproto_temp$tot.withinss,
#         cluster_size = k)
#     
#     kvals_store[[k]] <- 
#       kproto_temp
#     
#     tibble_store[[k]] <-
#       tibble_temp
#     
#     
#   }
#   
#   # unravel tibble
#   tibble_df <-
#     do.call(rbind, tibble_store)
#   
#   # store data for elbow plots
#   elbow_plot_store[[i]] <-
#     tibble_df
#   
#   # store prototype data
#   kproto_store[[i]] <-
#     kvals_store
#   
#   
# }
# 
# 
# # combine elbow plot data from each country
# elbow_plot_all_countries <-
#   as.data.frame(do.call(rbind, elbow_plot_store)) %>%
#   mutate(across(.cols = c(tot_within_ss, cluster_size),
#                 .fns = ~as.numeric(.x)))
# 
# # plot by country
# elbow_plot_all_countries %>%
#   ggplot(aes(x = cluster_size, y = tot_within_ss)) +
#   geom_point(col = "steelblue") +
#   geom_line(col = "steelblue") +
#   facet_wrap(~country) +
#   theme_minimal() +
#   theme(axis.line = element_line())



# METHOD A (basics) ----------------------------------------

# data
main_df <-
  mirt_res_by_country$ess_seventh_by_country %>%
  filter(cntry != "BE",
         cntry != "CZ",
         cntry != "EE",
         cntry != "SI") %>%
  mutate(across(.cols = c(ipcrtiv:impfun),
                .fns = ~ifelse(.x > 6 | .x < 1, NA, .x)),
         acetalv = ifelse(acetalv > 3, NA, acetalv),
         acetalv = ifelse(acetalv == 1 | acetalv == 2, 0, 1)) %>%
  group_by(cntry) %>%
  mutate(across(.cols = c(contains("trst"),
                          contains("stf"),
                          ipcrtiv:impfun),
                .fns = ~scale(.x, center = TRUE, scale = TRUE))) %>%
  ungroup()

  


# ---------- simple model: imm pref as f'n of populist
mod_A1 <-
  lm(country_specific_F2 ~
       cntry + populist,
     data = main_df)

# summary
summary(mod_A1)


# ---------- add individual covariates
mod_A2 <-
  lm(country_specific_F2 ~
       cntry + populist +
       lrscale + hinctnta +
       agea + as.factor(edu_cat) +
       as.factor(edu_cat)*hinctnta +
       rlgdgr +
       major_occupation_label,
     data = main_df)
summary(mod_A2)


# ---------- add individual covariates with subjective beliefs
mod_A3 <-
  lm(country_specific_F2 ~
       cntry + populist +
       lrscale + hinctnta +
       agea + as.factor(edu_cat) +
       as.factor(edu_cat)*hinctnta +
       rlgdgr + major_occupation_label +
       stfgov + stfeco +
       trstplt + ipstrgv + 
       impsafe,
     data = main_df)
summary(mod_A3)







# METHOD B.1 MATCHING WITH NN --------------------

library(MatchIt)
library(cobalt)

temp <- mirt_res_by_country$ess_seventh_by_country
  

nn_df <- 
  temp %>%
  mutate(populist_not_nationalist = 
           ifelse(populist == 1 & nationalist == 0, 1, 0),
         populist_nationalist = ifelse(populist == 1 & nationalist == 1, 1, 0)) %>%
  dplyr::select(cntry, populist, nationalist, populist_not_nationalist,
                populist_nationalist, country_specific_F1,
                country_specific_F2, country_specific_xeno,
                uempla, eisced, edu_cat, hinctnta, hinctnta_cat,
                lrscale, lrscale_cat, rlgdgr, agea, age_cat,
                brncntr, mbtru, gndr, uemp5yr, uemp3m,
                major_occupation_label, acetalv) %>%
  mutate(recent_unemp = ifelse(uemp3m == 1 & uemp5yr == 2, 1, 0),
         acetalv = ifelse(acetalv > 3, NA, acetalv),
         acetalv = ifelse(acetalv == 1 | acetalv == 2, 0, 1),
         binary_college = 
           case_when(eisced > 5 ~ 1,
                     eisced <= 5 ~ 0),
         xeno =
           ifelse(country_specific_F2 >
                    quantile(temp$country_specific_F2,
                             probs = c(0.8), na.rm = TRUE), 1, 0),
         xeno = as.factor(xeno),
         xeno_decile = ntile(country_specific_F2,
                             n = 10),
         econ_insec =
           ifelse(country_specific_F1 > 
                    quantile(temp$country_specific_F1, 
                             probs = c(0.8), na.rm = TRUE), 1, 0),
         econ_insec = as.factor(econ_insec),
         econ_insec_decile = ntile(country_specific_F1,
                                   n = 10)) %>%
  dplyr::select(-c(uemp5yr, uemp3m)) %>%
  drop_na() %>%
  filter(major_occupation_label != "Armed Forces Occupations") %>%
  mutate(major_occupation_label = as.factor(major_occupation_label),
         edu_cat = as.factor(edu_cat),
         age_cat = as.factor(age_cat)) %>%
  filter(cntry != "BE",
         cntry != "CZ",
         cntry != "LT",
         cntry != "SI")
  


# -------------------- matching models


library(MatchIt)

# index countries,
list_countries <-
  unique(nn_df$cntry)

# list of data
nn_store <- list()
cem_store <- list()


# --------------- nearest neighbor matching 


# loop
for(i in seq_along(list_countries)){
  
  # subset data
  temp_df <-
    nn_df %>%
    filter(cntry == list_countries[i]) %>%
    select(xeno, populist_nationalist,
           populist_not_nationalist, hinctnta, edu_cat,
           age_cat, lrscale_cat, recent_unemp, acetalv, econ_insec,
           major_occupation_label) %>%
    mutate(across(.cols = c(edu_cat, age_cat, 
                            lrscale_cat, major_occupation_label),
                  .fns = ~as.factor(.x))) %>%
    drop_na()
  
  # matching model
  temp_mod <-
    matchit(xeno ~ hinctnta + edu_cat +
              age_cat + lrscale_cat +
              recent_unemp + acetalv +
              econ_insec +
              major_occupation_label,
            data = temp_df,
            method = "nearest",
            estimand = "ATT")
  
  # matching data
  temp_match_df <-
    match.data(temp_mod)
  
  
  # plot statistical balance
  balance_plot <-
    love.plot(temp_mod, stats = c("c", "ks"),
            thresholds = c(cor = .1), 
            abs = TRUE, wrap = 20,
            var.order = "unadjusted", line = TRUE)
  
  # plot proportions by treatment for some vars
  occ_props <-
    bal.plot(temp_mod, "major_occupation_label", which = "both")
  
  lrscale_props <-
    bal.plot(temp_mod, "lrscale_cat", which = "both")
  
  econ_insec_props <-
    bal.plot(temp_mod, "econ_insec", which = "both")
  
  hinc_props <-
    bal.plot(temp_mod, "hinctnta", which = "both")
  
  
  
  
  # estimate outcome pop
  temp_out_pop <-
    glm(populist_not_nationalist ~
          xeno*(hinctnta + edu_cat +
                  age_cat + lrscale_cat +
                  recent_unemp + acetalv +
                  econ_insec +
                  major_occupation_label),
        data = temp_match_df,
        weights = weights)
  
  # estimate outcome nat
  temp_out_nat <-
    glm(populist_nationalist ~
          xeno*(hinctnta + edu_cat +
                  age_cat + lrscale_cat +
                  recent_unemp + acetalv +
                  econ_insec +
                  major_occupation_label),
        data = temp_match_df,
        weights = weights)
  
  # store
  nn_store[[i]] <- 
    list(temp_df, temp_mod, balance_plot,
         occ_props, lrscale_props, econ_insec_props,
         hinc_props, temp_match_df, 
         temp_out_pop, temp_out_nat)
  
  
  
}


names(nn_store) <- list_countries


# save results
# save(nn_store,
#      file = "guilty_by_association_nearest_neighbor_matching_models.RData")




# --------------- coarsened exact matching

# loop
for(i in seq_along(list_countries)){
  
  # subset data
  temp_df_cem <-
    nn_df %>%
    filter(cntry == list_countries[i]) %>%
    select(xeno, populist_nationalist,
           populist_not_nationalist, hinctnta, eisced,
           agea, lrscale, recent_unemp, econ_insec_decile,
           major_occupation_label) %>%
    mutate(major_occupation_label = as.factor(major_occupation_label)) %>%
    drop_na()
  
  # matching model
  temp_mod_cem <-
    matchit(xeno ~ hinctnta + eisced +
              agea + lrscale +
              recent_unemp  +
              econ_insec_decile +
              major_occupation_label,
            data = temp_df_cem,
            method = "cem")
  
  # matching data
  temp_match_df_cem <-
    match.data(temp_mod_cem)
  
  
  # plot statistical balance
  balance_plot_cem <-
    love.plot(temp_mod_cem, stats = c("c", "ks"),
              thresholds = c(cor = .1), 
              abs = TRUE, wrap = 20,
              var.order = "unadjusted", line = TRUE)
  
  # plot proportions by treatment for some vars
  occ_props_cem <-
    bal.plot(temp_mod_cem, "major_occupation_label", which = "both")
  
  lrscale_props_cem <-
    bal.plot(temp_mod_cem, "lrscale", which = "both")
  
  econ_insec_props_cem <-
    bal.plot(temp_mod_cem, "econ_insec_decile", which = "both")
  
  hinc_props_cem <-
    bal.plot(temp_mod_cem, "hinctnta", which = "both")
  
  
  
  
  # estimate outcome pop
  temp_out_pop_cem <-
    glm(populist_not_nationalist ~
          xeno*(hinctnta + eisced +
                  agea + lrscale +
                  recent_unemp  +
                  econ_insec_decile +
                  major_occupation_label),
        data = temp_match_df_cem,
        weights = weights)
  
  # estimate outcome nat
  temp_out_nat_cem <-
    glm(populist_nationalist ~
          xeno*(hinctnta + eisced +
                  agea + lrscale +
                  recent_unemp  +
                  econ_insec_decile +
                  major_occupation_label),
        data = temp_match_df_cem,
        weights = weights)
  
  # store
  cem_store[[i]] <- 
    list(temp_df_cem, temp_mod_cem, balance_plot_cem,
         occ_props_cem, lrscale_props_cem, econ_insec_props_cem,
         hinc_props_cem, temp_match_df_cem, 
         temp_out_pop_cem, temp_out_nat_cem)
  
  
  
}






















# METHOD B.2 (matching and mediation models) ---------------------------


# load package
library(mediation)
library(gtools)




# ---------- prepare data

# get data from saved object
cem_df <-
  mirt_res_by_country$ess_seventh_by_country


# subset to include trade exposure
df_trade_exposure <-
  cem_df %>%
  dplyr::select(cntry, region, populist, nationalist, country_specific_F1,
                country_specific_F2, country_specific_xeno,
                uempla, eisced, edu_cat, hinctnta, hinctnta_cat,
                lrscale, lrscale_cat, rlgdgr, agea, age_cat,
                brncntr, mbtru, gndr, uemp5yr, uemp3m,
                major_occupation_label, acetalv, present_trade_status,
                longrun_trade_status, ordinal_longrun_trade_status) %>%
  mutate(recent_unemp = ifelse(uemp3m == 1 & uemp5yr == 2, 1, 0),
         acetalv = ifelse(acetalv > 3, NA, acetalv),
         acetalv = ifelse(acetalv == 1 | acetalv == 2, 0, 1),
         binary_longrun_trade_exposure = 
           case_when(ordinal_longrun_trade_status <= 2 ~ 1,
                     ordinal_longrun_trade_status > 2 ~ 0),
         binary_present_trade_exposure = 
           case_when(present_trade_status == "very advantaged" |
                       present_trade_status == "advantaged" ~ 0,
                     present_trade_status == "disadvantaged" |
                       present_trade_status == "very disadvantaged" ~ 1),
         xeno = 
           ifelse(country_specific_F2 > 
                    quantile(cem_df$country_specific_F2, 
                                  probs = c(0.8), na.rm = TRUE), 1, 0),
         xeno = as.factor(xeno),
         xeno_decile = ntile(country_specific_F2,
                             n = 10),
         econ_insec =
           ifelse(country_specific_F1 > 
                    quantile(cem_df$country_specific_F1, 
                             probs = c(0.8), na.rm = TRUE), 1, 0),
         econ_insec = as.factor(econ_insec),
         econ_insec_decile = ntile(country_specific_F1,
                                   n = 10)) %>%
  dplyr::select(-c(uemp5yr, uemp3m)) %>%
  drop_na() %>%
  filter(major_occupation_label != "Armed Forces Occupations") %>%
  mutate(major_occupation_label = as.factor(major_occupation_label),
         edu_cat = as.factor(edu_cat),
         age_cat = as.factor(age_cat)) %>%
  filter(cntry != "BE",
         cntry != "CZ",
         cntry != "LT",
         cntry != "SI")



# subset to include variables but not trade exposure
cem_df_short <-
  cem_df %>%
  dplyr::select(cntry, populist, nationalist, country_specific_F1,
                country_specific_F2, country_specific_xeno,
                uempla, eisced, edu_cat, hinctnta, hinctnta_cat,
                lrscale, lrscale_cat, rlgdgr, agea, age_cat,
                brncntr, mbtru, gndr, uemp5yr, uemp3m,
                major_occupation_label, acetalv) %>%
  mutate(recent_unemp = ifelse(uemp3m == 1 & uemp5yr == 2, 1, 0),
         acetalv = ifelse(acetalv > 3, NA, acetalv),
         acetalv = ifelse(acetalv == 1 | acetalv == 2, 0, 1),
         binary_college = 
           case_when(eisced > 5 ~ 1,
                     eisced <= 5 ~ 0),
         xeno = 
           ifelse(country_specific_F2 > 
                    quantile(cem_df$country_specific_F2, 
                             probs = c(0.8), na.rm = TRUE), 1, 0),
         xeno = as.factor(xeno),
         xeno_decile = ntile(country_specific_F2,
                             n = 10),
         econ_insec =
           ifelse(country_specific_F1 > 
                    quantile(cem_df$country_specific_F1, 
                             probs = c(0.8), na.rm = TRUE), 1, 0),
         econ_insec = as.factor(econ_insec),
         econ_insec_decile = ntile(country_specific_F1,
                                   n = 10)) %>%
  dplyr::select(-c(uemp5yr, uemp3m)) %>%
  drop_na() %>%
  filter(major_occupation_label != "Armed Forces Occupations") %>%
  mutate(major_occupation_label = as.factor(major_occupation_label),
         edu_cat = as.factor(edu_cat),
         age_cat = as.factor(age_cat)) %>%
  filter(cntry != "BE",
         cntry != "CZ",
         cntry != "LT",
         cntry != "SI")







# -------------------- coarsened exact matching on treatments


# ---------- unemployment

# temp cem match
cem_imm_mod <-
  matchit(recent_unemp ~ 
            as.factor(lrscale_cat) + as.factor(age_cat) + 
            as.factor(edu_cat) + hinctnta +
            as.factor(major_occupation_label),
          data = cem_df_short,
          method = "cem")

# get data
unemp_match_df <-
  match.data(cem_imm_mod)



# ---------- long run trade exposure


imm_match_mod <-
  matchit(binary_longrun_trade_exposure ~
            as.factor(lrscale_cat) + as.factor(age_cat) + 
            as.factor(edu_cat) + hinctnta +
            as.factor(major_occupation_label),
          data = df_trade_exposure,
          method = "cem")

# matching data
lrexp_match_df <-
  match.data(imm_match_mod)




# ---------- present run trade exposure

imm_match_mod <-
  matchit(binary_present_trade_exposure ~
            as.factor(lrscale_cat) + as.factor(age_cat) + 
            as.factor(edu_cat) + hinctnta +
            as.factor(major_occupation_label),
          data = df_trade_exposure,
          method = "cem")

# matching data
srexp_match_df <-
  match.data(imm_match_mod)







# -------------------- matching models for imm prefs


# ---------- using unemployment treatment

# cultural model
mod_2cult_unemp <-
  lm(country_specific_F2 ~ cntry +
       recent_unemp*
       (as.factor(lrscale_cat) + as.factor(age_cat) + 
          as.factor(edu_cat) + hinctnta +
          as.factor(major_occupation_label)),
     data = unemp_match_df,
     weights = weights)


# economic model
mod_2econ_unemp <-
  lm(country_specific_F1 ~ cntry +
       recent_unemp*
       (as.factor(lrscale_cat) + as.factor(age_cat) + 
          as.factor(edu_cat) + hinctnta +
          as.factor(major_occupation_label)),
     data = unemp_match_df,
     weights = weights)



# heterogeneous treatments
comp_2cult_unemp <-
  comparisons(mod_2cult_unemp, 
              variables = "recent_unemp",
              vcov = ~subclass,
              newdata = subset(unemp_match_df,  recent_unemp == 1),
              wts = "weights",
              by = "major_occupation_label")
comp_2econ_unemp <-
  comparisons(mod_2econ_unemp, 
              variables = "recent_unemp",
              vcov = ~subclass,
              newdata = subset(unemp_match_df,  recent_unemp == 1),
              wts = "weights",
              by = "major_occupation_label")






# ---------- using long run exposure treatment

# cultural model
mod_2cult_lrexp <-
  lm(country_specific_F2 ~ cntry +
       binary_longrun_trade_exposure*
       (as.factor(lrscale_cat) + as.factor(age_cat) + 
          as.factor(edu_cat) + hinctnta +
          as.factor(major_occupation_label)),
     data = lrexp_match_df,
     weights = weights)


# economic model
mod_2econ_lrexp <-
  lm(country_specific_F1 ~ cntry +
       binary_longrun_trade_exposure*
       (as.factor(lrscale_cat) + as.factor(age_cat) + 
          as.factor(edu_cat) + hinctnta +
          as.factor(major_occupation_label)),
     data = lrexp_match_df,
     weights = weights)


# heterogeneous treatments
comp_2cult_lrexp <-
  comparisons(mod_2cult_lrexp, 
              variables = "binary_longrun_trade_exposure",
              vcov = ~subclass,
              newdata = subset(lrexp_match_df,  
                               binary_longrun_trade_exposure == 1),
              wts = "weights",
              by = "edu_cat")
comp_2econ_lrexp <-
  comparisons(mod_2econ_lrexp, 
              variables = "binary_longrun_trade_exposure",
              vcov = ~subclass,
              newdata = subset(lrexp_match_df,  
                               binary_longrun_trade_exposure == 1),
              wts = "weights",
              by = "edu_cat")





# ---------- using short run exposure treatment

# cultural model
mod_2cult_srexp <-
  lm(country_specific_F2 ~ cntry +
       binary_present_trade_exposure*
       (as.factor(lrscale_cat) + as.factor(age_cat) + 
          as.factor(edu_cat) + hinctnta +
          as.factor(major_occupation_label)),
     data = srexp_match_df,
     weights = weights)


# economic model
mod_2econ_srexp <-
  lm(country_specific_F1 ~ cntry +
       binary_present_trade_exposure*
       (as.factor(lrscale_cat) + as.factor(age_cat) + 
          as.factor(edu_cat) + hinctnta +
          as.factor(major_occupation_label)),
     data = srexp_match_df,
     weights = weights)


# heterogeneous treatments
comp_2cult_srexp <-
  comparisons(mod_2cult_srexp, 
              variables = "binary_present_trade_exposure",
              vcov = ~subclass,
              newdata = subset(srexp_match_df,  
                               binary_present_trade_exposure == 1),
              wts = "weights",
              by = "edu_cat")
comp_2econ_srexp <-
  comparisons(mod_2econ_srexp, 
              variables = "binary_present_trade_exposure",
              vcov = ~subclass,
              newdata = subset(srexp_match_df,  
                               binary_present_trade_exposure == 1),
              wts = "weights",
              by = "edu_cat")






# MATCHING MODELS FOR POPULISM


# ----------------------------------------- matching models


# ---------- recent unemployment treatment

# recent unemployment
mod_match_unemp_xeno <-
  matchit(recent_unemp ~ as.factor(edu_cat) + 
            xeno_decile +
            econ_insec_decile +
            as.factor(age_cat) + hinctnta + 
            as.factor(major_occupation_label) +
            as.factor(lrscale_cat),
          data = cem_df_short,
          method = "cem")

# matching df
df_match_unemp_xeno <- match.data(mod_match_unemp_xeno)




# ---------- long run exposure

# long run exposure
mod_match_lrexp_xeno <-
  matchit(binary_longrun_trade_exposure ~ 
            as.factor(edu_cat) + 
            xeno_decile +
            econ_insec_decile +
            as.factor(age_cat) + hinctnta + 
            as.factor(major_occupation_label) +
            as.factor(lrscale_cat),
          data = df_trade_exposure,
          method = "cem")

# matching df
df_match_lrexp_xeno <- match.data(mod_match_lrexp_xeno)



# ---------- short run exposure

# short run exposure
mod_match_srexp_xeno <-
  matchit(binary_present_trade_exposure ~ 
            as.factor(edu_cat) + 
            xeno_decile +
            econ_insec_decile +
            as.factor(age_cat) + hinctnta + 
            as.factor(major_occupation_label) +
            as.factor(lrscale_cat),
          data = df_trade_exposure,
          method = "cem")

# matching df
df_match_srexp_xeno <- match.data(mod_match_srexp_xeno)





# -------------------------------------------------- treatment effects

# -------------------- using recent unemployment

# populist model
mod_3pop_unemp <-
  lm(populist ~ cntry +
       recent_unemp*
       (xeno_decile + econ_insec_decile +
          as.factor(lrscale_cat) + as.factor(age_cat) + 
          as.factor(edu_cat) + hinctnta +
          as.factor(major_occupation_label)),
     data = df_match_unemp_xeno,
     weights = weights)


# nationalist model
mod_3nat_unemp <-
  lm(nationalist ~ cntry +
       recent_unemp*
       (xeno_decile + econ_insec_decile +
          as.factor(lrscale_cat) + as.factor(age_cat) + 
          as.factor(edu_cat) + hinctnta +
          as.factor(major_occupation_label)),
     data = df_match_unemp_xeno,
     weights = weights)



# heterogeneous treatment effects


# ----- populist
comp_3pop_unemp_xeno <-
  comparisons(mod_3pop_unemp, 
              variables = "recent_unemp",
              vcov = ~subclass,
              newdata = subset(df_match_unemp_xeno,  
                               recent_unemp == 1),
              wts = "weights",
              by = "xeno_decile")
comp_3pop_unemp_econ <-
  comparisons(mod_3pop_unemp, 
              variables = "recent_unemp",
              vcov = ~subclass,
              newdata = subset(df_match_unemp_xeno,  
                               recent_unemp == 1),
              wts = "weights",
              by = "econ_insec_decile")



# ----- nationalist
comp_3nat_unemp_xeno <-
  comparisons(mod_3nat_unemp, 
              variables = "recent_unemp",
              vcov = ~subclass,
              newdata = subset(df_match_unemp_xeno,  
                               recent_unemp == 1),
              wts = "weights",
              by = "xeno_decile")
comp_3nat_unemp_econ <-
  comparisons(mod_3nat_unemp, 
              variables = "recent_unemp",
              vcov = ~subclass,
              newdata = subset(df_match_unemp_xeno,  
                               recent_unemp == 1),
              wts = "weights",
              by = "econ_insec_decile")




  # -------------------- using lr exposure

# populist model
mod_3pop_lrexp <-
  lm(populist ~ cntry +
       binary_longrun_trade_exposure*
       (xeno_decile + econ_insec_decile +
          as.factor(lrscale_cat) + as.factor(age_cat) + 
          as.factor(edu_cat) + hinctnta +
          as.factor(major_occupation_label)),
     data = df_match_lrexp_xeno,
     weights = weights)


# nationalist model
mod_3nat_lrexp <-
  lm(nationalist ~ cntry +
       binary_longrun_trade_exposure*
       (xeno_decile + econ_insec_decile +
          as.factor(lrscale_cat) + as.factor(age_cat) + 
          as.factor(edu_cat) + hinctnta +
          as.factor(major_occupation_label)),
     data = df_match_lrexp_xeno,
     weights = weights)





# -------------------- using lr exposure

# populist model
mod_3pop_srexp <-
  lm(populist ~ cntry +
       binary_present_trade_exposure*
       (xeno_decile + econ_insec_decile +
          as.factor(lrscale_cat) + as.factor(age_cat) + 
          as.factor(edu_cat) + hinctnta +
          as.factor(major_occupation_label)),
     data = df_match_srexp_xeno,
     weights = weights)


# nationalist model
mod_3nat_srexp <-
  lm(nationalist ~ cntry +
       binary_present_trade_exposure*
       (xeno_decile + econ_insec_decile +
          as.factor(lrscale_cat) + as.factor(age_cat) + 
          as.factor(edu_cat) + hinctnta +
          as.factor(major_occupation_label)),
     data = df_match_srexp_xeno,
     weights = weights)

















# -------------------------------------------------- mediation models



# --------------- mediation model with xenophobia as mediator


# initial model
med_fit <-
  glm(country_specific_xeno ~
        recent_unemp + lrscale +
        hinctnta + lrscale*hinctnta +
        edu_cat + rlgdgr +
        major_occupation_label,
      data = cem_imm_match_df,
      weights = weights)

# outcome model
out_fit <-
  glm(populist ~ cntry + 
        country_specific_xeno +
        recent_unemp + lrscale +
        hinctnta + lrscale*hinctnta +
        edu_cat + rlgdgr +
        major_occupation_label +
        country_specific_xeno*recent_unemp,
      data = cem_imm_match_df,
      weights = weights)

# mediation model
med_out_fit <-
  mediate(med_fit, out_fit, treat = "recent_unemp",
          mediator = "country_specific_xeno",
          robustSE = TRUE, sims = 100,
          weights = weights)
summary(med_out_fit)





# --------------- mediation model with imm quantity as mediator

# initial model
med_fit_quantity <-
  glm(acetalv ~ recent_unemp + 
        country_specific_F2 + lrscale +
        hinctnta + lrscale*hinctnta +
        edu_cat + rlgdgr +
        major_occupation_label,
      data = cem_imm_match_df,
      weights = weights)

# outcome model
out_fit_quantity <-
  glm(populist ~ cntry + acetalv +
        recent_unemp + lrscale +
        hinctnta + lrscale*hinctnta +
        edu_cat + rlgdgr +
        major_occupation_label +
        country_specific_F2 +
        acetalv*country_specific_F2,
      data = cem_imm_match_df,
      weights = weights)

# mediation model
med_out_fit_quantity <-
  mediate(med_fit_quantity, out_fit_quantity, 
          treat = "recent_unemp",
          mediator = "acetalv",
          robustSE = TRUE, sims = 100,
          weights = weights)
summary(med_out_fit_quantity)




# --------------- mediation model with education as mediator

# education as mediator
med_fit_edu <-
  glm(binary_college ~ recent_unemp,
      data = cem_imm_match_df,
      weights = weights)

# outcome with education
out_fit_edu <-
  glm(populist ~ cntry + binary_college + 
        recent_unemp + binary_college*recent_unemp +
        lrscale_cat + hinctnta + 
        country_specific_xeno + rlgdgr +
        major_occupation_label,
      data = cem_imm_match_df,
      weights = weights)


# mediation model
med_out_fit_edu <-
  mediate(med_fit_edu, out_fit_edu,
          sims = 100, robustSE = TRUE,
          treat = "recent_unemp",
          mediator = "binary_college",
          weights = weights)


# basic matching model
matching_mod_edu <-
  glm(populist ~ cntry +
        recent_unemp*(
          lrscale_cat + hinctnta +
            country_specific_xeno +
            rlgdgr + binary_college +
            major_occupation_label),
      data = cem_imm_match_df,
      weights = weights)





# ------------------------------ cem by country


# index for loop
cntrys <- unique(cem_df_short$cntry)

# object
mediation_store <- list()

# loop
for(i in 1:length(cntrys)){
  
  # match df
  temp_df <-
    cem_df_short %>%
    filter(cntry == cntrys[i])
  
  # # ---------- matching under regular model
  # 
  # temp_mod_reg <-
  #   matchit(country_specific_xeno ~
  #             hinctnta + edu_cat +
  #             age_cat + lrscale_cat +
  #             uemp3m + major_occupation_label +
  #             trade_disadvantage_treatment,
  #           data = temp_df,
  #           method = "cem")
  # 
  # 
  # temp_reg_match_df <-
  #   match.data(temp_mod_reg)
  # 
  # 
  # temp_mod_reg_treat <-
  #   glm(populist ~ 
  #         xenophobic*(
  #           uemp3m + hinctnta + age_cat +
  #             edu_Cat + lrscale_cat +
  #             F1),
  #       data = temp_reg_match_df,
  #       weights = weights)
  
  
  
  
  
  
  
  # ---------- matching under mediation model
  
  # match model
  temp_mod <-
    matchit(country_specific_xeno ~ 
              edu_cat + age_cat + hinctnta +
              lrscale + rlgdgr,
            data = temp_df,
            method = "cem")
  
  # match df output
  temp_match_df <-
    match.data(temp_mod)
  
  
  # mediation model
  med.fit <- glm(country_specific_xeno ~ 
                   uempla + edu_cat + age_cat + 
                   hinctnta + lrscale,
                 data = temp_match_df,
                 weights = weights)
  
  
  # match model
  out.fit <-
    glm(populist ~ country_specific_xeno +
          uempla + edu_cat + age_cat + 
          hinctnta + lrscale,
        data = temp_match_df,
        weights = weights)
  
  
  # adjusted model
  med.out <-
    mediate(med.fit, out.fit, treat = "uempla",
            mediator = "country_specific_xeno",
            robustSE = TRUE, sims = 100,
            weights = weights)
  
  
  # save
  mediation_store[[i]] <-
    list(temp_df,
         temp_mod,
         temp_match_df,
         med.fit,
         out.fit,
         med.out)
  
  
}

# mediation model - unemployment as treatment, xenophobia as outcome


summary(mediation_store[[13]][[2]])

bal.plot(mediation_store[[13]][[2]],
         var.name = "as.factor(major_occupation_label)")

love.plot(mediation_store[[13]][[2]],
          binary = "std")




















# MATCHING MODEL ---------------------------------------------------------------

# define cutoff for xenophobic prefs - trait 2 (I think)
# drop nas
ess_match_df <-
  ess_seventh_with_traits %>%
  mutate(xenophobic = ifelse(F2 > 1.5, 1, 0)) %>%
  dplyr::select(idno, cntry, vote,
                populist, farright, age_cat,
                hinctnta, edu_cat,
                lrscale_cat, citizen, F1, F2, 
                xenophobic, employment_rate,
                log_gdp, uemp3m) %>%
  drop_na()


# matching model
match_mod <-
  matchit(as.factor(xenophobic) ~
            hinctnta + as.factor(edu_cat) +
            as.factor(age_cat) +
            as.factor(lrscale_cat) + 
            as.factor(uemp3m) +
            employment_rate,
          data = ess_match_df,
          method = "cem")


# match diagnostics
match_df <- match.data(match_mod) %>%
  mutate(populist = ifelse(populist == 0, 0, 1),
         farright = ifelse(farright == 0, 0, 1))




# ----- estimate matching models

# far right
treat_mod_right <-
  glm(farright ~ xenophobic*
        (hinctnta + edu_cat + 
           lrscale_cat + uemp3m) +
        employment_rate + cntry,
      data = match_df,
      weights = weights)
summary(treat_mod_right)


# populist
treat_mod_pop <-
  glm(populist ~ xenophobic*
        (hinctnta_cat + edu_cat + 
           lrscale_cat +
           uemp3m) + cntry,
      data = match_df,
      weights = weights)

summary(treat_mod_pop)





# SIMULATING PREDICTED PROBABILITIES --------------------------------------------------

# define predicted prob function
pred_vals <- function(model, sims, cases){
  
  # draw parameter vector
  params <-
    mvrnorm(mu = matrix(coef(model)),
            n = sims,
            Sigma = matrix(vcov(model),
                           ncol = nrow(matrix(coef(model))),
                           nrow = nrow(matrix(coef(model)))))
  
  
  # simulate outcomes
  out <-
    params %*% t(cases) + 
    rnorm(n = sims, mean = mean(residuals(model)),
          sd = sd(residuals(model)))
  
  
  # transform to response scale
  inv.logit(out)
  
  
}



# ---------- for loop for diff cases


# fixed treat, vary college

case.treat.college <- 
  tibble(intercept = 1,
         xenophobic = 1,
         hinctnta_catmid_60 = 1,
         hinctnta_catupper_20 = 0,
         edu_catno_college = 0,
         lrscale_catliberal = 0,
         lrscale_catmoderate = 1,
         citizenyes = 1,
         uempla = 0,
         cntryBE = 0,
         cntryCH = 0,
         cntryCZ = 0,
         cntryDE = 0,
         cntryDK = 1,
         cntryES = 0,
         cntryFI = 0,
         cntryFR = 0,
         cntryGB = 0,
         cntryHU = 0,
         cntryIE = 0,
         cntryIL = 0,
         cntryLT = 0,
         cntryNL = 0,
         cntryNO = 0,
         cntryPL = 0,
         cntryPT = 0,
         cntrySE = 0,
         cntrySI = 0,
         xeno_hinc_mid60 = 1,
         xeno_hinc_upper_20 = 0,
         xeno_educat_no_college = 0,
         xeno_lrscale_lib = 0,
         xeno_lrscale_moderate = 1,
         xeno_citizen = 1,
         xeno_uempla = 0)


case.treat.no.college <- 
  tibble(intercept = 1,
        xenophobic = 1,
        hinctnta_catmid_60 = 1,
        hinctnta_catupper_20 = 0,
        edu_catno_college = 1,
        lrscale_catliberal = 0,
        lrscale_catmoderate = 1,
        citizenyes = 1,
        uempla = 0,
        cntryBE = 0,
        cntryCH = 0,
        cntryCZ = 0,
        cntryDE = 0,
        cntryDK = 1,
        cntryES = 0,
        cntryFI = 0,
        cntryFR = 0,
        cntryGB = 0,
        cntryHU = 0,
        cntryIE = 0,
        cntryIL = 0,
        cntryLT = 0,
        cntryNL = 0,
        cntryNO = 0,
        cntryPL = 0,
        cntryPT = 0,
        cntrySE = 0,
        cntrySI = 0,
        xeno_hinc_mid60 = 1,
        xeno_hinc_upper_20 = 0,
        xeno_educat_no_college = 1,
        xeno_lrscale_lib = 0,
        xeno_lrscale_moderate = 1,
        xeno_citizen = 1,
        xeno_uempla = 0)

case.no.treat.college <- 
  tibble(intercept = 1,
         xenophobic = 0,
         hinctnta_catmid_60 = 1,
         hinctnta_catupper_20 = 0,
         edu_catno_college = 0,
         lrscale_catliberal = 0,
         lrscale_catmoderate = 1,
         citizenyes = 1,
         uempla = 0,
         cntryBE = 0,
         cntryCH = 0,
         cntryCZ = 0,
         cntryDE = 0,
         cntryDK = 1,
         cntryES = 0,
         cntryFI = 0,
         cntryFR = 0,
         cntryGB = 0,
         cntryHU = 0,
         cntryIE = 0,
         cntryIL = 0,
         cntryLT = 0,
         cntryNL = 0,
         cntryNO = 0,
         cntryPL = 0,
         cntryPT = 0,
         cntrySE = 0,
         cntrySI = 0,
         xeno_hinc_mid60 = 0,
         xeno_hinc_upper_20 = 0,
         xeno_educat_no_college = 0,
         xeno_lrscale_lib = 0,
         xeno_lrscale_moderate = 0,
         xeno_citizen = 0,
         xeno_uempla = 0)

case.no.treat.no.college <- 
  tibble(intercept = 1,
         xenophobic = 0,
         hinctnta_catmid_60 = 1,
         hinctnta_catupper_20 = 0,
         edu_catno_college = 1,
         lrscale_catliberal = 0,
         lrscale_catmoderate = 1,
         citizenyes = 1,
         uempla = 0,
         cntryBE = 0,
         cntryCH = 0,
         cntryCZ = 0,
         cntryDE = 0,
         cntryDK = 1,
         cntryES = 0,
         cntryFI = 0,
         cntryFR = 0,
         cntryGB = 0,
         cntryHU = 0,
         cntryIE = 0,
         cntryIL = 0,
         cntryLT = 0,
         cntryNL = 0,
         cntryNO = 0,
         cntryPL = 0,
         cntryPT = 0,
         cntrySE = 0,
         cntrySI = 0,
         xeno_hinc_mid60 = 0,
         xeno_hinc_upper_20 = 0,
         xeno_educat_no_college = 0,
         xeno_lrscale_lib = 0,
         xeno_lrscale_moderate = 0,
         xeno_citizen = 0,
         xeno_uempla = 0)




# ----- prediction for treatment

# college
pred.vals.treat.college <-
  pred_vals(model = treat_mod_pop,
          sims = 1000,
          cases = case.treat.college)

# no college
pred.vals.treat.no.college <-
  pred_vals(model = treat_mod_pop,
            sims = 1000,
            cases = case.treat.no.college)


# ----- prediction for no treatment

# college
pred.vals.no.treat.college <-
  pred_vals(model = treat_mod_pop,
            sims = 1000,
            cases = case.no.treat.college)

# no college
pred.vals.no.treat.no.college <-
  pred_vals(model = treat_mod_pop,
            sims = 1000,
            cases = case.no.treat.no.college)


df <-
  rbind(pred.vals.treat.college,
        pred.vals.treat.no.college,
        pred.vals.no.treat.college,
        pred.vals.no.treat.no.college) %>%
  as.data.frame() %>%
  rename(out = 1) %>%
  mutate(treat_status = rep(c("treat", "no treat"),
                     each = 2000),
         college_status = 
           rep(c("college", "no college"),
               each = 1000,
               times = 2),
         out = as.numeric(out)) %>%
  group_by(treat_status, college_status) %>%
  summarise(avg_out = mean(out),
            upper_ci = quantile(out, probs = 0.95),
            lower_ci = quantile(out, probs = 0.05))

df %>%
  ggplot(aes(x = as.factor(treat_status),
             y = avg_out,
             col = college_status,
             group = college_status)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci,
                    x = as.factor(treat_status)),
                width = 0.1) +
  theme_minimal()






# SAVING DATA ----------------------------------

# save
models_and_plots <-
  list(ess_seventh_full,
       ess_seventh_main,
       main_df,
       plot_df,
       mod_A1, mod_A2, mod_A3,
       mod2_A,
       match_df_anti_imm, 
       match_mod_anti_imm,
       match_out_anti_imm,
       med_fit_quantity,
       med_out_fit_quantity,
       out_fit_quantity,
       cem_df,
       cem_df_short,
       cem_imm_match_df,
       cem_imm_mod,
       occ_table_by_anti_imm,
       industry_table_by_anti_imm, 
       vip_plot_humanvals,
       vip_plot_sociodem)


names(models_and_plots) <-
  c("ess_seventh_full",
    "ess_seventh_main",
    "main_df",
    "plot_df",
    "mod_A1", 
    "mod_A2", 
    "mod_A3",
    "mod2_A",
    "match_df_anti_imm", 
    "match_mod_anti_imm",
    "match_out_anti_imm",
    "med_fit_quantity",
    "med_out_fit_quantity",
    "out_fit_quantity",
    "cem_df",
    "cem_df_short",
    "cem_imm_match_df",
    "cem_imm_mod",
    "occ_table_by_anti_imm",
    "industry_table_by_anti_imm",
    "vip_plot_humanvals",
    "vip_plot_sociodem")

save(models_and_plots,
     file = "guilty_by_association_plot_and_models.RData")
       



 
                 
# APPENDIX PLOTS ---------------------------------------------------------

# Tables of most and least populist occupations, by country

                 
                 
                 
                 
                 
                 
                 

