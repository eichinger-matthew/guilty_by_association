# --------------------- data cleaning for GUILTY BY ASSOCIATION --------------------- 
# steps in order:

# combine datasets
# country-specific multiple imputation on survey questions only
# country-specific factor analysis

# set wd
setwd("C:/Users/eichi/Downloads/guilty_by_association_paper_copy")
rm(list = ls())
# load libraries
library(tidyverse)
library(janitor)
library(readxl)
library(haven)
library(countrycode)
library(psych)
library(mice)
# hard code select
select <- dplyr::select


# LOADING DATA ----------------------------------------------


# ------------ load ess data

# ess, seventh wave
ess <- read_dta("./external_data/ESS7e02_2.dta") %>% mutate(rand_id = row_number())
# subset voting data
ess_voting <- 
  ess %>%
  select(cntry, rand_id, essround, contains("prtcl"))


# ----- load trade, industry, nuts-level data
# one-digit industries
one_digit_trade <-
  read_rds("./internal_data/industry_exposure_un_comtrade_one_digit_industries.rds")  %>%
  mutate(eurostat_name = countrycode(reporter,
                                     origin = "country.name",
                                     destination = "eurostat"),
         eurostat_name = ifelse(eurostat_name == "UK",
                                "GB",
                                eurostat_name))

# two-digit industries
two_digit_trade <-
  read_rds("./internal_data/industry_exposure_un_comtrade_two_digit_industries.rds")  %>%
  mutate(eurostat_name = 
           countrycode(
             reporter, 
             origin = "country.name", 
             destination = "eurostat"),
         eurostat_name = ifelse(
           eurostat_name == "UK", "GB",eurostat_name)
         ) %>%
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
  read_rds("./internal_data/eurostat_nuts3_crime_population_employment_gdp.rds") %>%
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
  read_xlsx("./internal_data/nace_revision_2.xlsx") %>%
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
  read_csv("./external_data/isco08_english.csv") %>%
  mutate(unit = as.character(unit)) %>%
  arrange(unit) %>%
  mutate(across(.cols = c(minor, unit),
                .fns = ~str_replace_all(.x, "[X,Y]", "0"))) %>%
  dplyr::select(major) %>%
  distinct()

# ----- nuts employment data
nuts_emp <-
  read_csv("./external_data/employment_by_industry_nuts2.csv") %>%
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
  one_digit_trade %>%
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




# JOIN MAIN DATA ------------------------------------------------

# create ess clean
ess_clean <-
  ess %>%
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
         nacer2 = ifelse(nacer2 == 666 | nacer2 == 777 | nacer2 == 888, NA, nacer2),
         nacer2 = ifelse(str_length(nacer2) == 1,
                         str_pad(nacer2, width = 2,
                                 side = "left", pad = "0"), nacer2),
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
         eisced = ifelse(str_detect(eisced, "NA") | str_detect(eisced, "55"), NA, eisced),
         eisced = as.numeric(eisced),
         edu_cat = 
           case_when(eisced <= 4 ~ "no college", 
                     eisced == 5 ~ "vocational",
                     eisced > 5 ~ "college"),
         religious_cat =
           case_when(rlgdgr < 5 ~ "not very religious",
                     rlgdgr  == 5 ~ "somewhat religious",
                     rlgdgr > 5 ~ "religious"),
         citizen = case_when(ctzcntr == 1 ~ "yes", ctzcntr == 2 ~ "no"),
         freehms = ifelse(str_detect(freehms, "NA"), NA, freehms),
         freehms = as.integer(freehms),
         euftf = ifelse(str_detect(euftf, "NA"), NA, euftf),
         euftf = as.numeric(euftf)) %>%
  mutate(pplstrd = case_when(pplstrd > 0 & pplstrd < 7 ~ (1-pplstrd+5)),
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
                .fns = ~ifelse(.x < 1 | .x > 6, NA, .x))) %>%
  zap_labels(.)


# join ess clean with trade and occupation data
ess_with_trade_occs <-
  ess_clean %>%
  left_join(nuts3,
            by = c("region" = "geo",
                   "year" = "year")) %>%
  left_join(two_digit_trade,
            by = c("cntry" = "eurostat_name",
                   "nacer2" = "nace_two_digit_code",
                   "year" = "year")) %>%
  select(-nace_two_digit_name) %>%
  left_join(occs,
            by = c("major_occupation_group" = "major")) %>%
  left_join(nace2_info,
            by = c("nacer2" = "nace_two_digit_code")) %>%
  left_join(nuts_emp_trade,
            by = c("cntry" = "country",
                   "region" = "geo"))
  

# # ----- poppa
# 
# # poppa dataset
# poppa <- read_rds("poppa_measures_of_eu_parties.rds") %>%
#   mutate(nationalist = ifelse(median_nativism >= 7, 1, 0),
#          middle_nationalist = ifelse(nationalist == 1 & 
#                                        median_lroverall > 2 & 
#                                        median_lroverall < 8, 1, 0),
#          right_nationalist = ifelse(nationalist == 1 & median_lroverall >= 8, 1, 0),
#          left_nationalist = ifelse(nationalist == 1 & median_lroverall <= 2, 1, 0),
#          populist_not_nationalist = 
#            ifelse(median_antielitism >= 7 & 
#                     median_peoplecentrism >= 7 &
#                     median_nativism < 7, 1, 0),
#          populist_nationalist = 
#            ifelse(median_antielitism >= 7 & 
#                     median_peoplecentrism >= 7 &
#                     median_nativism >= 7, 1, 0)) %>%
#   rename(far_right_nationalist = right_nationalist,
#          far_left_nationalist = left_nationalist)
# 
# # merge poppa and ess
# ess_parties_with_poppa <-
#   parties_ess %>%
#   left_join(poppa, by = c("partyfacts_id"))
# 
# 
# # filter to prtcl questions to avoid double-counting (annoying otherwise)
# ess_poppa_prtcl_questions <-
#   ess_parties_with_poppa %>%
#   filter(str_detect(ess_variable, "prtcl")) %>%
#   unite(col = "link_code",
#         c(ess_cntry, essround, ess_party_id),
#         sep = "_",
#         remove = FALSE) %>%
#   select(link_code, ess_cntry, essround,
#          ess_variable, ess_party_id,
#          partyfacts_id, median_intradem:populist_nationalist)




# # MULTIPLE IMPUTATION ---------------------------------------------
# 
# 
# # ----- ONLY RUN IF YOU WANT TO RE-RUN MULTIPLE IMPUTATION STAGE
# 
# # put data into country list
# # use multiple imputation to fill in NA observations that ARE NOT votes
# ess_short <-
#   ess_with_trade_occs %>%
#   select(cntry, region, rand_id, pspwght,
#          clsprty, lrscale, blgetmg, uempli, uempla,
#          uemp3m, uemp12m, uemp5yr,
#          agea, gndr, eisced, nacer2, isco08,
#          hinctnta, hincfel, lrscale_cat:citizen,
#          major_occupation_group:major_occupation_label,
#          contains("trst"), contains("stf"), ipcrtiv:iplylfr,
#          qfimedu:qfimcmt, pplstrd, gvrfgap,
#          smegbli, smegbhw, smctmbe,
#          imbgeco, imueclt, imwbcnt, imtcjob, imbleco,
#          imwbcrm, imdetbs, imdetmr, lwdscwp, rlgueim,
#          population:log_gdp, employment_2008,
#          employment_change_since_2008,
#          industry_imports:log_import_exposure)
# 
# # select only descriptive chars and subjective sentiments - NO VOTING DATA
# ess_multimp_questions <-
#   ess_short %>%
#   select(cntry, rand_id, uempli, uempla, uemp3m, agea, gndr,
#          edu_cat, lrscale_cat, hinctnta_cat, religious_cat,
#          major_occupation_label, contains("trst"), contains("stf"),
#          ipcrtiv:iplylfr, qfimedu:qfimcmt, pplstrd, gvrfgap,
#          smegbli, smegbhw, smctmbe, imbgeco, imueclt,
#          imwbcnt, imtcjob, imbleco, imwbcrm, imdetbs, imdetmr,
#          lwdscwp, rlgueim)
# # 
# # grouped list
# ess_grouped_list <-
#   ess_multimp_questions %>%
#   group_by(cntry) %>%
#   group_split(.keep = FALSE)
# 
# # define multiple imputation function
# mult_impute <- function(df){
#   
#   # multiple imputation
#   temp <- mice(df)
#   
#   # get df
#   complete_temp <- complete(data = temp)
#   
#   return(complete_temp)
# }
# 
# # apply multiple imputation to list
# ess_imputed <-
#   ess_grouped_list %>%
#   map(mult_impute)
# 
# # extract grouped data
# ess_imputed_full <-
#   ess_imputed %>%
#   do.call(rbind, .)
# 
# # recombine with other data
# ess_imputed_final <-
#   ess_imputed_full %>%
#   left_join(ess_short, by = c("rand_id")) %>% 
#   select(-contains(".y")) %>%
#   rename_with(.fn = ~str_remove_all(.x, "\\.x"))
# 
# # # save
# # saveRDS(ess_imputed_final,
# #         file = "ess_short_2014_imputed.rds")


# FACTOR ANALYSIS --------------------------------------------------

# load multiply-imputed df
ess_imputed_final <-
  read_rds("./internal_data/ess_short_2014_imputed.rds")

# two versions: (1) pooled and (2) by country

# use common dataset
ess_fa <-
  ess_imputed_final %>%
  select(rand_id, cntry, qfimedu:qfimcmt,
         pplstrd, gvrfgap, smegbli, smegbhw, smctmbe,
         imbgeco, imueclt, imwbcnt, imtcjob, imbleco,
         imwbcrm, imdetbs, imdetmr, lwdscwp, rlgueim)

# -------------------- POOLED

# only have to estimate model and explore latent factors

# estimate model with different number of latent factors (1-10)
# save data for elbow plot

# results storage
fa_store <- list()
# latent factor index
n_factors <- seq(1, 10, 1)
for(i in seq_along(n_factors)){
  # run fa model
  temp <- fa(ess_fa[,-c(1,2)], nfactors = n_factors[i], rotate = "oblimin")
  
  # for when there is only one latent factor
  if(i == 1){
    # get unaccounted variance
    var_exp <- max(temp$Vaccounted[2,])
    fa_store[[i]] <- var_exp
    # when more than 1 latent factor
  }else{
    # get cumulative variance
    var_exp <- max(temp$Vaccounted[3,])
    fa_store[[i]] <- var_exp
  }
}


# unravel list into df
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



# ----------- settle on four factors

# estimate fa
fa_oblimin <- fa(ess_fa[,-c(1,2)], nfactors = 4, rotate = "oblimin")
fa_varimax <- fa(ess_fa[,-c(1,2)], nfactors = 3, rotate = "varimax")

# check structure
fa_oblimin$Vaccounted
fa_oblimin$Structure
fa_oblimin$loadings
fa_oblimin$score.cor


fa_varimax$Vaccounted
fa_varimax$Structure
fa_varimax$loadings


# ----- semantic meaning of factors

# factor 1 is general approval
# factor 2 is acceptance, given good economic qualifications
# factor 3 is racial and ethnic control over immigrants
# factor 4 is general racism

# get factor scores and bind to original data
fscores_oblimin <- 
  as.data.frame(fa_oblimin$scores)

fscores_varimax <-
  as.data.frame(fa_varimax$scores) %>%
  rename(general_imm_pref_varimax = MR1,
         econ_qual_varimax = MR2,
         ethnic_race_varimax = MR3)


ess_with_fa <-
  ess_imputed_final %>%
  bind_cols(fscores_oblimin) %>%
  bind_cols(fscores_varimax) %>%
  rename(general_imm_pref_oblimin = MR1,
         acceptance_for_econ_qual_oblimin = MR2,
         acceptance_for_ethnic_race_oblimin = MR3,
         racism_oblimin = MR4)

ess_with_fa %>%
  ggplot(
    aes(x = econ_qual_varimax, y = ethnic_race_varimax, 
        col = cntry, group = cntry)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)



# ----- produce table of factor loadings for paper

# get df of loadings
nquests <- ncol(ess_fa[,-c(1,2)])
num_factors <- 4

# make table of factor analysis results
factor_info <-
  as.data.frame(fa_oblimin$loadings[1:nquests,1:num_factors]) %>%
  rename(general_imm_pref = MR1,
         acceptance_for_econ_qual = MR2,
         acceptance_for_ethnic_race = MR3,
         racism = MR4) %>%
  mutate(across(.cols = everything(),
                .fns = ~ifelse(.x < 0.10 & .x > -0.10, "--", round(.x, digits = 2))),
         survey_question = row.names(.)) %>%
  remove_rownames() %>%
  arrange(survey_question) %>%
  select(survey_question, everything())

# output to file
# write_csv(factor_info,
#           file = "factor_analysis_loadings.csv")


# ----- produce figures of factor scores by political belief, by country

# econ factor
# do not include austria, ess did not ask questions about boss and marriage
facet_plot_econ_factors <-
  ess_with_fa %>%
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
  ggplot(aes(x = acceptance_for_econ_qual_oblimin,
             group = lrscale_cat,
             fill = lrscale_cat)) +
  geom_density(alpha = 0.6) +
  scale_fill_discrete(name = "Left-Right") +
  lims(x = c(-3, 3)) +
  facet_wrap(~cntry_full)


# racial and ethnic factor
# same, but for cultural
facet_plot_cultural_factors <-
  ess_with_fa %>%
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
  ggplot(aes(x = acceptance_for_ethnic_race_oblimin,
             group = lrscale_cat,
             fill = lrscale_cat)) +
  geom_density(alpha = 0.6) +
  scale_fill_discrete(name = "Left-Right") +
  lims(x = c(-3, 3)) +
  facet_wrap(~cntry_full)







# ------------------------------- BY COUNTRY

# do not need to reproduce table and figures
# fix factor number at four

# create loop index
countries <- unique(ess_fa$cntry)
fa_bycountry_store <- list()
for(i in seq_along(countries)){
  
  # get data
  tempdf <-
    ess_fa %>%
    filter(cntry == countries[i]) %>%
    remove_empty(which = "cols")
  
  # estimate model
  tempmod <- 
    fa(tempdf[,-c(1,2)], nfactors = 3, rotate = "oblimin")
  
  # get scores
  tempscores <- 
    as.data.frame(tempmod$scores) %>%
    rename(general_imm_pref = MR1,
           acceptance_for_econ_qual = MR2,
           acceptance_for_ethnic_race = MR3)
  
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
         acceptance_for_ethnic_race_by_country = acceptance_for_ethnic_race)

# join to ess_with_fa data
ess_fa_full <-
  ess_imputed_final %>%
  left_join(combined_fa_df, by = c("rand_id", "cntry"))

# save data
# saveRDS(ess_fa_full,
#         file = "ess_2014_with_factor_analysis_scores.rds")


ess_fa_full <- 
  read_rds("./internal_data/ess_2014_with_factor_analysis_scores.rds")

# JOIN ESS WITH PARTY POPULISM ---------------------------

# get people who declined to feeling close
decliners <-
  ess_fa_full %>%
  left_join(ess_voting, by = c("cntry", "rand_id")) %>%
  mutate(has_close_feeling = case_when(clsprty == 1 ~ "yes",  clsprty == 2 ~ "no")) %>%
  filter(has_close_feeling == "no")

# get people who feel close
closers <-
  ess_fa_full %>%
  left_join(ess_voting, by = c("cntry", "rand_id")) %>%
  mutate(has_close_feeling = case_when(clsprty == 1 ~ "yes", clsprty == 2 ~ "no")) %>%
  filter(has_close_feeling == "yes") %>%
  mutate(party_closest_to = rowSums(across(contains("prtcl")), na.rm = TRUE),
         party_closest_to = 
           ifelse(party_closest_to == 66 | party_closest_to == 77 |
                    party_closest_to == 88 | party_closest_to == 99 |
                    party_closest_to == 0, NA, party_closest_to)) %>%
  select(-contains("prtcl"))

# get people who feel close but do not say which
silent_closers <-
  closers %>%
  filter(is.na(party_closest_to))

# get people who say which party they feel closest to
public_closers <-
  closers %>%
  filter(!is.na(party_closest_to))




# -------------------- get to work on linking codes

# load ess prtv data from github
ess_prtv_codes <-
  read_csv("https://raw.githubusercontent.com/hdigital/partyfactsdata/main/import/essprtc/essprtc.csv")

# extract unique codes from current df
codes <-
  public_closers %>%
  mutate(party_vartype = "c") %>%
  unite(c(cntry, essround, party_closest_to, party_vartype), 
        col = "dataset_party_id", sep = "-", remove = FALSE) %>%
  distinct(dataset_party_id) %>%
  pull()

# string match from current df to ess prtv data
# storage vector
store <- vector(length = length(codes))
# loop
for(i in seq_along(codes)){
  # loop over codes in storage
  for(j in seq_along(ess_prtv_codes$ess_ids)){
    # if pattern matches
    if(grepl(codes[i], ess_prtv_codes$ess_ids[j]) == TRUE){
      # grab partyfacts code
      temp <- ess_prtv_codes$partyfacts_id[j]
      # store
      store[i] <- temp
    }
  }
}

# combine store with codes
match_ids <-
  cbind(codes, store) %>%
  data.frame() %>%
  rename(partyfacts_id = store)

# add matchids back to data
public_closers2 <-
  public_closers %>%
  mutate(party_vartype = "c") %>%
  unite(c(cntry, essround, party_closest_to, party_vartype), 
        col = "dataset_party_id", sep = "-", remove = FALSE) %>%
  left_join(match_ids, by = c("dataset_party_id" = "codes")) %>%
  mutate(partyfacts_id = as.numeric(partyfacts_id))


# load partyfacts database with code keys
pfacts <-
  read_csv("./external_data/partyfacts-external-parties.csv")

# PopuList
pop <- read_xlsx("./external_data/populist-version-2-20200626.xlsx") %>%
  select(populist, farright, farleft, eurosceptic, partyfacts_id)

# CHES
ches <- read_rds("./internal_data/pippa_inglehart_ches_party_scores_clean.rds") %>%
  select(ches_partyid, auth_liberal, pop_plural)

# load parlgov data
parlgov <- read_csv("https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_party.csv")

# get ches link codes
# adjust for vlaams belang (change 553 to 1968)
ches_link <-
  pfacts %>%
  filter(dataset_key == "ches") %>%
  inner_join(ches, by = c("dataset_party_id" = "ches_partyid")) %>%
  mutate(partyfacts_id = ifelse(partyfacts_id == 553, 1968, partyfacts_id)) %>%
  select(partyfacts_id, auth_liberal, pop_plural)


# clean parlgov data
pfacts_parl <- 
  pfacts %>%
  filter(dataset_key == "parlgov") %>%
  mutate(dataset_party_id = as.numeric(dataset_party_id)) %>%
  inner_join(parlgov, by = c("dataset_party_id" = "party_id")) %>%
  select(partyfacts_id, family_name, name_english, name_short) %>%
  rename(parlgov_family = family_name) %>%
  filter(!is.na(partyfacts_id)) %>%
  group_by(partyfacts_id) %>%
  mutate(num_obs = n()) %>%
  filter(num_obs == 1) %>%
  select(-num_obs)

# link to party populism data
public_closers3 <-
  public_closers2 %>%
  left_join(pfacts_parl, by = c("partyfacts_id")) %>%
  left_join(pop, by = c("partyfacts_id")) %>%
  left_join(ches_link, by = c("partyfacts_id")) %>%
  mutate(across(c(populist, farright, farleft, eurosceptic), ~ifelse(is.na(.x), 0, .x)),
         mainstream = ifelse(populist == 0 & farright == 0 & farleft == 0 & eurosceptic == 0, 1, 0))




# -------------- add last-minute predictors that I forgot to put in earlier

hincome <- read_rds("./internal_data/ess_short_2014_imputed.rds") %>%
  select(rand_id, hinctnta)

# load data
public_closers6 <- read_rds("./internal_data/ess_2014_factor_analysis_and_populism_info.rds")

# load isco crosswalks
isco.cross <- read_xls("./external_data/isco08_xwalk_d.xls")

isco.cross.keep <-
  isco.cross %>%
  select(isco08, isco88)

# load own-johnston data
oj <- read_dta("./external_data/owen_johnston_2017.dta")

# get occupation-level info
oj.occs <-
  oj %>%
  select(isco88, routineness, rti, offshor2, c_relcog, c_relman) %>%
  distinct() %>%
  mutate(across(c(routineness:c_relman), ~as.numeric(scale(.x)))) %>%
  rename(routine_task_intensity = rti,
         offshorability = offshor2,
         cog_intense_relative = c_relcog,
         manual_intense_relative = c_relman)

# just want to add one-digit nace info and some final cleans

# load nace codes
nace <- 
  read_xlsx("./internal_data/nace_revision_2.xlsx") %>%
  select(nace_broad_category_name, nace_two_digit_code) %>%
  distinct()

# load exposure
exposure <- read_rds("./external_data/industry_exposure_un_comtrade_one_digit_industries.rds") %>%
  filter(!is.na(nace_broad_category)) %>%
  mutate(across(c(industry_imports_baseline_year, industry_exports_baseline_year,
                  industry_trade_balance_baseline_year),
                ~ifelse(is.na(.x), 0, .x)),
         lr.exp.rat = (industry_exports + 1)/(industry_exports_baseline_year + 1),
         lr.imp.rat = (industry_imports + 1)/(industry_imports_baseline_year + 1),
         tb.lr = industry_trade_balance - industry_trade_balance_baseline_year,
         lr.impexp.rat = lr.imp.rat/lr.exp.rat,
         log_lr.impexp.rat = round(log(lr.impexp.rat), 2),
         country = countrycode(reporter, "country.name", "eurostat")) %>%
  filter(year == 2014) %>%
  select(country, nace_broad_category_name, log_lr.impexp.rat)


public_closers4 <-
  public_closers3 %>%
  rename(occ = major_occupation_label) %>%
  mutate(lrimpexp = log(longrun_import_export_ratio),
         country_full = countrycode(cntry, "iso2c", "country.name"),
         auth_liberal = as.numeric(auth_liberal),
         pop_plural = as.numeric(pop_plural),
         auth.b = ifelse(auth_liberal >= 75, 1, 0),
         pop.b = ifelse(pop_plural >= 75, 1, 0),
         imm_gen = general_imm_pref_by_country,
         imm_econ = acceptance_for_econ_qual_by_country,
         imm_ethnic = acceptance_for_ethnic_race_by_country,
         edu_cat = factor(edu_cat),
         edu_cat = as.factor(edu_cat),
         occ =
           case_when(
             occ == "Skilled Agricultural, Forestry, and Fishery Workers" ~ "ag, forestry, fishery",
             occ == "Clerical Support Workers" ~ "clerical support",
             occ == "Technicians and Associate Professionals" ~ "techs",
             occ == "Professionals" ~ "professionals",
             occ == "Craft and Related Trades Workers" ~ "craft and trades",
             occ == "Managers" ~ "managers",
             occ == "Service and Sales Workers" ~ "service and sales",
             occ == "Plant and Machine Operators, Assemblers" ~ "plant, machine, assembly",
             occ == "Elementary Occupations" ~ "elementary"),
         occ = as.factor(occ),
         occ_treat = case_when(
           occ == "craft and trades" |
             occ == "plant, machine, assembly" |
             occ == "elementary" ~ 1,
           occ == "ag, forestry, fishery" |
             occ == "clerical support" |
             occ == "techs" |
             occ == "professionals" |
             occ == "managers" |
             occ == "service and sales" ~ 0)) %>%
  left_join(nace, by = c("nacer2" = "nace_two_digit_code")) %>%
  left_join(exposure, by = c("cntry" = "country", "nace_broad_category_name")) %>%
  mutate(log_lr.impexp.rat = 
           ifelse(is.na(log_lr.impexp.rat), 0, log_lr.impexp.rat))

# add some OTHER ones I forgot
public_closers5 <-
  public_closers4 %>%
  mutate(occ_treat = case_when(
    occ == "craft and trades" |
      occ == "plant, machine, assembly" |
      occ == "elementary" ~ 1,
    occ == "ag, forestry, fishery" |
      occ == "clerical support" |
      occ == "techs" |
      occ == "professionals" |
      occ == "managers" |
      occ == "service and sales" ~ 0),
    current_unemployment = ifelse(uempla == 1, 1, 0),
    been_unemployed = uemp3m)

public_closers6 <-
  public_closers5 %>%
  select(rand_id, cntry, country_full, region, agea, edu_cat, lrscale, occ, occ_treat,
         isco08, nacer2, nace_broad_category_name, log_lr.impexp.rat,
         current_unemployment, been_unemployed, imm_gen, imm_econ, imm_ethnic,
         name_english, parlgov_family, populist, farright, farleft, eurosceptic,
         mainstream, auth_liberal, pop_plural, auth.b, pop.b,
         starts_with("trst"), starts_with("stf")) %>%
  left_join(isco.cross.keep, by = c("isco08")) %>%
  left_join(oj.occs, by = c("isco88"))

public_closers7 <-
  public_closers6 %>%
  select(rand_id, cntry, country_full, region, agea, edu_cat, lrscale, occ, occ_treat,
         isco08, nacer2, nace_broad_category_name, log_lr.impexp.rat,
         current_unemployment, been_unemployed, imm_gen, imm_econ, imm_ethnic,
         name_english, parlgov_family, populist, farright, farleft, eurosceptic,
         mainstream, auth_liberal, pop_plural, auth.b, pop.b,
         starts_with("trst"), starts_with("stf")) %>%
  left_join(hincome, by = c("rand_id")) %>%
  left_join(isco.cross.keep, by = c("isco08")) %>%
  left_join(oj.occs, by = c("isco88"))



# ----- save final datasets

# do not feel close
saveRDS(decliners, file = "does_not_feel_close_to_party.rds")
# feels close but does not say which
saveRDS(silent_closers, file = "feels_close_but_does_not_say_which.rds")
# save cleaned dataset
saveRDS(public_closers7, file = "ess_2014_factor_analysis_and_populism_info.rds")




