# -------------------- descriptive statistics and tables for GUILTY BY ASSOCIATION --------------------

# setwd
setwd("C:/Users/eichi/Downloads/guilty_by_association_paper_copy")

# remove objects
rm(list = ls())


# ----- load libraries

# load libraries
library(tidyverse)
library(ggExtra)
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
library(wesanderson)
library(janitor)

select <- dplyr::select


# LOAD DATA -------------------------------------------------------------

# load ess
ess <- 
  read_rds("./internal_data/ess_2014_factor_analysis_and_populism_info.rds") %>%
  mutate(plain_populist = ifelse(populist == 1 & farright == 0 & farleft == 0, 1, 0),
         ches_party = case_when(pop.b == 1 & auth.b == 1 ~ "authoritarian and populist",
                                pop.b == 1 & auth.b == 0 ~ "populist",
                                pop.b == 0 & auth.b == 1 ~ "authoritarian",
                                pop.b == 0 & auth.b == 0 ~ "mainstream"),
         c_auth_populist = ifelse(ches_party == "authoritarian and populist", 1, 0),
         c_populist = ifelse(ches_party == "populist", 1, 0))

  

# TABLES ------------------------------------

# table of plain populist parties, by country
populist_shares_by_country <-
  ess %>%
  group_by(country_full) %>%
  summarise(plain_pop = sum(plain_populist, na.rm = TRUE),
            pop_ches = sum(pop.b, na.rm = TRUE),
            total_obs = n()) %>%
  mutate(plain_pop_share = round(plain_pop/total_obs, 2),
         pop_ches_share = round(pop_ches/total_obs, 2)) %>%
  arrange(desc(pop_ches_share))


# table of average auth and pop, by country
country_average_auth_pop <-
  ess %>%
  group_by(country_full) %>%
  summarise(average_populism = mean(pop_plural, na.rm = TRUE),
            average_authoritarianism = mean(auth_liberal, na.rm = TRUE)) %>%
  filter(!is.nan(average_populism)) %>%
  arrange(desc(average_authoritarianism))


# table of beliefs by industry, top 30 
table_most_populist_industries <-
  ess %>%
  group_by(nace_two_digit_name) %>%
  summarise(average_populism = round(mean(pop_plural, na.rm = TRUE), 2),
            average_authoritarianism = round(mean(auth_liberal, na.rm = TRUE), 2),
            average_imm_econ = round(mean(imm_econ, na.rm = TRUE), 2),
            average_imm_ethnic = round(mean(imm_ethnic, na.rm = TRUE), 2)) %>%
  arrange(desc(average_authoritarianism)) %>%
  slice_head(n = 30)

# correlations
table_most_populist_industries %>%
  ggplot(aes(x = average_populism, y = average_imm_ethnic)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# plot of offshorability and income, all people
p_offshore_income_all <-
  ess %>%
  filter(!is.na(hinctnta), !is.na(nace_broad_category_name)) %>%
  ggplot(aes(x = offshorability, y = hinctnta)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "offshorability", y = "income decile",
       title = "Bivariate Plot of Offshorability and Income",
       subtitle = "Data from 2014 European Social Survey") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))


# plot of offshorability and income, by industry
p_offshore_income_by_industry <-
  ess %>%
  filter(!is.na(hinctnta), !is.na(nace_broad_category_name)) %>%
  ggplot(aes(x = offshorability, y = hinctnta)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~nace_broad_category_name) +
  labs(x = "offshorability", y = "income decile",
       title = "Bivariate Plot of Offshorability and Income",
       subtitle = "By NACE rev.2 industry, 2014 European Social Survey") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))


# table of beliefs by occupation
table_most_populist_occupations <-
  ess %>%
  filter(major_occupation_label != "Armed Forces Occupations") %>%
  group_by(major_occupation_label) %>%
  summarise(average_populism = round(mean(pop_plural, na.rm = TRUE), 
                                     digits = 2),
            average_authoritarian = round(mean(auth_liberal, na.rm = TRUE),
                                          digits = 2),
            average_econ_qual = round(mean(acceptance_for_econ_qual_by_country, 
                                           na.rm = TRUE),
                                      digits = 2),
            average_ethnic_qual = round(mean(acceptance_for_ethnic_race_by_country, 
                                             na.rm = TRUE),
                                        digits = 2)) %>%
  ungroup() %>%
  arrange(desc(average_populism))



# GRAPHS ---------------------------------------------


# boxplot of income, by trade exposure
ess %>%
  filter(!is.na(trade_treat),
         country_full != "Hungary") %>%
  ggplot(aes(x = as.factor(trade_treat),
             y = hinctnta)) +
  geom_boxplot() +
  facet_wrap(~country_full) +
  labs(x = "trade exposure",
       y = "income decile",
       title = "Are trade exposed workers poorer than non-exposed workers?",
       subtitle = "By country")


# economic preferences and authoritarianism
ess %>%
  ggplot(aes(x = acceptance_for_econ_qual_by_country,
             y = auth_liberal)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "economic restrictions",
       y = "authoritarian score",
       title = "Economic immigration preferences and authoritarianism",
       subtitle = "Preferring more restrictions explains authoritarian score") +
  theme_minimal() +
  theme(axis.line = element_line())

# ethnic preferences and authoritarianism
ess %>%
  ggplot(aes(x = acceptance_for_ethnic_race_by_country,
             y = auth_liberal)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "ethnic restrictions",
       y = "authoritarian score",
       title = "Ethnic immigration preferences and authoritarianism",
       subtitle = "Preferring more restrictions explains authoritarian score") +
  theme_minimal() +
  theme(axis.line = element_line())


# isolate factors
factors <-
  ess %>%
  select(general_imm_pref_by_country,
         acceptance_for_ethnic_race_by_country,
         acceptance_for_econ_qual_by_country) %>%
  rename(imm_pref = general_imm_pref_by_country,
         eth_restrict = acceptance_for_ethnic_race_by_country,
         econ_restrict = acceptance_for_econ_qual_by_country) %>%
  drop_na()

# get correlation matrix
cor_mat <-
  cor(factors, method = "pearson")

# plot correlation matrix
ggcorrplot(cor_mat, hc.order = TRUE,
           outline.col = "white", lab = TRUE)



# plot distribution of latent ethnic and econ factors
# one for pooled, one for by country
latent_factor_df <-
  ess %>%
  select(country_full, general_imm_pref,
         acceptance_for_ethnic_race,
         acceptance_for_ethnic_race_by_country,
         acceptance_for_econ_qual,
         acceptance_for_econ_qual_by_country,
         lrscale_cat) %>%
  filter(lrscale_cat != "moderate")

# pooled data, ethnic
latent_factor_df %>%
  filter(country_full != "Austria") %>%
  ggplot(aes(x = acceptance_for_ethnic_race,
             fill = lrscale_cat, group = lrscale_cat)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("firebrick", "dodgerblue")) +
  facet_wrap(~country_full)

# pooled data, econ
latent_factor_df %>%
  filter(country_full != "Austria") %>%
  ggplot(aes(x = acceptance_for_econ_qual,
             fill = lrscale_cat, group = lrscale_cat)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("firebrick", "dodgerblue")) +
  facet_wrap(~country_full)


# plot distribution of
country_auth_and_pop_table <-
  ess %>%
  group_by(country_full) %>%
  summarise(average_auth = mean(auth_liberal, na.rm = TRUE),
            sd_auth = sd(auth_liberal, na.rm = TRUE),
            average_pop = mean(pop_plural, na.rm = TRUE),
            sd_pop = sd(pop_plural, na.rm = TRUE),) %>%
  ungroup() %>%
  arrange(desc(average_auth))


# PARTY CLASS --------------------------

# short df
short <-
  ess %>%
  select(rand_id, country_full, pop.b, auth.b, populist, farright, farleft, eurosceptic,
         hinctnta, imm_gen, imm_econ, imm_ethnic, lrscale) %>%
  rename(country = country_full,
         left_right = lrscale)

# adjust
short_parties <-
  short %>%
  mutate(ches_party = case_when(pop.b == 1 & auth.b == 1 ~ "authoritarian and populist",
                                pop.b == 1 & auth.b == 0 ~ "populist",
                                pop.b == 0 & auth.b == 1 ~ "authoritarian",
                                pop.b == 0 & auth.b == 0 ~ "mainstream"),
         populist_db = case_when(populist == 1 & farright == 0 & farleft == 0 ~ "plain populism",
                                 populist == 1 & farright == 1 ~ "far right populism",
                                 populist == 1 & farleft == 1 ~ "far left populism",
                                 populist == 0 & farright == 0 & farleft == 0 ~ "mainstream",
                                 populist == 0 & farright == 1 ~ "nationalist",
                                 populist == 0 & farleft == 1 ~ "socialist"))


# plot
parties_by_imm_ethnic <-
  short_parties %>%
  filter(!is.na(ches_party)) %>%
  ggplot(aes(x = imm_ethnic, col = ches_party, group = ches_party)) +
  geom_density() +
  scale_fill_manual(values = wes_palette(name = "Darjeeling2")) +
  labs(x = "strength of immigration restrictions", y = "densities",
       title = "Conditional Distributions of Ethnic Immigration Preferences",
       subtitle = "By type of preferred party, CHES data") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# plot
parties_by_polideo <-
  short_parties %>%
  filter(!is.na(ches_party), !is.na(left_right)) %>%
  ggplot(aes(x = left_right, col = ches_party, group = ches_party)) +
  geom_density(adjust = 2) +
  scale_fill_manual(values = wes_palette(name = "Darjeeling2")) +
  labs(x = "left-right scale, 0-10", y = "densities",
       title = "Conditional Distributions of Political Ideology",
       subtitle = "By type of preferred party, CHES data") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))





# SAVING -----------------------------------------

# make list of tabs
tables <- list(
  "populist_shares_by_country" = populist_shares_by_country,
  "data" = ess
  )
graphs <- list(
  "offshorability_and_income_all" = p_offshore_income_all,
  "offshorability_and_income_by_industry" = p_offshore_income_by_industry,
  "parties_by_polideo" = parties_by_polideo,
  "parties_by_imm_ethnic" = parties_by_imm_ethnic,
  "data" = short_parties
)

# make list
descripts <- 
  list(
    "tables" = tables,
    "graphs" = graphs
  )

# save
save(descripts, file = "final_guilty_by_association_descriptives.RData")

