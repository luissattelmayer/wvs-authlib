library(tidyverse)
library(purrr)
library(GGally)
library(corrr)
library(corrplot)
library(ggridges)
library(patchwork)



## Data Preparation
# this code is to understand the data management I have done prior to any analysis;
# after this script, I continue to work with the file called "wvs_7_cleaned.dta"

wvs <- haven::read_dta("data/WVS_TimeSeries_4_0.dta", encoding="latin1") 

# Filter for wave 7 + data management (mostly renaming the variables)
wvs <- wvs |> 
  # filtering for EU countries
  filter(COUNTRY_ALPHA %in% c("AUT", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
                                "FIN", "FRA", "DEU", "GBR", "GRC", "LTU",
                                "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP",
                                "SWE", "HUN", "ISL", "ITA", "LVA", "CHE", "NOR")) |> 
  # selecting needed variables
  select(S002VS, S003, COUNTRY_ALPHA, S006, E235, E111, E114, E115, E116, E117,
         E117B, E267, F118, F120, F121, F114C, F144_02, E290, D078, A124_02, A124_06, A124_10,
         A124_09, A124_05, A124_17, E007, H009, H010, H011, G006, E220, A006, F063,
         F028, E110, H008_07, X001, X003, X002, X025, X028, X045, X047_WVS, X050C,
         Y001) |> 
  # renaming selected variables
  rename(# dataset identifiers
         wave = S002VS,
         cntry_iso = S003, 
         country_name = COUNTRY_ALPHA,
         id = S006,
         # system variables
         imp_dem = E235,
         satis_pol_sys = E111,
         leader = E114, 
         technocracy = E115,
         army_rule = E116, 
         dem_good = E117,
         rel_aut = E117B,
         imp_elec = E267,
         # justification/acceptance of social values
         lgbtq = F118,
         abortion = F120,
         divorce = F121,
         beat_child = F114C,
         death_pen = F144_02,
         pol_violence = E290,
         sexism = D078,
         racism = A124_02,
         nativism = A124_06,
         homophobia = A124_09,
         islamophobia = A124_05, 
         antisemitism = A124_10, 
         antiromani = A124_17,
         # law & order/surveillance
         maintain_order = E007, 
         surveil_vid = H009,
         surveil_internet = H010,
         surveil_indiv = H011,
         nat_pride = G006,
         # faith/religiosity
         sci_faith = E220, 
         imp_rel = A006,
         imp_god = F063,
         rel_attend = F028,
         satis_dem_dev = E110, 
         free_equal = H008_07, 
         # controls
         gender = X001,
         age = X003, 
         yrbrn = X002,
         educ = X025,
         empl = X028, 
         sub_class = X045,
         inc = X047_WVS,
         urbrur = X050C, 
         postmat = Y001) |> 
  filter(wave == 7)











