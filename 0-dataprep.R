library(tidyverse)
library(purrr)
library(GGally)
library(corrr)
library(corrplot)
library(ggridges)
library(patchwork)



## Data Preparation
# this code is to understand the data management I have done prior to any analysis
# after this script, I continue to work with the file called "wvs_7_cleaned.dta"

wvs <- haven::read_dta("data/WVS_TimeSeries_4_0.dta", encoding="latin1") 

# Filter for wave 7 + data management (mostly renaming the variables)
wvs <- wvs |> 
  # filtering for countries of AUTHLIB
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



## distributions per variable
# code for individual histograms & facet wrap per country
# by changing "imp_dem" to any other variable of the dataset, you can inspect 
# the distribution of any other variable

wvs_7 |> 
  mutate(imp_dem = if_else(imp_dem < 0, NA, imp_dem)) |> 
  ggplot(aes(imp_dem, fill = country_name)) +
  geom_histogram() +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Distribution of Importance of Democracy per Country between 2017-2022") +
  facet_wrap(~ country_name, scales = "free")

# density ridges (less interpretable than a histogram for heavily skewed variables)
wvs |> 
  mutate(imp_dem = if_else(imp_dem < 0, NA, imp_dem)) |> 
  ggplot(aes(imp_dem, country_name, fill = country_name)) +
  geom_density_ridges() +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none") +
  ggtitle("Distribution of Importance of Democracy per Country between 2017-2022")



# creating a function that creates a histogram for every variable of a dataset
plot_histogram <- function(data, variable_name) {
  ggplot(data, aes(x = {{ variable_name }})) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", enquo(variable_name)),
         x = quo_name(variable_name),
         y = "Frequency") +
    theme_minimal()
}


# creating a tibble object which contains nested data frames for each country and
# in a second column the nested plots of a variable per country
# by changing imp_dem to your variable of interest, you can 
histograms <- wvs_7 |>
  filter(imp_rel >= 0) |>  # Filter values >= 0
  nest(data = -country_name) |>
  mutate(
    histograms = pmap(
      .l = list(country_name, data),
      .f = function(country_name, data) {
        ggplot(data, aes(x = imp_rel)) +
          geom_histogram(binwidth = 1, alpha = 0.8, color = "black") +
          scale_fill_brewer("Set1") +
          labs(
            title = as.character(country_name)
          )
      }
    )
  )



histograms$histograms |>
  reduce(`+`)




## reshaping the data: pivoting to longer format so that each row is one variable
# value per country 
wvs_cleaned <- wvs |>
  select(-id, -wave) |>
  pivot_longer(cols = -c(country_name), 
               names_to = "variable", 
               values_to = "value") |> 
  filter(is.numeric(value) & !is.na(value) & value >= 0)


# Creating a function for the histograms for each individual variable that will
# be displayed in a facet wrap per country to compare distributions visually
# across cases. The first part subsets the given data to keep only the rows where
# the variable column matches the specified var_name. This ensures that only the
# data related to the desired variable is used for plotting. 

create_histograms <- function(data, var_name) {
  # Subset data for the variable
  data_subset <- data |> 
    filter(variable == var_name)
  
  # Create the histogram plot by using ggplot, storing the output in plot object
  plot <- ggplot(data_subset, aes(x = value)) + 
    geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
    facet_wrap(~ country_name, scales = "free_y") +
    labs(title = paste("Histogram for", var_name),
         x = var_name,
         y = "Count")
  
  # in order to see all the plots for every variable in your plot panel in R, we
  # need to print it at the end of the function, this might take some time
  print(plot)
}


# unique() so that we "map"/"walk" our created function over each variable name
# only once; this might take some seconds to finish. Once the code is done running,
# you should be able to see the plots in your plot panel. To see all the others,
# use the arrow symbols in the panel to click through them

unique(wvs_cleaned$variable) |>
  purrr::walk(~ create_histograms(wvs_cleaned, .x))











