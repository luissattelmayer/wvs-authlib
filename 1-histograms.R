library(tidyverse)
library(purrr)
library(ggridges)
library(patchwork)


wvs <- haven::read_dta("wvs_7_cleaned.dta")

# or if you have downloaded the whole folder with the project and data file
# load manually

wvs <- haven::read_dta("data/wvs_7_cleaned.dta")

glimpse(wvs)


## distributions per variable
# code for individual histograms & facet wrap per country
# by changing "imp_dem" to any other variable of the dataset, you can inspect 
# the distribution of any other variable

wvs |> 
  mutate(imp_dem = if_else(imp_dem < 0, NA, imp_dem)) |> 
  ggplot(aes(imp_dem, fill = country_name)) +
  geom_histogram() +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Distribution of Importance of Democracy per Country between 2017-2022") +
  facet_wrap(~ country_name, scales = "free")

# density ridges 
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
histograms <- wvs |>
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


# To view all the plots together, you can use the reduce() function in 
# combination with the patchwork package. The patchwork package allows you to 
# combine plots into a larger plot using the plus operator (+):

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
    geom_histogram(binwidth = 1, fill = "grey", color = "black") +
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

