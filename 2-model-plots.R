library(tidyverse)
library(purrr)
library(broom)


wvs <- haven::read_dta("data/wvs_7_cleaned.dta")


# subsetting the df for model; only keeping positive/equal to 0 values and 
# dropping unnecessary variables
wvs_modeling <- wvs |> 
  select(-c(wave, cntry_iso, id)) |> 
  mutate(across(everything(), ~ pmax(., 0))) |> 
  filter(gender > 0)


# simple regression model in which we can change the dependent variable and then
# compare the coefficients one by one
m1 <- lm(imp_dem ~ urbrur + gender + inc + age, wvs_modeling)
broom::tidy(m1)
summary(m1)


# disaggregating by country

wvs_modeling_cze <- wvs |> 
  select(-c(wave, cntry_iso, id)) |> 
  mutate(across(everything(), ~ pmax(., 0))) |> 
  filter(gender > 0 & country_name == "CZE")

m2 <- lm(imp_dem ~ urbrur + gender + inc + age, wvs_modeling_cze)
broom::tidy(m2)
summary(m2)



# running country-specific models on every variable of the dataset

model_data <- wvs |>
  # drop unimportant variables
  select(-wave, -cntry_iso, -id) |>
  # get rid of negative values measuring "no response", "not asked" etc items
  mutate(across(everything(), ~ pmax(., 0))) |>
  # pivoting to longer format for later use
  pivot_longer(cols = -c(country_name, gender, age, educ, inc, urbrur),
               names_to = "variable",
               values_to = "value") |> 
  group_by(variable, country_name) |>
  # nesting into tibbles per country and variable
  nest() |>
  # within nested lists create models per country and variable
  mutate(models = map(data, ~lm(value ~ gender + age + educ + inc + urbrur, data = .x)),
         # get information on every model (CIs, SEs, p-value etc)
         tidied = map(models, ~tidy(.x, conf.int = TRUE, exponentiate = TRUE)),
         # goodness of fit measures
         glanced = map(models, glance),
         # fitted values/predicted
         augmented = map(models, augment),
         # extract residuals manually
         residuals = map(augmented, ~tibble(residuals = .x$.resid)))




# extract (unnest) the list of model information for each country and variable
coef_data <- model_data |>
  unnest(tidied) |>
  select(variable, country_name, term, estimate, conf.low, conf.high) |>
  # get rid of Intercept estimate
  filter(term != "(Intercept)")

unique_vars <- model_data |> 
  distinct(variable) |> 
  pull(variable)

# create function to plot all the coefficients of all the different variables
plot_coefs <- function(data, variable_name) {
  # Filter the tidied data for the specific variable
  filtered_data <- data |>
    filter(variable == variable_name)
  
  ggplot(filtered_data, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
    coord_flip() +
    labs(title = paste("Coefficient Plot for", variable_name),
         y = "Estimate",
         x = "") +
    facet_wrap(~ country_name)
}

# Use walk to plot coefficients for each variable in country facet wrap
unique_vars[1:36] |> 
  walk(~print(plot_coefs(coef_data, .x)))



