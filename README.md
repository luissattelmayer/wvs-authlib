# Overview
This is a collaborative GitHub repository to work on the World Value Survey estimates. 


## Dataprep
The first document entitled `0-dataprep.R` is the data wrangling and management that was done prior to all analyses. Mostly renaming variables and filtering for the waves and countries of interest. 

## Distribtuions
The second document entitled `1-histograms.R` uses the cleaned version of the data (end of first doc) and plots the distributions in several different ways over the whole dataset. The final output which can be generated via this script are more than 40 plots of all the variables' distributions. 

## Models & Plots
The third document contains code that creates models for all of the variables as DVs. The predictors used are classic socio-economic independent variables of urban-rural, income, age, and gender. 

## Quarto-scripts
A rendered, annotated and detailled version of the code including the outputs can be found in the `quarto-sript.qmd` (raw code) and the `quarto-script.html` file.


