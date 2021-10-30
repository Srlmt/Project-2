Project 2 - Predictive Modeling and Automation
================
Joey Chen and John Williams
10/28/2021

# Purpose

The purpose of this project is to use automation to show different predictive model fitting using linear regression models and the ensemble tree-based models. We use the [Online News Popularity Dataset](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity) to try to predict the number of shares with numerous predictors. And We separate the analysis by six different data channels by using automation in R Markdown. 

# Required Packages

The following packages are required for this project:

  - `tidyverse`: Useful data tools for transforming and visualizing data
  - `corrplot`: Visual exploratory tool on correlation matrix
  - `caret`: Tools to help create predictive models
  - `modelr`: Modeling functions such as rmse

# Links to Analyses

  - The analysis for [Business articles is available here](BusAnalysis.md)
  - The analysis for [Entertainment articles is available here](EntertainmentAnalysis.md)
  - The analysis for [Lifestyle articles is available here](LifestyleAnalysis.md)
  - The analysis for [Social Media articles is available here](SocmedAnalysis.md)
  - The analysis for [Tech articles is available here](TechAnalysis.md)
  - The analysis for [World articles is available here](WorldAnalysis.md)

# Render Code

The following code is used in `renderProject.Rmd` file to automate and render documents from different channels. 

``` r
# Description: This program renders Project-2 and outputs the documents

# Necessary packages
library(stringr)

# Create channel names
channels <- c("lifestyle", "entertainment", "bus", "socmed", "tech", "world")

# Create filenames
output_file <- paste0(str_to_title(channels), "Analysis.md")

# Create a list for each channel with just the channel name parameter
params = lapply(channels, FUN = function(x){list(channel = x)})

# Put into a data frame 
reports <- tibble(output_file, params)

apply(reports, MARGIN = 1, FUN = function(x){
  rmarkdown::render(input = "Project-2.Rmd", 
                    output_format = "github_document", 
                    output_file = x[[1]],
                    params = x[[2]],
                    output_options = list(
                      html_preview = FALSE,
                      toc = TRUE,
                      toc_depth = 2
                    ))
  })
```
