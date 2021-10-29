Project 2
================
Joey Chen and John Williams
10/28/2021

# Purpose

...

# Required Packages

The following packages are required for this project:

  - `tidyverse`: Useful data tools for transforming and visualizing data
  - `corrplot`: Visual exploratory tool on correlation matrix
  - `caret`: Tools to help create predictive models
  - `modelr`: Modeling functions such as rmse

# Links to Analyses

The analysis for [Business articles is available here](BusAnalysis.html)
The analysis for [Entertainment articles is available here](EntertainmentAnalysis.html)
The analysis for [Lifestyle articles is available here](LifestyleAnalysis.html)
The analysis for [Social Media articles is available here](SocmedAnalysis.html)
The analysis for [Tech articles is available here](TechAnalysis.html)
The analysis for [World articles is available here](WorldAnalysis.html)

# Render Code

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
