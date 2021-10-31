Example Using Entertainment Channel Data
================
Joey Chen and John Williams
10/24/2021

  - [Introduction](#introduction)
  - [Data Preparation](#data-preparation)
  - [Exploratory Data Analysis](#exploratory-data-analysis)
      - [Distribution of Response
        Variable](#distribution-of-response-variable)
      - [Log(shares) by Number of Words in the
        Title](#logshares-by-number-of-words-in-the-title)
      - [Log(shares) by Day of Week](#logshares-by-day-of-week)
      - [Summary by Interval](#summary-by-interval)
      - [Log(shares) by Number of
        Keywords](#logshares-by-number-of-keywords)
      - [Log(shares) by Number of Images and
        Videos](#logshares-by-number-of-images-and-videos)
      - [Correlation of Predictors](#correlation-of-predictors)
  - [Model Selection](#model-selection)
      - [Linear Model \#1](#linear-model-1)
      - [Linear Model \#2](#linear-model-2)
      - [Random Forest Model](#random-forest-model)
      - [Boosted Tree Model](#boosted-tree-model)
  - [Model Evaluation](#model-evaluation)

# Introduction

This project is a simple walk-through of these steps of the data science
process:

> Data Preparation –\> Exploratory Data Analysis –\> Model Selection –\>
> Model Evaluation

We’ll be using the `OnlineNewsPopularity` dataset from the [UC Irvine
Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/).
It includes continuous and categorical variables about articles
published by Mashable (www.mashable.com) over a period of two years. Our
goal is to predict the popularity of articles in social networks (number
of `shares`) using 58 predictive variables.

The 58 predictive variables can be grouped together by the similar
attributes each measures. There are integer variables that measure the
number of words, keywords, hyperlinks, images, and videos within the
article itself and its title; binary variables that categorize on what
day of the week the article was published; variables that measure the
rate of positive/negative words in the content; and variables measuring
polarity, subjectivity, and sentiment of the article or title. For more
information on the dataset and detailed descriptions of the predictive
variables, click
[here](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity).

After completing EDA, we will describe and demonstrate building 4
predictive models: 2 linear regression models and 2 ensemble models
(Random Forest and Boosted Tree). To build and evaluate these models, we
will split the data into a “training” set and “testing” set (70% and
30%, respectively). Finally, we will select the model that has the
lowest root mean square error (RMSE) as the “best fit”.

# Data Preparation

Here, we read in the data, filtering observations based on channel and
removing non-predictive variables:

``` r
# read data
news_data <- read_csv("./OnlineNewsPopularity.csv")

# data cleaning
channel_data <- news_data %>% 
                   # automation to subset data by data channel
                   filter(eval(parse(text = paste0("data_channel_is_", 
                                                   params[[1]]))) == 1) %>%
                   # remove data channel variables
                   select(-starts_with("data_channel_is")) %>%
                   # remove non-predictive variables
                   select(-url, -timedelta)
```

Next, to facilitate evaluation of models, we partition the the data into
a Training Set and Test Set:

``` r
set.seed(31415)

partition <- createDataPartition(y = channel_data$shares, 
                                 p= 0.7, 
                                 list = FALSE)
channel_train <- channel_data[partition,]
channel_test <- channel_data[-partition,]
```

# Exploratory Data Analysis

## Distribution of Response Variable

First, we can take a look at the distribution of our response variable
`shares`. We can first look at the histogram.

``` r
# Histogram of shares
ggplot(channel_data, aes(x=shares)) +
  geom_histogram(bins=50, fill="navy", col="darkgreen") +
  labs(title="Histogram of shares")
```

![](EntertainmentAnalysis_files/figure-gfm/Joey%20EDA%20shares-1.png)<!-- -->

From the histogram We can see that the distribution is heavily right
skewed. The histogram scale may be influenced by potential outliers, so
we can look at the distribution of `shares` under 5,000.

``` r
# Histogram of shares < 5000
ggplot(filter(channel_data, shares < 5000), aes(x=shares)) +
  geom_histogram(bins=50, fill="navy", col="darkgreen") +
  labs(title="Histogram of shares under 5000")
```

![](EntertainmentAnalysis_files/figure-gfm/Joey%20EDA%20shares%20le%205000-1.png)<!-- -->

As we can see, the distribution is still heavily right skewed. So we may
want to look at the distribution of `log(shares)`.

``` r
# Histogram of log(shares) 
ggplot(channel_data, aes(x=log(shares))) +
  geom_histogram(bins=50, fill="navy", col="darkgreen") +
  labs(title="Histogram of log(shares)")
```

![](EntertainmentAnalysis_files/figure-gfm/Joey%20EDA%20log%20shares-1.png)<!-- -->

From the histogram we can see that the distribution is slightly right
skewed. But it is much closer to normal compared to the original
distribution.

We can also look at the numeric summary of `shares` vs `log(shares)`.

``` r
# Numeric summary of shares
shares_summary <- channel_data %>% summarise(Min. = min(shares),
                                             Q1 = quantile(shares, 0.25),
                                             Median = median(shares),
                                             Mean = mean(shares),
                                             SD = sd(shares),
                                             Q3 = quantile(shares, 0.75),
                                             Max = max(shares),
                                             CV = scales::percent(SD / Mean))

knitr::kable(shares_summary, digits=0, caption = "Numeric Summary of Shares")
```

| Min. |  Q1 | Median | Mean |   SD |   Q3 |    Max | CV   |
| ---: | --: | -----: | ---: | ---: | ---: | -----: | :--- |
|   47 | 833 |   1200 | 2970 | 7858 | 2100 | 210300 | 265% |

Numeric Summary of Shares

``` r
# Numeric summary of log(shares)
log_shares_summary <- channel_data %>% summarise(Min. = min(log(shares)),
                                                 Q1 = quantile(log(shares), 0.25),
                                                 Median = median(log(shares)),
                                                 Mean = mean(log(shares)),
                                                 SD = sd(log(shares)),
                                                 Q3 = quantile(log(shares), 0.75),
                                                 Max = max(log(shares)),
                                                 CV = scales::percent(SD / Mean))

knitr::kable(log_shares_summary, digits=3, caption="Numeric Summary of log(Shares)")
```

| Min. |    Q1 | Median | Mean |    SD |   Q3 |    Max | CV  |
| ---: | ----: | -----: | ---: | ----: | ---: | -----: | :-- |
| 3.85 | 6.725 |   7.09 | 7.31 | 0.933 | 7.65 | 12.256 | 13% |

Numeric Summary of log(Shares)

In addition to the histograms, we can see that the coefficient of
variation (CV) is much larger in `shares` compared to `log(shares)`.
This can make `shares` harder to predict since the variability is so
high. So we will continue to do EDA using `log(shares)` and will fit the
models on `log(shares)` since that can also help reduce the impact of
some extreme values of `shares`.

## Log(shares) by Number of Words in the Title

We can investigate how the number of words in the title might impact
log(shares). We can start by looking at the distribution of the title
word count. We can see which title word counts are used the most within
this data.

``` r
ggplot(data=channel_data, aes(x=n_tokens_title)) +
  geom_bar(col="darkblue", fill="darkgreen") +
  labs(title="Bar graph of Number of Words in Title",
       x="Number of Words in the Title")
```

![](EntertainmentAnalysis_files/figure-gfm/Joey%20EDA%20Title%20Words%20bar-1.png)<!-- -->

Here is the numerical summary. We can see which title word count has the
highest mean or median log(shares).

``` r
words_title_summary <- channel_data %>% 
  group_by("Title Word Count" = n_tokens_title) %>% 
  summarise(n=length(log(shares)),
            Min. = min(log(shares)),
            Q1 = quantile(log(shares), 0.25),
            Median = median(log(shares)),
            Mean = mean(log(shares)),
            SD = sd(log(shares)),
            Q3 = quantile(log(shares), 0.75),
            Max = max(log(shares)))

knitr::kable(words_title_summary, digit=2, caption="Summary Log(shares) by Number of Words in the Title")
```

| Title Word Count |    n | Min. |   Q1 | Median | Mean |   SD |   Q3 |   Max |
| ---------------: | ---: | ---: | ---: | -----: | ---: | ---: | ---: | ----: |
|                2 |    1 | 7.24 | 7.24 |   7.24 | 7.24 |   NA | 7.24 |  7.24 |
|                3 |    1 | 7.00 | 7.00 |   7.00 | 7.00 |   NA | 7.00 |  7.00 |
|                4 |    5 | 6.45 | 7.00 |   7.00 | 7.02 | 0.35 | 7.31 |  7.31 |
|                5 |   17 | 6.32 | 6.91 |   7.44 | 7.63 | 0.99 | 8.48 |  9.31 |
|                6 |   65 | 4.69 | 6.64 |   6.91 | 7.12 | 0.78 | 7.44 |  9.33 |
|                7 |  243 | 4.97 | 6.86 |   7.17 | 7.45 | 0.97 | 7.82 | 11.13 |
|                8 |  476 | 3.85 | 6.71 |   7.09 | 7.28 | 0.93 | 7.65 | 11.03 |
|                9 |  837 | 3.89 | 6.71 |   7.00 | 7.24 | 0.87 | 7.55 | 11.84 |
|               10 | 1216 | 4.41 | 6.69 |   7.00 | 7.28 | 0.92 | 7.60 | 12.17 |
|               11 | 1353 | 4.48 | 6.73 |   7.09 | 7.35 | 0.98 | 7.74 | 12.26 |
|               12 | 1143 | 4.04 | 6.74 |   7.09 | 7.31 | 0.91 | 7.70 | 12.19 |
|               13 |  904 | 3.89 | 6.72 |   7.00 | 7.33 | 1.00 | 7.65 | 11.63 |
|               14 |  489 | 4.06 | 6.76 |   7.17 | 7.35 | 0.92 | 7.74 | 11.18 |
|               15 |  200 | 6.05 | 6.74 |   7.00 | 7.24 | 0.84 | 7.44 | 10.22 |
|               16 |   81 | 6.21 | 6.74 |   7.09 | 7.24 | 0.65 | 7.65 |  9.15 |
|               17 |   22 | 5.49 | 6.73 |   6.90 | 7.26 | 1.01 | 7.67 |  9.66 |
|               18 |    4 | 6.20 | 6.23 |   6.42 | 6.51 | 0.38 | 6.70 |  7.00 |

Summary Log(shares) by Number of Words in the Title

We can also assess the relationship between log(shares) and title word
count by looking at the boxplot. The red dot represents the mean. Higher
red dot means the articles are shared more on average, for that number
of words in title. However, the mean can be heavily influenced by
outliers.

``` r
ggplot(channel_data, aes(x = as.factor(n_tokens_title), y = log(shares))) +
  geom_boxplot(aes(fill=as.factor(n_tokens_title))) +
  stat_summary(fun.y="mean", col="red") +
  theme(legend.position = "none") +
  labs(title="Boxplot of Log(shares) by Number of Words in title", 
       x = "Number of Words in Title")
```

![](EntertainmentAnalysis_files/figure-gfm/Joey%20EDA%20Title%20Words%20Boxplot-1.png)<!-- -->

## Log(shares) by Day of Week

Let’s create a categorical variable `day_published` and determine if the
mean/median number of shares changes depending on what day the article
was published:

``` r
channel_data2 <- channel_data %>%
                 pivot_longer(starts_with("weekday"), 
                              names_to = "day_published",
                              names_prefix = "weekday_is_") %>%
                 filter(value == 1) %>%
                 select(-value)

channel_data2$day_published <- factor(channel_data2$day_published,
                                      levels=c("monday", "tuesday", "wednesday",
                                               "thursday", "friday", "saturday",
                                               "sunday"))

ggplot(channel_data2, aes(x = day_published, y = log(shares))) +
  geom_boxplot(aes(fill = day_published)) +
  stat_summary(fun = "mean") +
  theme(legend.position = "none") +
  labs(x = "Day of Publication", 
       title = "Log(Shares) by Day of Publication")
```

![](EntertainmentAnalysis_files/figure-gfm/John%20EDA1-1.png)<!-- -->

After viewing the boxplot above, you should be able to determine what
day (or days) have the highest mean/median shares compared to other
days. The median is represented by the bold horizontal black line ithin
the colored box; the mean is represented by the black dot within the
colored box.

We can further see evidence of this by examining the categorical
variable `is_weekend`:

``` r
ggplot(channel_data, aes(x = as.factor(is_weekend), y = log(shares))) +
  geom_boxplot(aes(fill = as.factor(is_weekend))) +
  theme(legend.position = "none") +
  labs(x = "Published on Weekend?  0 = No, 1 = Yes", 
       title = "Log(Shares) by Day of Publication")
```

![](EntertainmentAnalysis_files/figure-gfm/John%20EDA2-1.png)<!-- -->

## Summary by Interval

Let’s examine the relationship between a continuous variable that is
within the range \[0, 1\] and `log(shares)`. One way we can do this by
“cutting” the variable into 11 subintervals ((-0.5, 0.5\], (0.5,
1.5\], (1.5, 2.5\], etc.) and calculating the mean/median `log(shares)`
for each subinterval. If the mean/median of `log(shares)` steadily
increases as the predictor increases, then there is a positive
relationship; if the mean/median of `log(shares)` steadily decreases as
the predictor increases, then there is a negative relationship. If there
is no clear pattern in the mean/median of `log(shares)` as the predictor
increases, then we cannot make any statement about the linear
relationship of that predictor and the response. For example,
`title_subjectivity` has a range \[0, 1\]. Let’s see how the mean/median
of `log(shares)` changes as `title_subjectivity` increases:

``` r
tab <- channel_data %>%
       mutate(title_subjectivity = cut(title_subjectivity, 
                                       seq(-0.05, 1.05, by = 0.1))) %>%
       group_by(title_subjectivity) %>%
       summarise(mean = mean(log(shares)), 
                 median = median(log(shares)), 
                 n = n())

knitr::kable(tab, 
             digits = 3,
             format.args = list(big.mark = ",", scientific = FALSE),
             col.names = c('Title Subjectivity', 'Mean', 'Median', 'Count'),
             caption = "Summary of Title Subjectivity")
```

| Title Subjectivity |  Mean | Median | Count |
| :----------------- | ----: | -----: | ----: |
| (-0.05,0.05\]      | 7.280 |  7.003 | 2,874 |
| (0.05,0.15\]       | 7.300 |  7.090 |   238 |
| (0.15,0.25\]       | 7.366 |  7.170 |   310 |
| (0.25,0.35\]       | 7.318 |  7.090 |   469 |
| (0.35,0.45\]       | 7.387 |  7.090 |   655 |
| (0.45,0.55\]       | 7.217 |  7.003 |   904 |
| (0.55,0.65\]       | 7.374 |  7.090 |   408 |
| (0.65,0.75\]       | 7.418 |  7.090 |   429 |
| (0.75,0.85\]       | 7.306 |  7.003 |   140 |
| (0.85,0.95\]       | 7.297 |  7.090 |   141 |
| (0.95,1.05\]       | 7.370 |  7.090 |   489 |

Summary of Title Subjectivity

Similarly, we can examine `rate_negative_words`:

``` r
tab <- channel_data %>%
       mutate(rate_negative_words = cut(rate_negative_words, 
                                        seq(-0.05, 1.05, by = 0.1))) %>%
       group_by(rate_negative_words) %>%
       summarise(mean = mean(log(shares)), 
                 median = median(log(shares)), 
                 n = n())

knitr::kable(tab, 
             digits = 3,
             format.args = list(big.mark = ",", scientific = FALSE),
             col.names = c('Rate Negative Words', 'Mean', 'Median', 'Count'),
             caption = "Summary of Rate Negative Words")
```

| Rate Negative Words |  Mean | Median | Count |
| :------------------ | ----: | -----: | ----: |
| (-0.05,0.05\]       | 7.314 |  7.090 |   406 |
| (0.05,0.15\]        | 7.371 |  7.090 |   651 |
| (0.15,0.25\]        | 7.345 |  7.090 | 1,594 |
| (0.25,0.35\]        | 7.300 |  7.090 | 1,783 |
| (0.35,0.45\]        | 7.278 |  7.003 | 1,467 |
| (0.45,0.55\]        | 7.256 |  7.003 |   778 |
| (0.55,0.65\]        | 7.309 |  7.003 |   257 |
| (0.65,0.75\]        | 7.314 |  7.003 |    72 |
| (0.75,0.85\]        | 7.416 |  7.170 |    26 |
| (0.85,0.95\]        | 7.695 |  7.242 |    16 |
| (0.95,1.05\]        | 7.386 |  7.244 |     7 |

Summary of Rate Negative Words

And `avg_positive_polarity`:

``` r
tab <- channel_data %>%
       mutate(avg_positive_polarity = cut(avg_positive_polarity, 
                                          seq(0, 1, by = 0.1), right = F)) %>%
       group_by(avg_positive_polarity) %>%
       summarise(mean = mean(log(shares)), 
                 median = median(log(shares)), 
                 n = n())

knitr::kable(tab, 
             digits = 3,
             format.args = list(big.mark = ",", scientific = FALSE),
             col.names = c('Avg Positive Polarity', 'Mean', 'Median', 'Count'),
             caption = "Summary of Average Positive Polarity")
```

| Avg Positive Polarity |  Mean | Median | Count |
| :-------------------- | ----: | -----: | ----: |
| \[0,0.1)              | 7.375 |  7.090 |   213 |
| \[0.1,0.2)            | 7.245 |  7.003 |   107 |
| \[0.2,0.3)            | 7.278 |  7.090 |   968 |
| \[0.3,0.4)            | 7.285 |  7.090 | 3,277 |
| \[0.4,0.5)            | 7.339 |  7.090 | 2,020 |
| \[0.5,0.6)            | 7.378 |  7.090 |   380 |
| \[0.6,0.7)            | 7.589 |  7.244 |    71 |
| \[0.7,0.8)            | 7.538 |  7.376 |    16 |
| \[0.8,0.9)            | 7.152 |  6.913 |     4 |
| NA                    | 6.708 |  6.708 |     1 |

Summary of Average Positive Polarity

## Log(shares) by Number of Keywords

Is the number of keywords influential in determining the number of
shares? Let’s transform `num_keywords` into a categorical variable and
create a boxplot. The boxplot will show us if any particular number of
keywords is related to higher number of shares.

``` r
ggplot(channel_data, aes(x = as.factor(num_keywords), y = log(shares))) +
  geom_boxplot(aes(fill=as.factor(num_keywords))) +
  stat_summary(fun.y = "mean") +
  theme(legend.position = "none") +
  labs(x = "Number of Keywords", 
       title = "Log(Shares) by Number of Keywords")
```

![](EntertainmentAnalysis_files/figure-gfm/John%20EDA6-1.png)<!-- -->

## Log(shares) by Number of Images and Videos

We can examine the impact of the number of videos and images on
`log(shares)`. We can do this by looking at the scatterplots. If the
points show an upward trend, then articles with more images or videos
would be shared more often. If we see a negative trend then articles
with more images or videos would be shared less often.

``` r
# Scatterplot of num_img and log(shares)
ggplot(channel_data, aes(x = num_imgs, y = log(shares))) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title="Scatterplot of Number of Images vs log(shares)",
       x="Number of Images")
```

![](EntertainmentAnalysis_files/figure-gfm/Joey%20EDA%20img%20video-1.png)<!-- -->

``` r
# Scatterplot of num_videos and log(shares)
ggplot(channel_data, aes(x = num_videos, y = log(shares))) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title="Scatterplot of Number of Videos vs log(shares)",
       x="Number of Videos")
```

![](EntertainmentAnalysis_files/figure-gfm/Joey%20EDA%20img%20video-2.png)<!-- -->

We can also examine the correlation coefficient (r) to observe the
strength of the association. For absolute values of r, we can considered
0-0.19 as very weak, 0.2-0.39 as weak, 0.4-0.59 as moderate, 0.6-0.79 as
strong and 0.8-1 as very strong correlation.

``` r
# Correlation Coefficient (r) of num_imgs vs log(shares) 
cor(x = channel_data$num_imgs, y = log(channel_data$shares))
```

    ## [1] 0.05089658

``` r
# Correlation Coefficient (r) of num_videos vs log(shares) 
cor(x = channel_data$num_videos, y = log(channel_data$shares))
```

    ## [1] 0.008311073

## Correlation of Predictors

Now, before creating our prediction models, we need to examine the
correlation among predictors, as we would not want to include predictors
in the model that are highly correlated. Since we have a large number of
predictors, let’s attempt to subset the predictors based on measuring
similar attributes.

``` r
# Create a correlogram for predictors that measure attributes of "tokens" 
# and "words"
cor1 <- channel_data %>% select(contains("tokens"), contains("words"))
cormatrix <- round(cor(cor1), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](EntertainmentAnalysis_files/figure-gfm/John%20EDA7-1.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of keywords 
cor2 <- channel_data %>% select(contains("keywords"), contains("kw"))
cormatrix <- round(cor(cor2), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](EntertainmentAnalysis_files/figure-gfm/John%20EDA7-2.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of "positve" and
# "negative" connotation 
cor3 <- channel_data %>% select(contains("positive"), contains("negative"))
cormatrix <- round(cor(cor3), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](EntertainmentAnalysis_files/figure-gfm/John%20EDA7-3.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of article titles
cor4 <- channel_data %>% select(contains("title"))
cormatrix <- round(cor(cor4), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](EntertainmentAnalysis_files/figure-gfm/John%20EDA7-4.png)<!-- -->

# Model Selection

Linear regression models a response variable as a linear combination of
input predictor variables. The models are often fitted using the least
squares approach, which is a method that finds the best fit by
minimizing the sum of the squares of the residuals of the points from
the curve. When we fit the model on the training data we would get
coefficients corresponding to each of the predictor variables, and these
coefficients can be used to make predictions on the response with
predictor variables from the test data.

## Linear Model \#1

The predictors in this model are selected by finding the statistically
significant variables when using the world news data channel. First, all
52 predictors are fit against `log(shares)` to test for the main effect,
and 19 predictors had significant main effect and these variables are
kept. Next, these 19 variables as well as their squares are fit to test
for quadratic effect, with 7 quadratic predictors being significant and
kept. Lastly, 19 main effect predictors and all their first order
interactions are fit to test for interaction effect, with 3 significant
interaction effects kept. The result is a model with 19 main effect
predictors, 7 quadratic predictors, and 3 interaction predictors.

``` r
lm1Fit <- lm(log(shares) ~ n_non_stop_words + 
                     num_hrefs +
                     num_imgs + 
                     num_videos +
                     average_token_length + 
                     kw_min_min +
                     kw_min_avg +
                     kw_max_avg +
                     kw_avg_avg +
                     self_reference_avg_sharess +
                     weekday_is_monday +
                     weekday_is_tuesday +
                     weekday_is_wednesday +
                     weekday_is_thursday +
                     weekday_is_friday +
                     LDA_00 +
                     LDA_02 +
                     global_subjectivity +
                     title_sentiment_polarity +
                     I(n_tokens_title^2) +
                     I(num_videos^2) +
                     I(average_token_length^2) + 
                     I(kw_min_avg^2) +
                     I(kw_avg_avg^2) +
                     I(self_reference_avg_sharess^2) +
                     I(title_sentiment_polarity^2) +
                     num_hrefs:num_imgs +
                     kw_min_min:weekday_is_tuesday +
                     n_tokens_title:num_imgs,
            data = channel_train)

summary(lm1Fit)
```

    ## 
    ## Call:
    ## lm(formula = log(shares) ~ n_non_stop_words + num_hrefs + num_imgs + 
    ##     num_videos + average_token_length + kw_min_min + kw_min_avg + 
    ##     kw_max_avg + kw_avg_avg + self_reference_avg_sharess + weekday_is_monday + 
    ##     weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + 
    ##     weekday_is_friday + LDA_00 + LDA_02 + global_subjectivity + 
    ##     title_sentiment_polarity + I(n_tokens_title^2) + I(num_videos^2) + 
    ##     I(average_token_length^2) + I(kw_min_avg^2) + I(kw_avg_avg^2) + 
    ##     I(self_reference_avg_sharess^2) + I(title_sentiment_polarity^2) + 
    ##     num_hrefs:num_imgs + kw_min_min:weekday_is_tuesday + n_tokens_title:num_imgs, 
    ##     data = channel_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8132 -0.5622 -0.2015  0.3329  5.2367 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      6.798e+00  1.248e-01  54.456  < 2e-16 ***
    ## n_non_stop_words                 1.664e-03  8.597e-04   1.935 0.053030 .  
    ## num_hrefs                        2.601e-03  1.439e-03   1.807 0.070758 .  
    ## num_imgs                         3.995e-03  6.011e-03   0.665 0.506343    
    ## num_videos                       7.908e-04  3.989e-03   0.198 0.842845    
    ## average_token_length            -2.100e-01  6.690e-02  -3.140 0.001701 ** 
    ## kw_min_min                       1.144e-03  2.348e-04   4.873 1.14e-06 ***
    ## kw_min_avg                      -2.344e-04  3.913e-05  -5.990 2.25e-09 ***
    ## kw_max_avg                      -3.998e-05  6.740e-06  -5.932 3.20e-09 ***
    ## kw_avg_avg                       3.256e-04  3.321e-05   9.802  < 2e-16 ***
    ## self_reference_avg_sharess       1.707e-05  2.639e-06   6.470 1.08e-10 ***
    ## weekday_is_monday               -3.764e-01  4.551e-02  -8.271  < 2e-16 ***
    ## weekday_is_tuesday              -4.243e-01  4.716e-02  -8.997  < 2e-16 ***
    ## weekday_is_wednesday            -4.285e-01  4.590e-02  -9.334  < 2e-16 ***
    ## weekday_is_thursday             -4.122e-01  4.633e-02  -8.898  < 2e-16 ***
    ## weekday_is_friday               -3.260e-01  4.900e-02  -6.653 3.18e-11 ***
    ## LDA_00                          -9.707e-02  1.371e-01  -0.708 0.478894    
    ## LDA_02                          -4.819e-02  1.003e-01  -0.481 0.630842    
    ## global_subjectivity              6.173e-01  1.588e-01   3.888 0.000102 ***
    ## title_sentiment_polarity         6.206e-02  4.756e-02   1.305 0.192033    
    ## I(n_tokens_title^2)              5.470e-05  3.156e-04   0.173 0.862411    
    ## I(num_videos^2)                 -1.454e-04  1.008e-04  -1.443 0.148971    
    ## I(average_token_length^2)        3.255e-02  1.151e-02   2.827 0.004721 ** 
    ## I(kw_min_avg^2)                  5.581e-08  1.412e-08   3.953 7.84e-05 ***
    ## I(kw_avg_avg^2)                  3.337e-09  2.414e-09   1.383 0.166825    
    ## I(self_reference_avg_sharess^2) -8.387e-11  3.016e-11  -2.781 0.005440 ** 
    ## I(title_sentiment_polarity^2)    9.178e-02  7.524e-02   1.220 0.222571    
    ## num_hrefs:num_imgs              -3.806e-05  5.494e-05  -0.693 0.488511    
    ## kw_min_min:weekday_is_tuesday   -5.946e-04  5.306e-04  -1.120 0.262570    
    ## num_imgs:n_tokens_title         -1.750e-04  5.277e-04  -0.332 0.740120    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8882 on 4911 degrees of freedom
    ## Multiple R-squared:   0.11,  Adjusted R-squared:  0.1048 
    ## F-statistic: 20.94 on 29 and 4911 DF,  p-value: < 2.2e-16

## Linear Model \#2

We built this model using our best judgment given what we knew about the
data. Using all 52 of the predictors (and their interactions) would not
be manageable or parsimonious. We chose a limited number of predictors
using EDA and common sense in order to limit colinearity and maintain
simplicity of the model.

First, we selected variables that we believed would have be the “best”
predictors of the number of shares. Recall that many of the predictors
could be grouped into categories based on the common attributes each
measured. We made an effort to pick at least one predictor from each
category. For example, of the 8 variables that measure day of
publication, we chose only `is_weekend` to include in our linear model;
or, of the 10 variables that measure keyword attributes, we chose only
`num_keywords` to include in our linear model. At the end of this step,
we landed on using 13 predictors. We decided not to include higher order
predictors to reduce complexity.

Next, we fit a full model using all 13 of our selected predictors with
all their associated interactions. This produced a very high dimensional
model. To quickly obtain parsimony, we “unscientifically” selected to
include into a reduced model our 13 predictors and only the interaction
terms that were highly significant within the full model. There were
only 7 of these interaction terms. This reduced model is what is used in
the code below for building Linear Model \#2.

``` r
# Remove variables with possible colinearity
reduced_world_news_data <- channel_data %>%
                           select(n_tokens_title,
                                  n_tokens_content,
                                  num_hrefs,
                                  num_imgs,
                                  num_videos,
                                  num_keywords,
                                  average_token_length,
                                  is_weekend,
                                  global_subjectivity,
                                  global_sentiment_polarity,
                                  title_sentiment_polarity,
                                  self_reference_avg_sharess,
                                  rate_positive_words,
                                  shares)

lm2Fit <- lm(log(shares) ~ . + n_tokens_title:average_token_length +
                               n_tokens_title:global_sentiment_polarity +
                               n_tokens_title:rate_positive_words +
                               n_tokens_content:num_imgs +
                               num_imgs:num_keywords +
                               num_imgs:global_subjectivity +
                               num_imgs:global_sentiment_polarity, 
             data = reduced_world_news_data)

summary(lm2Fit)
```

    ## 
    ## Call:
    ## lm(formula = log(shares) ~ . + n_tokens_title:average_token_length + 
    ##     n_tokens_title:global_sentiment_polarity + n_tokens_title:rate_positive_words + 
    ##     n_tokens_content:num_imgs + num_imgs:num_keywords + num_imgs:global_subjectivity + 
    ##     num_imgs:global_sentiment_polarity, data = reduced_world_news_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7676 -0.5643 -0.2295  0.3249  5.2992 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                               7.353e+00  3.715e-01  19.793  < 2e-16 ***
    ## n_tokens_title                           -2.816e-02  3.142e-02  -0.896  0.37005    
    ## n_tokens_content                         -9.258e-05  2.860e-05  -3.238  0.00121 ** 
    ## num_hrefs                                 4.689e-03  9.530e-04   4.920 8.84e-07 ***
    ## num_imgs                                  1.993e-03  5.748e-03   0.347  0.72887    
    ## num_videos                                1.467e-03  1.885e-03   0.778  0.43662    
    ## num_keywords                              2.604e-02  6.626e-03   3.930 8.59e-05 ***
    ## average_token_length                     -1.761e-02  1.027e-01  -0.172  0.86382    
    ## is_weekend                                3.858e-01  3.247e-02  11.882  < 2e-16 ***
    ## global_subjectivity                       6.077e-01  1.385e-01   4.389 1.15e-05 ***
    ## global_sentiment_polarity                 1.498e+00  9.214e-01   1.626  0.10405    
    ## title_sentiment_polarity                  6.140e-02  4.021e-02   1.527  0.12677    
    ## self_reference_avg_sharess                8.951e-06  9.550e-07   9.373  < 2e-16 ***
    ## rate_positive_words                      -1.111e+00  6.028e-01  -1.843  0.06530 .  
    ## n_tokens_title:average_token_length      -5.635e-03  9.060e-03  -0.622  0.53395    
    ## n_tokens_title:global_sentiment_polarity -1.311e-01  8.386e-02  -1.563  0.11800    
    ## n_tokens_title:rate_positive_words        1.061e-01  5.461e-02   1.943  0.05208 .  
    ## n_tokens_content:num_imgs                -5.924e-08  9.023e-07  -0.066  0.94765    
    ## num_imgs:num_keywords                     2.219e-04  4.840e-04   0.459  0.64654    
    ## num_imgs:global_subjectivity              3.730e-03  8.783e-03   0.425  0.67108    
    ## num_imgs:global_sentiment_polarity       -8.119e-03  7.625e-03  -1.065  0.28700    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.91 on 7036 degrees of freedom
    ## Multiple R-squared:  0.05108,    Adjusted R-squared:  0.04838 
    ## F-statistic: 18.94 on 20 and 7036 DF,  p-value: < 2.2e-16

## Random Forest Model

The random forest model is a classification algorithm that consists of
many decision trees. It creates multiple (e.g. 500) bootstrapped samples
and fit decision trees to each of the bootstrapped samples. When
selecting a split point, the learning algorithm selects a random sample
of predictors of which to search, instead of all the predictors. By not
looking at all the predictors every time, it prevents one or two strong
predictors to dominate the tree fits. The prediction of trees are then
averaged (for regression) to get the final predicted value. This would
lead to less variance and better fit over an individual tree fit.

Regarding the number of predictors to look for, a good rule of thumb is
m = sqrt(p) for classification and m=p/3 for regression, where m is the
number of randomly selected features at each point, and p is the number
of input variables. In our dataset there are 52 predictors so we will be
using m = 17.

``` r
# Fit the random forest Model
rfFit <- train(log(shares) ~ .,
               data = channel_train,
               method = "rf",
               trControl = trainControl(method = "cv", number = 5),
               preProcess = c("center", "scale"),
               tuneGrid = data.frame(mtry = 1:17))
```

## Boosted Tree Model

Just like the random forest model, the boosted tree model (sometimes
called gradient boosted tree) combines outputs from individual decision
trees to create a prediction model. It differs from the random forest
model by combining weak learners sequentially so that each new tree
corrects errors of the previous one. In this case, weak learners are
decision trees that perform relatively poorly, not much better than
random guessing. Sequentially combining these poor performing models to
create a better fitting model is called boosting. It’s analogous to the
proverb “None of us is as smart as all of us.”

The first step in creating a boosted tree model is fitting a single
decision tree with \(d\) splits to the data. We evaluate this fit using
a loss function, a method of measuring prediction error. There are many
different loss functions and its selection is arbitrary. Step 2 is to
add a second decision tree (also with \(d\) splits) to the first such
that it lowers the loss compared to the first tree alone:

\[Boosted Ensemble = First Tree + \lambda * Second Tree\]

\[Loss(Boosted Ensemble) < Loss(First Tree)\]

Here, \(\lambda\) is a shrinkage parameter which slows the fitting
process. It’s sometimes called the learning rate. We repeat the second
step \(B\) times to finish building the model. The tuning parameters
\(\lambda\), \(d\), and \(B\) can be chosen using cross validation.

In the R code below which uses the `caret` package to build a boosted
tree model, `n.trees` is \(B\), `interaction.depth` is \(d\), and
`shrinkage` is \(\lambda\). The additional tuning parameter
`n.minobsinnode` allows for controlling the minimum number of
observations within each tree node.

``` r
control <- trainControl(method = "cv", number = 5)
boostedFit <- train(log(shares) ~ ., data = channel_train, 
                    method = "gbm",
                    trControl = control,
                    preProcess = c("center", "scale"),
                    tuneGrid = expand.grid(n.trees = c(300, 350, 400, 450, 500),
                                           interaction.depth = c(14:24),
                                           shrinkage = c(0.01),
                                           n.minobsinnode = 10),
                    verbose = FALSE)
```

# Model Evaluation

We can compare the RMSE, Rsquared, and MAE of the four model fits on the
test data. We will be choosing the model with the lowest RMSE.

``` r
prediction <- predict(lm1Fit, newdata = channel_test)
lm1 <- round(postResample(prediction, log(channel_test$shares)), 4)

prediction <- predict(lm2Fit, newdata = channel_test)
lm2 <- round(postResample(prediction, log(channel_test$shares)), 4)

prediction <- predict(rfFit, newdata = channel_test)
rf <- round(postResample(prediction, log(channel_test$shares)), 4)

prediction <- predict(boostedFit, newdata = channel_test)
boost <- round(postResample(prediction, log(channel_test$shares)), 4)

compareFits <- data.frame(lm1, lm2, rf, boost)
names(compareFits) <- c("Linear Model 1", 
                        "Linear Model 2",
                        "Random Forest Model",
                        "Boosted Tree Model")

compareFitsLong <- data.frame(t(compareFits))

# Sort the table of models by ascending RMSE and pick the first row as the model
finalModel <- compareFitsLong %>% arrange(RMSE) %>% filter(row_number()==1) 

knitr::kable(compareFitsLong)
```

|                     |   RMSE | Rsquared |    MAE |
| :------------------ | -----: | -------: | -----: |
| Linear Model 1      | 0.9248 |   0.0420 | 0.6535 |
| Linear Model 2      | 0.9033 |   0.0367 | 0.6615 |
| Random Forest Model | 0.8872 |   0.0712 | 0.6501 |
| Boosted Tree Model  | 0.8851 |   0.0736 | 0.6423 |

Our final model is the Boosted Tree Model. When we fit the model on the
test data we get RMSE=0.8851, Rsquared=0.0736, and MAE=0.6423.
