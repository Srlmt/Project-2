Project 2
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
      - [Our final model is the Random Forest Model. When we fit the
        model on the test data we get RMSE=0.9291, Rsquared=0.0543, and
        MAE=0.7123.](#our-final-model-is-the-random-forest-model.-when-we-fit-the-model-on-the-test-data-we-get-rmse0.9291-rsquared0.0543-and-mae0.7123.)

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

``` r
# read data
news_data <- read_csv("./OnlineNewsPopularity.csv")

# data cleaning
world_news_data <- news_data %>% 
                   # automation to subset data by data channel
                   filter(eval(parse(text = paste0("data_channel_is_", 
                                                   params[[1]]))) == 1) %>%
                   # remove data channel variables
                   select(-starts_with("data_channel_is")) %>%
                   # remove non-predictive variables
                   select(-url, -timedelta)
```

# Exploratory Data Analysis

## Distribution of Response Variable

First, we can take a look at the distribution of our response variable
`shares`. We can first look at the histogram.

``` r
# Histogram of shares
ggplot(world_news_data, aes(x=shares)) +
  geom_histogram(bins=50, fill="navy", col="darkgreen") +
  labs(title="Histogram of shares")
```

![](EntertainmentAnalysis_files/figure-gfm/Joey%20EDA%20shares-1.png)<!-- -->

From the histogram We can see that the distribution is heavily right
skewed. The histogram scale may be influenced by potential outliers, so
we can look at the distribution of `shares` under 5,000.

``` r
# Histogram of shares < 5000
ggplot(filter(world_news_data, shares < 5000), aes(x=shares)) +
  geom_histogram(bins=50, fill="navy", col="darkgreen") +
  labs(title="Histogram of shares under 5000")
```

![](EntertainmentAnalysis_files/figure-gfm/Joey%20EDA%20shares%20le%205000-1.png)<!-- -->

As we can see, the distribution is still heavily right skewed. So we may
want to look at the distribution of `log(shares)`.

``` r
# Histogram of log(shares) 
ggplot(world_news_data, aes(x=log(shares))) +
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
shares_summary <- world_news_data %>% summarise(Min. = min(shares),
                                                Q1 = quantile(shares, 0.25),
                                                Median = median(shares),
                                                Mean = mean(shares),
                                                SD = sd(shares),
                                                Q3 = quantile(shares, 0.75),
                                                Max = max(shares),
                                                CV = scales::percent(SD / Mean))

knitr::kable(shares_summary, digits=0, caption = "Numeric Summary of Shares")
```

| Min. |   Q1 | Median | Mean |   SD |   Q3 |    Max | CV   |
| ---: | ---: | -----: | ---: | ---: | ---: | -----: | :--- |
|   28 | 1100 |   1700 | 3682 | 8885 | 3250 | 208300 | 241% |

Numeric Summary of Shares

``` r
# Numeric summary of log(shares)
log_shares_summary <- world_news_data %>% summarise(Min. = min(log(shares)),
                                                Q1 = quantile(log(shares), 0.25),
                                                Median = median(log(shares)),
                                                Mean = mean(log(shares)),
                                                SD = sd(log(shares)),
                                                Q3 = quantile(log(shares), 0.75),
                                                Max = max(log(shares)),
                                                CV = scales::percent(SD / Mean))

knitr::kable(log_shares_summary, digits=3, caption="Numeric Summary of log(Shares)")
```

|  Min. |    Q1 | Median |  Mean |    SD |    Q3 |    Max | CV  |
| ----: | ----: | -----: | ----: | ----: | ----: | -----: | :-- |
| 3.332 | 7.003 |  7.438 | 7.606 | 0.943 | 8.086 | 12.247 | 12% |

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
ggplot(data=world_news_data, aes(x=n_tokens_title)) +
  geom_bar(col="darkblue", fill="darkgreen") +
  labs(title="Bar graph of Number of Words in Title",
       x="Number of Words in the Title")
```

![](EntertainmentAnalysis_files/figure-gfm/Joey%20EDA%20Title%20Words%20bar-1.png)<!-- -->

Here is the numerical summary. We can see which title word count has the
highest mean or median log(shares).

``` r
words_title_summary <- world_news_data %>% group_by("Title Word Count" = n_tokens_title) %>% summarise(n=length(log(shares)),
                                                Min. = min(log(shares)),
                                                Q1 = quantile(log(shares), 0.25),
                                                Median = median(log(shares)),
                                                Mean = mean(log(shares)),
                                                SD = sd(log(shares)),
                                                Q3 = quantile(log(shares), 0.75),
                                                Max = max(log(shares)))

knitr::kable(words_title_summary, digit=2, caption="Summary Log(shares) by Number of Words in the Title")
```

| Title Word Count |   n | Min. |   Q1 | Median | Mean |   SD |   Q3 |   Max |
| ---------------: | --: | ---: | ---: | -----: | ---: | ---: | ---: | ----: |
|                3 |   1 | 7.00 | 7.00 |   7.00 | 7.00 |   NA | 7.00 |  7.00 |
|                5 |   7 | 6.41 | 6.89 |   7.44 | 7.58 | 1.08 | 7.88 |  9.67 |
|                6 |  46 | 4.84 | 6.83 |   7.44 | 7.35 | 0.91 | 7.73 |  9.99 |
|                7 | 161 | 5.04 | 7.00 |   7.31 | 7.51 | 0.91 | 8.01 |  9.91 |
|                8 | 351 | 4.53 | 7.00 |   7.44 | 7.58 | 0.97 | 8.01 | 10.93 |
|                9 | 432 | 5.06 | 7.00 |   7.50 | 7.70 | 1.01 | 8.19 | 12.25 |
|               10 | 399 | 5.95 | 7.00 |   7.38 | 7.58 | 0.84 | 8.02 | 10.38 |
|               11 | 335 | 3.33 | 7.00 |   7.44 | 7.62 | 0.96 | 8.16 | 11.30 |
|               12 | 183 | 5.19 | 7.00 |   7.44 | 7.62 | 0.92 | 7.99 | 10.91 |
|               13 | 121 | 4.36 | 7.00 |   7.38 | 7.60 | 1.01 | 8.22 | 10.90 |
|               14 |  42 | 6.10 | 6.93 |   7.49 | 7.69 | 0.97 | 8.33 | 10.18 |
|               15 |  12 | 6.56 | 7.04 |   7.24 | 7.45 | 0.77 | 7.56 |  9.13 |
|               16 |   5 | 6.28 | 6.77 |   7.44 | 7.22 | 0.68 | 7.70 |  7.90 |
|               17 |   3 | 6.46 | 7.00 |   7.55 | 7.34 | 0.79 | 7.78 |  8.01 |
|               18 |   1 | 7.17 | 7.17 |   7.17 | 7.17 |   NA | 7.17 |  7.17 |

Summary Log(shares) by Number of Words in the Title

We can also assess the relationship between log(shares) and title word
count by looking at the boxplot. The red dot represents the mean. Higher
red dot means the articles are shared more on average, for that number
of words in title. However, the mean can be heavily influenced by
outliers.

``` r
ggplot(world_news_data, aes(x = as.factor(n_tokens_title), y = log(shares))) +
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
world_news_data2 <- world_news_data %>%
                    pivot_longer(starts_with("weekday"), 
                                 names_to = "day_published",
                                 names_prefix = "weekday_is_") %>%
                    filter(value == 1) %>%
                    select(-value)

ggplot(world_news_data2, aes(x = day_published, y = log(shares))) +
  geom_boxplot(aes(fill = day_published)) +
  stat_summary(fun.y = "mean") +
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
ggplot(world_news_data, aes(x = as.factor(is_weekend), y = log(shares))) +
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
tab <- world_news_data %>%
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
| (-0.05,0.05\]      | 7.588 |  7.378 |   997 |
| (0.05,0.15\]       | 7.695 |  7.438 |    82 |
| (0.15,0.25\]       | 7.529 |  7.550 |    69 |
| (0.25,0.35\]       | 7.659 |  7.523 |   130 |
| (0.35,0.45\]       | 7.527 |  7.378 |    92 |
| (0.45,0.55\]       | 7.554 |  7.378 |   268 |
| (0.55,0.65\]       | 7.674 |  7.438 |   119 |
| (0.65,0.75\]       | 7.603 |  7.438 |   118 |
| (0.75,0.85\]       | 7.634 |  7.496 |    45 |
| (0.85,0.95\]       | 8.016 |  7.901 |    41 |
| (0.95,1.05\]       | 7.635 |  7.378 |   138 |

Summary of Title Subjectivity

Similarly, we can examine `rate_negative_words`:

``` r
tab <- world_news_data %>%
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
| (-0.05,0.05\]       | 7.597 |  7.313 |    74 |
| (0.05,0.15\]        | 7.509 |  7.378 |   271 |
| (0.15,0.25\]        | 7.664 |  7.496 |   681 |
| (0.25,0.35\]        | 7.584 |  7.438 |   626 |
| (0.35,0.45\]        | 7.591 |  7.438 |   278 |
| (0.45,0.55\]        | 7.608 |  7.313 |   109 |
| (0.55,0.65\]        | 7.694 |  7.438 |    39 |
| (0.65,0.75\]        | 7.722 |  7.601 |    18 |
| (0.75,0.85\]        | 8.006 |  8.006 |     1 |
| (0.85,0.95\]        | 6.417 |  6.417 |     1 |
| (0.95,1.05\]        | 7.550 |  7.550 |     1 |

Summary of Rate Negative Words

And `avg_positive_polarity`:

``` r
tab <- world_news_data %>%
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
| \[0,0.1)              | 8.343 |  8.243 |    23 |
| \[0.1,0.2)            | 7.879 |  8.039 |    15 |
| \[0.2,0.3)            | 7.546 |  7.313 |   215 |
| \[0.3,0.4)            | 7.576 |  7.378 |   964 |
| \[0.4,0.5)            | 7.631 |  7.438 |   748 |
| \[0.5,0.6)            | 7.641 |  7.496 |   121 |
| \[0.6,0.7)            | 7.366 |  7.242 |    12 |
| \[0.7,0.8)            | 7.378 |  7.378 |     1 |

Summary of Average Positive Polarity

## Log(shares) by Number of Keywords

Is the number of keywords influential in determining the number of
shares? Let’s transform `num_keywords` into a categorical variable and
create a boxplot. The boxplot will show us if any particular number of
keywords is related to higher number of shares.

``` r
ggplot(world_news_data, aes(x = as.factor(num_keywords), y = log(shares))) +
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
ggplot(world_news_data, aes(x = num_imgs, y = log(shares))) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title="Scatterplot of Number of Images vs log(shares)",
       x="Number of Images")
```

![](EntertainmentAnalysis_files/figure-gfm/Joey%20EDA%20img%20video-1.png)<!-- -->

``` r
# Scatterplot of num_videos and log(shares)
ggplot(world_news_data, aes(x = num_videos, y = log(shares))) +
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
cor(x = world_news_data$num_imgs, y = log(world_news_data$shares))
```

    ## [1] 0.1431437

``` r
# Correlation Coefficient (r) of num_videos vs log(shares) 
cor(x = world_news_data$num_videos, y = log(world_news_data$shares))
```

    ## [1] 0.03591697

## Correlation of Predictors

Now, before creating our prediction models, we need to examine the
correlation among predictors, as we would not want to include predictors
in the model that are highly correlated. Since we have a large number of
predictors, let’s attempt to subset the predictors based on measuring
similar attributes.

``` r
# Create a correlogram for predictors that measure attributes of "tokens" 
# and "words"
cor1 <- world_news_data %>% select(contains("tokens"), contains("words"))
cormatrix <- round(cor(cor1), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](EntertainmentAnalysis_files/figure-gfm/John%20EDA7-1.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of keywords 
cor2 <- world_news_data %>% select(contains("keywords"), contains("kw"))
cormatrix <- round(cor(cor2), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](EntertainmentAnalysis_files/figure-gfm/John%20EDA7-2.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of "positve" and
# "negative" connotation 
cor3 <- world_news_data %>% select(contains("positive"), contains("negative"))
cormatrix <- round(cor(cor3), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](EntertainmentAnalysis_files/figure-gfm/John%20EDA7-3.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of article titles
cor4 <- world_news_data %>% select(contains("title"))
cormatrix <- round(cor(cor4), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](EntertainmentAnalysis_files/figure-gfm/John%20EDA7-4.png)<!-- -->

# Model Selection

``` r
set.seed(31415)

partition <- createDataPartition(y = world_news_data$shares, 
                                 p= 0.7, 
                                 list = FALSE)
world_news_train <- world_news_data[partition,]
world_news_test <- world_news_data[-partition,]
```

Linear regression models a response variable as a linear combination of
input predictor variables. The models are often fitted using the least
squares approach, which is a method that finds the best fit by
minimizing the sum of the squares of the residuals of the points from
the curve. When we fit the model on the training data we would get
coefficients corresponding to each of the predictor variables, and these
coefficients can be used to make predictions on the response with
predictor variables from the test data.

## Linear Model \#1

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
            data = world_news_train)

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
    ##     data = world_news_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0187 -0.5918 -0.1748  0.4554  4.2887 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      7.978e+00  2.982e-01  26.751  < 2e-16 ***
    ## n_non_stop_words                -5.313e+00  4.680e+00  -1.135  0.25652    
    ## num_hrefs                        2.446e-03  2.796e-03   0.875  0.38182    
    ## num_imgs                        -2.069e-02  1.904e-02  -1.087  0.27723    
    ## num_videos                       6.103e-02  2.783e-02   2.193  0.02846 *  
    ## average_token_length             1.969e+00  1.991e+00   0.989  0.32300    
    ## kw_min_min                       1.054e-03  3.529e-04   2.986  0.00288 ** 
    ## kw_min_avg                      -5.833e-05  7.268e-05  -0.802  0.42240    
    ## kw_max_avg                      -2.236e-05  8.505e-06  -2.629  0.00865 ** 
    ## kw_avg_avg                       1.626e-04  5.772e-05   2.818  0.00490 ** 
    ## self_reference_avg_sharess       6.125e-06  2.626e-06   2.332  0.01983 *  
    ## weekday_is_monday               -1.582e-01  8.354e-02  -1.894  0.05839 .  
    ## weekday_is_tuesday              -1.947e-01  9.064e-02  -2.148  0.03185 *  
    ## weekday_is_wednesday            -2.069e-01  8.129e-02  -2.545  0.01103 *  
    ## weekday_is_thursday             -1.833e-01  8.254e-02  -2.220  0.02654 *  
    ## weekday_is_friday               -2.683e-01  8.732e-02  -3.072  0.00216 ** 
    ## LDA_00                           6.348e-02  9.698e-02   0.655  0.51283    
    ## LDA_02                          -2.536e-01  2.325e-01  -1.091  0.27557    
    ## global_subjectivity             -9.324e-02  3.146e-01  -0.296  0.76701    
    ## title_sentiment_polarity        -2.128e-01  9.978e-02  -2.132  0.03314 *  
    ## I(n_tokens_title^2)             -2.493e-04  7.319e-04  -0.341  0.73347    
    ## I(num_videos^2)                 -2.487e-03  1.532e-03  -1.623  0.10478    
    ## I(average_token_length^2)       -2.092e-01  2.118e-01  -0.987  0.32363    
    ## I(kw_min_avg^2)                  9.521e-09  2.434e-08   0.391  0.69566    
    ## I(kw_avg_avg^2)                  2.587e-09  3.548e-09   0.729  0.46595    
    ## I(self_reference_avg_sharess^2) -1.522e-11  9.232e-12  -1.648  0.09948 .  
    ## I(title_sentiment_polarity^2)    1.133e-01  1.378e-01   0.822  0.41135    
    ## num_hrefs:num_imgs               5.811e-05  1.711e-04   0.340  0.73414    
    ## kw_min_min:weekday_is_tuesday   -1.623e-03  7.832e-04  -2.072  0.03842 *  
    ## num_imgs:n_tokens_title          2.959e-03  1.820e-03   1.625  0.10429    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.908 on 1442 degrees of freedom
    ## Multiple R-squared:  0.08101,    Adjusted R-squared:  0.06253 
    ## F-statistic: 4.383 on 29 and 1442 DF,  p-value: 1.55e-13

## Linear Model \#2

``` r
# Remove variables with possible colinearity
reduced_world_news_data <- world_news_data %>%
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
    ## -4.1816 -0.6124 -0.1779  0.4673  4.5221 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                               7.284e+00  9.989e-01   7.292 4.32e-13 ***
    ## n_tokens_title                            6.162e-02  9.449e-02   0.652  0.51440    
    ## n_tokens_content                          1.119e-04  5.084e-05   2.201  0.02782 *  
    ## num_hrefs                                 3.641e-03  2.319e-03   1.570  0.11649    
    ## num_imgs                                  2.942e-02  2.037e-02   1.444  0.14880    
    ## num_videos                                1.562e-02  1.062e-02   1.471  0.14137    
    ## num_keywords                              1.738e-03  1.428e-02   0.122  0.90316    
    ## average_token_length                      1.174e-01  2.496e-01   0.470  0.63810    
    ## is_weekend                                2.581e-01  5.392e-02   4.786 1.83e-06 ***
    ## global_subjectivity                      -5.401e-02  3.002e-01  -0.180  0.85721    
    ## global_sentiment_polarity                -1.282e+00  1.894e+00  -0.677  0.49855    
    ## title_sentiment_polarity                 -1.091e-01  7.259e-02  -1.502  0.13314    
    ## self_reference_avg_sharess                4.004e-06  1.344e-06   2.979  0.00292 ** 
    ## rate_positive_words                      -4.545e-01  1.274e+00  -0.357  0.72120    
    ## n_tokens_title:average_token_length      -2.017e-02  2.413e-02  -0.836  0.40320    
    ## n_tokens_title:global_sentiment_polarity  1.268e-01  1.884e-01   0.673  0.50096    
    ## n_tokens_title:rate_positive_words        2.595e-02  1.276e-01   0.203  0.83889    
    ## n_tokens_content:num_imgs                -1.327e-06  1.094e-06  -1.212  0.22547    
    ## num_imgs:num_keywords                    -3.126e-04  1.976e-03  -0.158  0.87436    
    ## num_imgs:global_subjectivity             -7.809e-03  2.611e-02  -0.299  0.76489    
    ## num_imgs:global_sentiment_polarity       -4.771e-02  3.034e-02  -1.572  0.11603    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9206 on 2078 degrees of freedom
    ## Multiple R-squared:  0.05576,    Adjusted R-squared:  0.04667 
    ## F-statistic: 6.135 on 20 and 2078 DF,  p-value: 3.191e-16

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
               data = world_news_train,
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
boostedFit <- train(log(shares) ~ ., data = world_news_train, 
                    method = "gbm",
                    trControl = control,
                    preProcess = c("center", "scale"),
                    tuneGrid = expand.grid(n.trees = c(25, 50, 100, 150, 200),
                                           interaction.depth = c(1:4),
                                           shrinkage = 0.1,
                                           n.minobsinnode = 10),
                    verbose = FALSE)
```

# Model Evaluation

We can compare the RMSE, Rsquared, and MAE of the four model fits on the
test data.

``` r
prediction <- predict(lm1Fit, newdata = world_news_test)
lm1 <- round(postResample(prediction, log(world_news_test$shares)), 4)

prediction <- predict(lm2Fit, newdata = world_news_test)
lm2 <- round(postResample(prediction, log(world_news_test$shares)), 4)

prediction <- predict(rfFit, newdata = world_news_test)
rf <- round(postResample(prediction, log(world_news_test$shares)), 4)

prediction <- predict(boostedFit, newdata = world_news_test)
boost <- round(postResample(prediction, log(world_news_test$shares)), 4)

compareFits <- data.frame(lm1, lm2, rf, boost)
names(compareFits) <- c("Linear Model 1", 
                        "Linear Model 2",
                        "Random Forest Model",
                        "Boosted Tree Model")

compareFitsLong <- data.frame(t(compareFits))

knitr::kable(compareFitsLong)
```

|                     |   RMSE | Rsquared |    MAE |
| :------------------ | -----: | -------: | -----: |
| Linear Model 1      | 0.9310 |   0.0532 | 0.7149 |
| Linear Model 2      | 0.9333 |   0.0445 | 0.7146 |
| Random Forest Model | 0.9291 |   0.0543 | 0.7123 |
| Boosted Tree Model  | 0.9458 |   0.0209 | 0.7190 |

We will choose the model with the lowest RMSE.

``` r
# Sort the table of models by ascending RMSE and pick the first row as the model
finalModel <- compareFitsLong %>% arrange(RMSE) %>% filter(row_number()==1) 
```

### Our final model is the Random Forest Model. When we fit the model on the test data we get RMSE=0.9291, Rsquared=0.0543, and MAE=0.7123.
