Example Using World Channel Data
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

![](WorldAnalysis_files/figure-gfm/Joey%20EDA%20shares-1.png)<!-- -->

From the histogram We can see that the distribution is heavily right
skewed. The histogram scale may be influenced by potential outliers, so
we can look at the distribution of `shares` under 5,000.

``` r
# Histogram of shares < 5000
ggplot(filter(channel_data, shares < 5000), aes(x=shares)) +
  geom_histogram(bins=50, fill="navy", col="darkgreen") +
  labs(title="Histogram of shares under 5000")
```

![](WorldAnalysis_files/figure-gfm/Joey%20EDA%20shares%20le%205000-1.png)<!-- -->

As we can see, the distribution is still heavily right skewed. So we may
want to look at the distribution of `log(shares)`.

``` r
# Histogram of log(shares) 
ggplot(channel_data, aes(x=log(shares))) +
  geom_histogram(bins=50, fill="navy", col="darkgreen") +
  labs(title="Histogram of log(shares)")
```

![](WorldAnalysis_files/figure-gfm/Joey%20EDA%20log%20shares-1.png)<!-- -->

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
|   35 | 827 |   1100 | 2288 | 6090 | 1900 | 284700 | 266% |

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

|  Min. |    Q1 | Median |  Mean |   SD |   Q3 |    Max | CV  |
| ----: | ----: | -----: | ----: | ---: | ---: | -----: | :-- |
| 3.555 | 6.718 |  7.003 | 7.203 | 0.83 | 7.55 | 12.559 | 12% |

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

![](WorldAnalysis_files/figure-gfm/Joey%20EDA%20Title%20Words%20bar-1.png)<!-- -->

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
|                4 |    3 | 6.51 | 6.58 |   6.65 | 6.72 | 0.26 | 6.83 |  7.00 |
|                5 |   26 | 5.42 | 6.76 |   7.17 | 7.27 | 0.81 | 7.67 |  9.38 |
|                6 |  133 | 3.81 | 6.67 |   7.00 | 7.17 | 0.80 | 7.60 |  9.29 |
|                7 |  340 | 3.76 | 6.67 |   7.09 | 7.19 | 0.97 | 7.60 | 11.76 |
|                8 |  793 | 3.71 | 6.71 |   7.00 | 7.11 | 0.78 | 7.38 | 10.15 |
|                9 | 1272 | 3.56 | 6.72 |   7.00 | 7.19 | 0.81 | 7.55 | 10.73 |
|               10 | 1598 | 3.76 | 6.72 |   7.00 | 7.19 | 0.82 | 7.50 | 11.62 |
|               11 | 1545 | 4.04 | 6.73 |   7.00 | 7.23 | 0.83 | 7.55 | 11.86 |
|               12 | 1224 | 3.74 | 6.71 |   7.00 | 7.20 | 0.80 | 7.50 | 11.48 |
|               13 |  812 | 3.95 | 6.74 |   7.09 | 7.22 | 0.82 | 7.56 | 11.35 |
|               14 |  385 | 4.04 | 6.74 |   7.09 | 7.31 | 0.92 | 7.55 | 12.56 |
|               15 |  194 | 4.49 | 6.72 |   7.09 | 7.27 | 0.85 | 7.65 | 10.78 |
|               16 |   67 | 4.08 | 6.75 |   7.17 | 7.37 | 1.09 | 7.65 | 11.66 |
|               17 |   24 | 6.37 | 6.90 |   7.24 | 7.70 | 1.24 | 7.95 | 10.87 |
|               18 |    7 | 6.25 | 6.60 |   7.00 | 7.12 | 0.90 | 7.21 |  8.95 |
|               19 |    2 | 6.83 | 6.98 |   7.13 | 7.13 | 0.43 | 7.29 |  7.44 |
|               20 |    1 | 6.70 | 6.70 |   6.70 | 6.70 |   NA | 6.70 |  6.70 |
|               23 |    1 | 8.79 | 8.79 |   8.79 | 8.79 |   NA | 8.79 |  8.79 |

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

![](WorldAnalysis_files/figure-gfm/Joey%20EDA%20Title%20Words%20Boxplot-1.png)<!-- -->

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

![](WorldAnalysis_files/figure-gfm/John%20EDA1-1.png)<!-- -->

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

![](WorldAnalysis_files/figure-gfm/John%20EDA2-1.png)<!-- -->

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
| (-0.05,0.05\]      | 7.172 |  7.003 | 4,329 |
| (0.05,0.15\]       | 7.167 |  7.003 |   385 |
| (0.15,0.25\]       | 7.213 |  7.090 |   426 |
| (0.25,0.35\]       | 7.216 |  7.003 |   527 |
| (0.35,0.45\]       | 7.238 |  7.003 |   586 |
| (0.45,0.55\]       | 7.199 |  7.003 |   848 |
| (0.55,0.65\]       | 7.246 |  7.003 |   239 |
| (0.65,0.75\]       | 7.305 |  7.090 |   316 |
| (0.75,0.85\]       | 7.239 |  7.090 |   119 |
| (0.85,0.95\]       | 7.267 |  7.090 |   195 |
| (0.95,1.05\]       | 7.346 |  7.170 |   457 |

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
| (-0.05,0.05\]       | 7.420 |  7.170 |   444 |
| (0.05,0.15\]        | 7.320 |  7.170 |   470 |
| (0.15,0.25\]        | 7.270 |  7.090 | 1,389 |
| (0.25,0.35\]        | 7.226 |  7.003 | 2,005 |
| (0.35,0.45\]        | 7.157 |  7.003 | 2,060 |
| (0.45,0.55\]        | 7.134 |  7.003 | 1,265 |
| (0.55,0.65\]        | 7.067 |  6.908 |   508 |
| (0.65,0.75\]        | 7.064 |  6.908 |   212 |
| (0.75,0.85\]        | 7.118 |  7.003 |    53 |
| (0.85,0.95\]        | 6.870 |  6.770 |    11 |
| (0.95,1.05\]        | 7.148 |  7.047 |    10 |

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
| \[0,0.1)              | 7.530 |  7.244 |   282 |
| \[0.1,0.2)            | 7.121 |  6.908 |   243 |
| \[0.2,0.3)            | 7.154 |  7.003 | 2,310 |
| \[0.3,0.4)            | 7.189 |  7.003 | 4,174 |
| \[0.4,0.5)            | 7.275 |  7.090 | 1,212 |
| \[0.5,0.6)            | 7.292 |  7.090 |   182 |
| \[0.6,0.7)            | 7.067 |  7.003 |    21 |
| \[0.8,0.9)            | 7.951 |  7.696 |     3 |

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

![](WorldAnalysis_files/figure-gfm/John%20EDA6-1.png)<!-- -->

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

![](WorldAnalysis_files/figure-gfm/Joey%20EDA%20img%20video-1.png)<!-- -->

``` r
# Scatterplot of num_videos and log(shares)
ggplot(channel_data, aes(x = num_videos, y = log(shares))) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title="Scatterplot of Number of Videos vs log(shares)",
       x="Number of Videos")
```

![](WorldAnalysis_files/figure-gfm/Joey%20EDA%20img%20video-2.png)<!-- -->

We can also examine the correlation coefficient (r) to observe the
strength of the association. For absolute values of r, we can considered
0-0.19 as very weak, 0.2-0.39 as weak, 0.4-0.59 as moderate, 0.6-0.79 as
strong and 0.8-1 as very strong correlation.

``` r
# Correlation Coefficient (r) of num_imgs vs log(shares) 
cor(x = channel_data$num_imgs, y = log(channel_data$shares))
```

    ## [1] 0.1304567

``` r
# Correlation Coefficient (r) of num_videos vs log(shares) 
cor(x = channel_data$num_videos, y = log(channel_data$shares))
```

    ## [1] 0.06812929

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

![](WorldAnalysis_files/figure-gfm/John%20EDA7-1.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of keywords 
cor2 <- channel_data %>% select(contains("keywords"), contains("kw"))
cormatrix <- round(cor(cor2), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](WorldAnalysis_files/figure-gfm/John%20EDA7-2.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of "positve" and
# "negative" connotation 
cor3 <- channel_data %>% select(contains("positive"), contains("negative"))
cormatrix <- round(cor(cor3), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](WorldAnalysis_files/figure-gfm/John%20EDA7-3.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of article titles
cor4 <- channel_data %>% select(contains("title"))
cormatrix <- round(cor(cor4), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](WorldAnalysis_files/figure-gfm/John%20EDA7-4.png)<!-- -->

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
    ## -3.4624 -0.4401 -0.1164  0.2997  5.0612 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      6.973e+00  1.107e-01  62.976  < 2e-16 ***
    ## n_non_stop_words                 1.375e+01  2.703e+00   5.086 3.77e-07 ***
    ## num_hrefs                        9.848e-03  1.367e-03   7.206 6.49e-13 ***
    ## num_imgs                         5.599e-02  1.118e-02   5.008 5.67e-07 ***
    ## num_videos                       3.722e-02  9.923e-03   3.751 0.000178 ***
    ## average_token_length            -5.580e+00  1.118e+00  -4.989 6.23e-07 ***
    ## kw_min_min                       1.451e-03  2.080e-04   6.974 3.42e-12 ***
    ## kw_min_avg                      -2.570e-04  4.442e-05  -5.786 7.57e-09 ***
    ## kw_max_avg                      -1.990e-05  7.119e-06  -2.795 0.005205 ** 
    ## kw_avg_avg                       2.837e-04  3.352e-05   8.464  < 2e-16 ***
    ## self_reference_avg_sharess       7.884e-06  1.234e-06   6.386 1.83e-10 ***
    ## weekday_is_monday               -2.392e-01  3.805e-02  -6.286 3.50e-10 ***
    ## weekday_is_tuesday              -2.645e-01  3.774e-02  -7.007 2.71e-12 ***
    ## weekday_is_wednesday            -3.053e-01  3.705e-02  -8.241  < 2e-16 ***
    ## weekday_is_thursday             -2.702e-01  3.697e-02  -7.307 3.09e-13 ***
    ## weekday_is_friday               -2.346e-01  3.870e-02  -6.061 1.44e-09 ***
    ## LDA_00                           3.099e-01  1.109e-01   2.794 0.005221 ** 
    ## LDA_02                          -1.986e-01  5.603e-02  -3.544 0.000397 ***
    ## global_subjectivity              4.379e-01  1.315e-01   3.331 0.000871 ***
    ## title_sentiment_polarity         1.809e-01  4.341e-02   4.168 3.12e-05 ***
    ## I(n_tokens_title^2)              1.076e-03  2.625e-04   4.097 4.23e-05 ***
    ## I(num_videos^2)                 -1.131e-03  3.893e-04  -2.905 0.003689 ** 
    ## I(average_token_length^2)        5.458e-01  1.157e-01   4.717 2.45e-06 ***
    ## I(kw_min_avg^2)                  1.026e-07  2.200e-08   4.664 3.16e-06 ***
    ## I(kw_avg_avg^2)                 -1.051e-08  3.853e-09  -2.729 0.006380 ** 
    ## I(self_reference_avg_sharess^2) -1.390e-11  2.449e-12  -5.675 1.45e-08 ***
    ## I(title_sentiment_polarity^2)    1.447e-01  7.141e-02   2.027 0.042732 *  
    ## num_hrefs:num_imgs              -3.145e-04  9.601e-05  -3.276 0.001060 ** 
    ## kw_min_min:weekday_is_tuesday   -1.006e-03  4.389e-04  -2.292 0.021940 *  
    ## num_imgs:n_tokens_title         -3.611e-03  1.020e-03  -3.540 0.000404 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7782 on 5870 degrees of freedom
    ## Multiple R-squared:  0.119,  Adjusted R-squared:  0.1146 
    ## F-statistic: 27.33 on 29 and 5870 DF,  p-value: < 2.2e-16

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
    ## -3.4816 -0.4624 -0.1442  0.3106  5.2739 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                               7.369e+00  2.577e-01  28.591  < 2e-16 ***
    ## n_tokens_title                            7.700e-03  2.320e-02   0.332 0.739966    
    ## n_tokens_content                         -6.728e-05  2.683e-05  -2.507 0.012186 *  
    ## num_hrefs                                 7.393e-03  1.077e-03   6.865 7.12e-12 ***
    ## num_imgs                                 -1.891e-02  8.631e-03  -2.191 0.028500 *  
    ## num_videos                                3.089e-02  5.642e-03   5.476 4.48e-08 ***
    ## num_keywords                              1.069e-03  5.363e-03   0.199 0.841970    
    ## average_token_length                     -1.933e-01  7.294e-02  -2.650 0.008067 ** 
    ## is_weekend                                2.795e-01  2.615e-02  10.686  < 2e-16 ***
    ## global_subjectivity                       6.847e-01  1.163e-01   5.888 4.06e-09 ***
    ## global_sentiment_polarity                 8.062e-01  9.763e-01   0.826 0.408960    
    ## title_sentiment_polarity                  1.171e-01  3.797e-02   3.085 0.002045 ** 
    ## self_reference_avg_sharess                2.229e-06  4.659e-07   4.784 1.75e-06 ***
    ## rate_positive_words                       1.225e-01  4.946e-01   0.248 0.804429    
    ## n_tokens_title:average_token_length       9.825e-05  6.710e-03   0.015 0.988317    
    ## n_tokens_title:global_sentiment_polarity -7.076e-02  9.025e-02  -0.784 0.433027    
    ## n_tokens_title:rate_positive_words        1.917e-02  4.590e-02   0.418 0.676177    
    ## n_tokens_content:num_imgs                -9.511e-06  2.481e-06  -3.833 0.000127 ***
    ## num_imgs:num_keywords                     3.807e-03  9.140e-04   4.166 3.14e-05 ***
    ## num_imgs:global_subjectivity              6.726e-02  1.433e-02   4.692 2.74e-06 ***
    ## num_imgs:global_sentiment_polarity       -7.896e-02  2.647e-02  -2.983 0.002866 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8004 on 8406 degrees of freedom
    ## Multiple R-squared:  0.07304,    Adjusted R-squared:  0.07084 
    ## F-statistic: 33.12 on 20 and 8406 DF,  p-value: < 2.2e-16

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
| Linear Model 1      | 0.7878 |   0.1164 | 0.5526 |
| Linear Model 2      | 0.8034 |   0.0808 | 0.5672 |
| Random Forest Model | 0.7754 |   0.1457 | 0.5447 |
| Boosted Tree Model  | 0.7702 |   0.1563 | 0.5392 |

Our final model is the Boosted Tree Model. When we fit the model on the
test data we get RMSE=0.7702, Rsquared=0.1563, and MAE=0.5392.
