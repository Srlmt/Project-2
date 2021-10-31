Example Using Socmed Channel Data
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

![](SocmedAnalysis_files/figure-gfm/Joey%20EDA%20shares-1.png)<!-- -->

From the histogram We can see that the distribution is heavily right
skewed. The histogram scale may be influenced by potential outliers, so
we can look at the distribution of `shares` under 5,000.

``` r
# Histogram of shares < 5000
ggplot(filter(channel_data, shares < 5000), aes(x=shares)) +
  geom_histogram(bins=50, fill="navy", col="darkgreen") +
  labs(title="Histogram of shares under 5000")
```

![](SocmedAnalysis_files/figure-gfm/Joey%20EDA%20shares%20le%205000-1.png)<!-- -->

As we can see, the distribution is still heavily right skewed. So we may
want to look at the distribution of `log(shares)`.

``` r
# Histogram of log(shares) 
ggplot(channel_data, aes(x=log(shares))) +
  geom_histogram(bins=50, fill="navy", col="darkgreen") +
  labs(title="Histogram of log(shares)")
```

![](SocmedAnalysis_files/figure-gfm/Joey%20EDA%20log%20shares-1.png)<!-- -->

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

| Min. |   Q1 | Median | Mean |   SD |   Q3 |    Max | CV   |
| ---: | ---: | -----: | ---: | ---: | ---: | -----: | :--- |
|    5 | 1400 |   2100 | 3629 | 5524 | 3800 | 122800 | 152% |

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

|  Min. |    Q1 | Median |  Mean |    SD |    Q3 |    Max | CV  |
| ----: | ----: | -----: | ----: | ----: | ----: | -----: | :-- |
| 1.609 | 7.244 |   7.65 | 7.777 | 0.841 | 8.243 | 11.718 | 11% |

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

![](SocmedAnalysis_files/figure-gfm/Joey%20EDA%20Title%20Words%20bar-1.png)<!-- -->

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

| Title Word Count |   n | Min. |   Q1 | Median | Mean |   SD |   Q3 |   Max |
| ---------------: | --: | ---: | ---: | -----: | ---: | ---: | ---: | ----: |
|                4 |   5 | 7.00 | 7.09 |   7.74 | 7.81 | 0.79 | 8.58 |  8.67 |
|                5 |  25 | 5.90 | 7.00 |   7.55 | 7.57 | 0.79 | 7.90 |  9.37 |
|                6 | 112 | 5.85 | 7.00 |   7.38 | 7.64 | 0.83 | 8.19 | 10.99 |
|                7 | 218 | 5.56 | 7.24 |   7.70 | 7.83 | 0.80 | 8.32 | 10.88 |
|                8 | 329 | 3.97 | 7.24 |   7.74 | 7.83 | 0.87 | 8.32 | 10.95 |
|                9 | 478 | 2.08 | 7.24 |   7.70 | 7.78 | 0.88 | 8.29 | 10.64 |
|               10 | 392 | 5.54 | 7.24 |   7.70 | 7.78 | 0.79 | 8.24 | 10.96 |
|               11 | 335 | 6.12 | 7.24 |   7.70 | 7.83 | 0.85 | 8.28 | 11.72 |
|               12 | 219 | 6.16 | 7.24 |   7.60 | 7.75 | 0.80 | 8.15 | 10.86 |
|               13 | 126 | 5.11 | 7.17 |   7.55 | 7.69 | 0.77 | 8.03 |  9.79 |
|               14 |  50 | 6.59 | 7.09 |   7.55 | 7.66 | 0.73 | 8.06 | 10.45 |
|               15 |  22 | 1.61 | 7.09 |   7.41 | 7.32 | 1.53 | 7.90 | 10.13 |
|               16 |   7 | 6.70 | 7.39 |   8.10 | 7.89 | 0.77 | 8.37 |  8.92 |
|               17 |   3 | 7.90 | 8.04 |   8.19 | 8.30 | 0.47 | 8.51 |  8.82 |
|               18 |   2 | 6.06 | 6.74 |   7.43 | 7.43 | 1.93 | 8.11 |  8.79 |

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

![](SocmedAnalysis_files/figure-gfm/Joey%20EDA%20Title%20Words%20Boxplot-1.png)<!-- -->

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

![](SocmedAnalysis_files/figure-gfm/John%20EDA1-1.png)<!-- -->

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

![](SocmedAnalysis_files/figure-gfm/John%20EDA2-1.png)<!-- -->

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
| (-0.05,0.05\]      | 7.760 |  7.650 | 1,075 |
| (0.05,0.15\]       | 7.934 |  7.863 |   169 |
| (0.15,0.25\]       | 7.712 |  7.550 |    95 |
| (0.25,0.35\]       | 7.758 |  7.601 |   158 |
| (0.35,0.45\]       | 7.743 |  7.550 |   166 |
| (0.45,0.55\]       | 7.765 |  7.650 |   271 |
| (0.55,0.65\]       | 7.596 |  7.601 |    87 |
| (0.65,0.75\]       | 7.829 |  7.673 |    70 |
| (0.75,0.85\]       | 7.820 |  7.575 |    48 |
| (0.85,0.95\]       | 7.922 |  7.696 |    44 |
| (0.95,1.05\]       | 7.878 |  7.741 |   140 |

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
| (-0.05,0.05\]       | 7.625 |  7.467 |   136 |
| (0.05,0.15\]        | 7.778 |  7.650 |   367 |
| (0.15,0.25\]        | 7.778 |  7.696 |   741 |
| (0.25,0.35\]        | 7.823 |  7.696 |   620 |
| (0.35,0.45\]        | 7.791 |  7.601 |   310 |
| (0.45,0.55\]        | 7.721 |  7.575 |   104 |
| (0.55,0.65\]        | 7.714 |  7.741 |    21 |
| (0.65,0.75\]        | 7.277 |  7.090 |    17 |
| (0.75,0.85\]        | 8.341 |  8.367 |     4 |
| (0.95,1.05\]        | 8.157 |  8.666 |     3 |

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
| \[0,0.1)              | 7.688 |  7.863 |    27 |
| \[0.1,0.2)            | 7.882 |  7.937 |    61 |
| \[0.2,0.3)            | 7.893 |  7.783 |   463 |
| \[0.3,0.4)            | 7.833 |  7.741 | 1,082 |
| \[0.4,0.5)            | 7.652 |  7.496 |   563 |
| \[0.5,0.6)            | 7.412 |  7.378 |   103 |
| \[0.6,0.7)            | 7.354 |  7.313 |    19 |
| \[0.7,0.8)            | 7.393 |  7.170 |     3 |
| \[0.8,0.9)            | 7.464 |  7.464 |     2 |

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

![](SocmedAnalysis_files/figure-gfm/John%20EDA6-1.png)<!-- -->

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

![](SocmedAnalysis_files/figure-gfm/Joey%20EDA%20img%20video-1.png)<!-- -->

``` r
# Scatterplot of num_videos and log(shares)
ggplot(channel_data, aes(x = num_videos, y = log(shares))) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title="Scatterplot of Number of Videos vs log(shares)",
       x="Number of Videos")
```

![](SocmedAnalysis_files/figure-gfm/Joey%20EDA%20img%20video-2.png)<!-- -->

We can also examine the correlation coefficient (r) to observe the
strength of the association. For absolute values of r, we can considered
0-0.19 as very weak, 0.2-0.39 as weak, 0.4-0.59 as moderate, 0.6-0.79 as
strong and 0.8-1 as very strong correlation.

``` r
# Correlation Coefficient (r) of num_imgs vs log(shares) 
cor(x = channel_data$num_imgs, y = log(channel_data$shares))
```

    ## [1] -0.05067036

``` r
# Correlation Coefficient (r) of num_videos vs log(shares) 
cor(x = channel_data$num_videos, y = log(channel_data$shares))
```

    ## [1] 0.002356568

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

![](SocmedAnalysis_files/figure-gfm/John%20EDA7-1.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of keywords 
cor2 <- channel_data %>% select(contains("keywords"), contains("kw"))
cormatrix <- round(cor(cor2), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](SocmedAnalysis_files/figure-gfm/John%20EDA7-2.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of "positve" and
# "negative" connotation 
cor3 <- channel_data %>% select(contains("positive"), contains("negative"))
cormatrix <- round(cor(cor3), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](SocmedAnalysis_files/figure-gfm/John%20EDA7-3.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of article titles
cor4 <- channel_data %>% select(contains("title"))
cormatrix <- round(cor(cor4), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](SocmedAnalysis_files/figure-gfm/John%20EDA7-4.png)<!-- -->

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
    ## -5.1006 -0.4874 -0.1026  0.3948  3.3539 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      6.709e+00  3.150e-01  21.295  < 2e-16 ***
    ## n_non_stop_words                 9.780e-01  4.461e+00   0.219 0.826513    
    ## num_hrefs                        1.935e-03  2.075e-03   0.932 0.351251    
    ## num_imgs                         1.097e-02  1.203e-02   0.912 0.362138    
    ## num_videos                       1.411e-02  1.152e-02   1.225 0.220743    
    ## average_token_length            -1.707e-01  1.895e+00  -0.090 0.928202    
    ## kw_min_min                       9.757e-04  2.761e-04   3.534 0.000421 ***
    ## kw_min_avg                      -8.011e-05  5.670e-05  -1.413 0.157868    
    ## kw_max_avg                       2.222e-06  1.038e-05   0.214 0.830470    
    ## kw_avg_avg                       2.632e-04  4.671e-05   5.635 2.06e-08 ***
    ## self_reference_avg_sharess       1.539e-05  1.779e-06   8.652  < 2e-16 ***
    ## weekday_is_monday               -8.511e-02  7.475e-02  -1.139 0.255076    
    ## weekday_is_tuesday              -1.812e-01  7.362e-02  -2.462 0.013934 *  
    ## weekday_is_wednesday            -1.833e-01  7.146e-02  -2.566 0.010391 *  
    ## weekday_is_thursday             -1.799e-01  6.947e-02  -2.590 0.009698 ** 
    ## weekday_is_friday               -4.451e-02  7.532e-02  -0.591 0.554638    
    ## LDA_00                           7.990e-01  8.121e-02   9.839  < 2e-16 ***
    ## LDA_02                           1.685e-01  1.049e-01   1.606 0.108493    
    ## global_subjectivity             -6.530e-01  2.378e-01  -2.746 0.006092 ** 
    ## title_sentiment_polarity        -1.726e-01  9.304e-02  -1.855 0.063721 .  
    ## I(n_tokens_title^2)              1.971e-04  5.252e-04   0.375 0.707563    
    ## I(num_videos^2)                  4.808e-05  4.600e-04   0.105 0.916770    
    ## I(average_token_length^2)        4.743e-03  2.015e-01   0.024 0.981223    
    ## I(kw_min_avg^2)                  2.457e-09  1.830e-08   0.134 0.893183    
    ## I(kw_avg_avg^2)                 -7.252e-09  1.410e-09  -5.145 3.01e-07 ***
    ## I(self_reference_avg_sharess^2) -5.513e-11  8.035e-12  -6.862 9.70e-12 ***
    ## I(title_sentiment_polarity^2)    3.740e-01  1.389e-01   2.692 0.007177 ** 
    ## num_hrefs:num_imgs              -1.378e-04  9.136e-05  -1.509 0.131563    
    ## kw_min_min:weekday_is_tuesday   -1.741e-04  6.463e-04  -0.269 0.787630    
    ## num_imgs:n_tokens_title         -9.791e-04  1.184e-03  -0.827 0.408292    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7805 on 1598 degrees of freedom
    ## Multiple R-squared:  0.1695, Adjusted R-squared:  0.1544 
    ## F-statistic: 11.24 on 29 and 1598 DF,  p-value: < 2.2e-16

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
    ## -5.5502 -0.5229 -0.1043  0.4444  3.7458 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                               7.173e+00  8.564e-01   8.375  < 2e-16 ***
    ## n_tokens_title                            3.440e-02  8.246e-02   0.417   0.6766    
    ## n_tokens_content                          1.932e-04  4.487e-05   4.306 1.73e-05 ***
    ## num_hrefs                                -2.286e-03  1.720e-03  -1.329   0.1839    
    ## num_imgs                                  1.104e-02  1.366e-02   0.808   0.4189    
    ## num_videos                                1.645e-03  4.734e-03   0.348   0.7282    
    ## num_keywords                              4.534e-02  9.138e-03   4.962 7.50e-07 ***
    ## average_token_length                      2.348e-01  1.951e-01   1.204   0.2289    
    ## is_weekend                                9.445e-02  5.069e-02   1.863   0.0625 .  
    ## global_subjectivity                      -3.034e-01  2.195e-01  -1.382   0.1670    
    ## global_sentiment_polarity                 1.961e+00  1.124e+00   1.745   0.0812 .  
    ## title_sentiment_polarity                  8.519e-02  6.782e-02   1.256   0.2092    
    ## self_reference_avg_sharess                4.545e-06  6.867e-07   6.619 4.48e-11 ***
    ## rate_positive_words                      -1.272e+00  7.658e-01  -1.661   0.0968 .  
    ## n_tokens_title:average_token_length      -2.944e-02  1.928e-02  -1.527   0.1268    
    ## n_tokens_title:global_sentiment_polarity -2.949e-01  1.150e-01  -2.564   0.0104 *  
    ## n_tokens_title:rate_positive_words        1.795e-01  7.944e-02   2.259   0.0239 *  
    ## n_tokens_content:num_imgs                -6.056e-06  2.760e-06  -2.195   0.0283 *  
    ## num_imgs:num_keywords                    -2.954e-04  1.213e-03  -0.244   0.8076    
    ## num_imgs:global_subjectivity              6.536e-03  2.396e-02   0.273   0.7851    
    ## num_imgs:global_sentiment_polarity       -6.611e-02  3.628e-02  -1.822   0.0686 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8161 on 2302 degrees of freedom
    ## Multiple R-squared:  0.06635,    Adjusted R-squared:  0.05824 
    ## F-statistic:  8.18 on 20 and 2302 DF,  p-value: < 2.2e-16

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
| Linear Model 1      | 0.9687 |   0.0394 | 0.5919 |
| Linear Model 2      | 0.7964 |   0.0641 | 0.5985 |
| Random Forest Model | 0.7335 |   0.2083 | 0.5508 |
| Boosted Tree Model  | 0.7327 |   0.2122 | 0.5481 |

Our final model is the Boosted Tree Model. When we fit the model on the
test data we get RMSE=0.7327, Rsquared=0.2122, and MAE=0.5481.
