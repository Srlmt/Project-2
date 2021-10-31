Example Using Tech Channel Data
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

![](TechAnalysis_files/figure-gfm/Joey%20EDA%20shares-1.png)<!-- -->

From the histogram We can see that the distribution is heavily right
skewed. The histogram scale may be influenced by potential outliers, so
we can look at the distribution of `shares` under 5,000.

``` r
# Histogram of shares < 5000
ggplot(filter(channel_data, shares < 5000), aes(x=shares)) +
  geom_histogram(bins=50, fill="navy", col="darkgreen") +
  labs(title="Histogram of shares under 5000")
```

![](TechAnalysis_files/figure-gfm/Joey%20EDA%20shares%20le%205000-1.png)<!-- -->

As we can see, the distribution is still heavily right skewed. So we may
want to look at the distribution of `log(shares)`.

``` r
# Histogram of log(shares) 
ggplot(channel_data, aes(x=log(shares))) +
  geom_histogram(bins=50, fill="navy", col="darkgreen") +
  labs(title="Histogram of log(shares)")
```

![](TechAnalysis_files/figure-gfm/Joey%20EDA%20log%20shares-1.png)<!-- -->

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
|   36 | 1100 |   1700 | 3072 | 9024 | 3000 | 663600 | 294% |

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
| 3.584 | 7.003 |  7.438 | 7.581 | 0.812 | 8.006 | 13.405 | 11% |

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

![](TechAnalysis_files/figure-gfm/Joey%20EDA%20Title%20Words%20bar-1.png)<!-- -->

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
|                4 |    4 | 6.73 | 7.00 |   7.44 | 7.75 | 1.18 | 8.18 |  9.38 |
|                5 |   41 | 6.62 | 7.17 |   7.60 | 7.73 | 0.82 | 8.07 |  9.81 |
|                6 |  198 | 5.30 | 7.24 |   7.65 | 7.76 | 0.92 | 8.24 | 11.16 |
|                7 |  474 | 5.10 | 7.00 |   7.50 | 7.60 | 0.83 | 8.04 | 10.53 |
|                8 |  858 | 4.41 | 7.00 |   7.44 | 7.62 | 0.84 | 8.04 | 10.92 |
|                9 | 1242 | 4.16 | 7.00 |   7.44 | 7.59 | 0.85 | 8.01 | 11.55 |
|               10 | 1362 | 4.52 | 7.00 |   7.44 | 7.57 | 0.78 | 8.01 | 10.84 |
|               11 | 1238 | 4.78 | 7.00 |   7.44 | 7.57 | 0.78 | 8.01 | 11.18 |
|               12 |  922 | 5.93 | 7.00 |   7.38 | 7.55 | 0.77 | 7.90 | 11.12 |
|               13 |  559 | 5.33 | 6.91 |   7.38 | 7.52 | 0.80 | 7.97 | 13.41 |
|               14 |  281 | 3.58 | 7.00 |   7.44 | 7.60 | 0.89 | 8.13 | 10.78 |
|               15 |  114 | 6.32 | 7.00 |   7.28 | 7.46 | 0.70 | 7.73 | 10.60 |
|               16 |   26 | 6.52 | 6.82 |   7.35 | 7.56 | 0.89 | 8.15 |  9.69 |
|               17 |   19 | 6.38 | 7.21 |   7.55 | 7.86 | 1.17 | 8.05 | 10.88 |
|               18 |    6 | 6.65 | 7.23 |   7.97 | 7.81 | 0.84 | 8.31 |  8.88 |
|               19 |    1 | 7.31 | 7.31 |   7.31 | 7.31 |   NA | 7.31 |  7.31 |
|               20 |    1 | 7.60 | 7.60 |   7.60 | 7.60 |   NA | 7.60 |  7.60 |

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

![](TechAnalysis_files/figure-gfm/Joey%20EDA%20Title%20Words%20Boxplot-1.png)<!-- -->

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

![](TechAnalysis_files/figure-gfm/John%20EDA1-1.png)<!-- -->

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

![](TechAnalysis_files/figure-gfm/John%20EDA2-1.png)<!-- -->

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
| (-0.05,0.05\]      | 7.565 |  7.438 | 3,654 |
| (0.05,0.15\]       | 7.599 |  7.496 |   300 |
| (0.15,0.25\]       | 7.546 |  7.438 |   246 |
| (0.25,0.35\]       | 7.690 |  7.550 |   480 |
| (0.35,0.45\]       | 7.476 |  7.378 |   518 |
| (0.45,0.55\]       | 7.562 |  7.438 |   974 |
| (0.55,0.65\]       | 7.623 |  7.496 |   319 |
| (0.65,0.75\]       | 7.615 |  7.438 |   239 |
| (0.75,0.85\]       | 7.624 |  7.438 |   143 |
| (0.85,0.95\]       | 7.718 |  7.496 |   131 |
| (0.95,1.05\]       | 7.693 |  7.550 |   342 |

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
| (-0.05,0.05\]       | 7.444 |  7.313 |   413 |
| (0.05,0.15\]        | 7.539 |  7.378 | 1,082 |
| (0.15,0.25\]        | 7.578 |  7.438 | 2,493 |
| (0.25,0.35\]        | 7.615 |  7.496 | 1,933 |
| (0.35,0.45\]        | 7.623 |  7.438 |   962 |
| (0.45,0.55\]        | 7.606 |  7.438 |   325 |
| (0.55,0.65\]        | 7.531 |  7.438 |    87 |
| (0.65,0.75\]        | 7.627 |  7.467 |    38 |
| (0.75,0.85\]        | 7.761 |  8.055 |     6 |
| (0.85,0.95\]        | 7.741 |  7.741 |     1 |
| (0.95,1.05\]        | 7.436 |  7.431 |     6 |

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
| \[0,0.1)              | 7.774 |  7.550 |    29 |
| \[0.1,0.2)            | 7.504 |  7.378 |   109 |
| \[0.2,0.3)            | 7.538 |  7.378 | 1,259 |
| \[0.3,0.4)            | 7.596 |  7.438 | 4,056 |
| \[0.4,0.5)            | 7.581 |  7.438 | 1,677 |
| \[0.5,0.6)            | 7.572 |  7.438 |   187 |
| \[0.6,0.7)            | 7.538 |  7.345 |    20 |
| \[0.7,0.8)            | 7.942 |  8.368 |     4 |
| \[0.8,0.9)            | 8.304 |  8.243 |     3 |
| \[0.9,1)              | 7.313 |  7.313 |     1 |
| NA                    | 7.003 |  7.003 |     1 |

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

![](TechAnalysis_files/figure-gfm/John%20EDA6-1.png)<!-- -->

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

![](TechAnalysis_files/figure-gfm/Joey%20EDA%20img%20video-1.png)<!-- -->

``` r
# Scatterplot of num_videos and log(shares)
ggplot(channel_data, aes(x = num_videos, y = log(shares))) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title="Scatterplot of Number of Videos vs log(shares)",
       x="Number of Videos")
```

![](TechAnalysis_files/figure-gfm/Joey%20EDA%20img%20video-2.png)<!-- -->

We can also examine the correlation coefficient (r) to observe the
strength of the association. For absolute values of r, we can considered
0-0.19 as very weak, 0.2-0.39 as weak, 0.4-0.59 as moderate, 0.6-0.79 as
strong and 0.8-1 as very strong correlation.

``` r
# Correlation Coefficient (r) of num_imgs vs log(shares) 
cor(x = channel_data$num_imgs, y = log(channel_data$shares))
```

    ## [1] 0.03183185

``` r
# Correlation Coefficient (r) of num_videos vs log(shares) 
cor(x = channel_data$num_videos, y = log(channel_data$shares))
```

    ## [1] 0.05358556

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

![](TechAnalysis_files/figure-gfm/John%20EDA7-1.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of keywords 
cor2 <- channel_data %>% select(contains("keywords"), contains("kw"))
cormatrix <- round(cor(cor2), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](TechAnalysis_files/figure-gfm/John%20EDA7-2.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of "positve" and
# "negative" connotation 
cor3 <- channel_data %>% select(contains("positive"), contains("negative"))
cormatrix <- round(cor(cor3), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](TechAnalysis_files/figure-gfm/John%20EDA7-3.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of article titles
cor4 <- channel_data %>% select(contains("title"))
cormatrix <- round(cor(cor4), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](TechAnalysis_files/figure-gfm/John%20EDA7-4.png)<!-- -->

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
    ## -4.0822 -0.5145 -0.1254  0.3992  5.7186 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      7.157e+00  2.204e-01  32.473  < 2e-16 ***
    ## n_non_stop_words                -4.695e+00  2.427e+00  -1.935 0.053090 .  
    ## num_hrefs                        9.283e-03  1.783e-03   5.207 1.99e-07 ***
    ## num_imgs                         2.887e-02  8.153e-03   3.542 0.000401 ***
    ## num_videos                       5.838e-02  1.134e-02   5.147 2.74e-07 ***
    ## average_token_length             1.887e+00  1.048e+00   1.799 0.072014 .  
    ## kw_min_min                       1.264e-03  1.707e-04   7.402 1.56e-13 ***
    ## kw_min_avg                      -2.796e-04  3.737e-05  -7.481 8.64e-14 ***
    ## kw_max_avg                      -3.333e-05  7.064e-06  -4.718 2.44e-06 ***
    ## kw_avg_avg                       4.128e-04  3.661e-05  11.276  < 2e-16 ***
    ## self_reference_avg_sharess       3.284e-06  8.657e-07   3.794 0.000150 ***
    ## weekday_is_monday               -2.545e-01  4.100e-02  -6.206 5.86e-10 ***
    ## weekday_is_tuesday              -2.729e-01  4.068e-02  -6.709 2.17e-11 ***
    ## weekday_is_wednesday            -2.496e-01  3.966e-02  -6.292 3.39e-10 ***
    ## weekday_is_thursday             -2.827e-01  4.012e-02  -7.048 2.05e-12 ***
    ## weekday_is_friday               -1.973e-01  4.264e-02  -4.627 3.80e-06 ***
    ## LDA_00                           2.948e-02  9.863e-02   0.299 0.765052    
    ## LDA_02                           9.040e-02  7.860e-02   1.150 0.250191    
    ## global_subjectivity              7.301e-02  1.559e-01   0.468 0.639516    
    ## title_sentiment_polarity         2.976e-02  5.654e-02   0.526 0.598661    
    ## I(n_tokens_title^2)              3.040e-04  2.916e-04   1.042 0.297244    
    ## I(num_videos^2)                 -1.094e-03  2.147e-04  -5.093 3.64e-07 ***
    ## I(average_token_length^2)       -2.089e-01  1.135e-01  -1.842 0.065587 .  
    ## I(kw_min_avg^2)                  9.156e-08  1.486e-08   6.162 7.72e-10 ***
    ## I(kw_avg_avg^2)                 -8.943e-09  3.377e-09  -2.648 0.008115 ** 
    ## I(self_reference_avg_sharess^2) -5.305e-12  1.561e-12  -3.399 0.000682 ***
    ## I(title_sentiment_polarity^2)    1.965e-01  8.594e-02   2.287 0.022240 *  
    ## num_hrefs:num_imgs              -1.746e-04  1.095e-04  -1.595 0.110824    
    ## kw_min_min:weekday_is_tuesday   -1.894e-04  3.822e-04  -0.496 0.620249    
    ## num_imgs:n_tokens_title         -2.338e-03  7.562e-04  -3.091 0.002005 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7758 on 5115 degrees of freedom
    ## Multiple R-squared:  0.09694,    Adjusted R-squared:  0.09182 
    ## F-statistic: 18.93 on 29 and 5115 DF,  p-value: < 2.2e-16

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
    ## -3.8318 -0.5305 -0.1443  0.4198  5.3615 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                               8.793e+00  6.542e-01  13.442  < 2e-16 ***
    ## n_tokens_title                           -9.064e-02  6.101e-02  -1.486 0.137405    
    ## n_tokens_content                          1.546e-04  2.933e-05   5.271 1.39e-07 ***
    ## num_hrefs                                 6.912e-03  1.363e-03   5.071 4.04e-07 ***
    ## num_imgs                                 -2.442e-02  1.188e-02  -2.055 0.039956 *  
    ## num_videos                                2.232e-02  6.022e-03   3.707 0.000211 ***
    ## num_keywords                             -2.370e-02  6.510e-03  -3.641 0.000273 ***
    ## average_token_length                     -4.110e-01  1.422e-01  -2.890 0.003866 ** 
    ## is_weekend                                3.025e-01  2.834e-02  10.676  < 2e-16 ***
    ## global_subjectivity                       2.818e-01  1.515e-01   1.860 0.062968 .  
    ## global_sentiment_polarity                 7.385e-01  8.908e-01   0.829 0.407069    
    ## title_sentiment_polarity                  1.355e-01  4.063e-02   3.336 0.000855 ***
    ## self_reference_avg_sharess                9.910e-07  2.847e-07   3.481 0.000502 ***
    ## rate_positive_words                       6.795e-01  5.254e-01   1.293 0.195986    
    ## n_tokens_title:average_token_length       3.453e-02  1.348e-02   2.562 0.010440 *  
    ## n_tokens_title:global_sentiment_polarity -1.250e-01  8.501e-02  -1.470 0.141604    
    ## n_tokens_title:rate_positive_words       -7.850e-02  5.082e-02  -1.545 0.122449    
    ## n_tokens_content:num_imgs                -6.259e-06  2.045e-06  -3.061 0.002217 ** 
    ## num_imgs:num_keywords                     2.551e-03  8.316e-04   3.067 0.002169 ** 
    ## num_imgs:global_subjectivity              1.964e-02  2.491e-02   0.788 0.430520    
    ## num_imgs:global_sentiment_polarity       -1.130e-02  2.411e-02  -0.469 0.639200    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7933 on 7325 degrees of freedom
    ## Multiple R-squared:  0.04936,    Adjusted R-squared:  0.04676 
    ## F-statistic: 19.02 on 20 and 7325 DF,  p-value: < 2.2e-16

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
| Linear Model 1      | 0.7775 |   0.0776 | 0.5956 |
| Linear Model 2      | 0.7863 |   0.0549 | 0.6012 |
| Random Forest Model | 0.7592 |   0.1190 | 0.5829 |
| Boosted Tree Model  | 0.7545 |   0.1300 | 0.5766 |

Our final model is the Boosted Tree Model. When we fit the model on the
test data we get RMSE=0.7545, Rsquared=0.13, and MAE=0.5766.
