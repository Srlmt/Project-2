Project 2
================
Joey Chen and John Williams
10/24/2021

  - [Introduction](#introduction)
  - [Data Preparation](#data-preparation)
  - [Exploratory Data Analysis](#exploratory-data-analysis)
      - [Distribution of Response
        Variable](#distribution-of-response-variable)
      - [Log(shares) by Day of Week](#logshares-by-day-of-week)
      - [Summary by Interval](#summary-by-interval)
      - [Log(shares) by Number of
        Keywords](#logshares-by-number-of-keywords)
      - [Log(shares) by Number of Images and
        Videos](#logshares-by-number-of-images-and-videos)
      - [Correlation of Predictors](#correlation-of-predictors)
  - [Modeling](#modeling)
      - [Linear Model \#1](#linear-model-1)
      - [Linear Model \#2](#linear-model-2)
      - [Random Forest Model](#random-forest-model)
      - [Boosted Tree Model](#boosted-tree-model)
      - [Comparison](#comparison)

# Introduction

…Placeholder for introduction…

# Data Preparation

``` r
# read data
news_data <- read_csv("./OnlineNewsPopularity.csv")

# data cleaning
world_news_data <- news_data %>% 
                   # subset data by data channel
                   filter(data_channel_is_world == 1) %>%
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
  geom_histogram(bins=50) +
  labs(title="Histogram of shares")
```

![](WorldAnalysis_files/figure-gfm/Joey%20EDA%20shares-1.png)<!-- -->

From the histogram We can see that the distribution is heavily right
skewed. The histogram scale may be influenced by potential outliers, so
we can look at the distribution of `shares` under 10,000.

``` r
# Histogram of shares < 5000
ggplot(filter(world_news_data, shares < 5000), aes(x=shares)) +
  geom_histogram(bins=50) +
  labs(title="Histogram of shares under 5000")
```

![](WorldAnalysis_files/figure-gfm/Joey%20EDA%20shares%20le%205000-1.png)<!-- -->

As we can see, the distribution is still heavily right skewed. So we may
want to look at the distribution of `log(shares)`.

``` r
# Histogram of log(shares) 
ggplot(world_news_data, aes(x=log(shares))) +
  geom_histogram(bins=50) +
  labs(title="Histogram of log(shares)")
```

![](WorldAnalysis_files/figure-gfm/Joey%20EDA%20log%20shares-1.png)<!-- -->

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
                                                Max = max(shares))

knitr::kable(shares_summary, digits=0, caption = "Numeric Summary of Shares")
```

| Min. |  Q1 | Median | Mean |   SD |   Q3 |    Max |
| ---: | --: | -----: | ---: | ---: | ---: | -----: |
|   35 | 827 |   1100 | 2288 | 6090 | 1900 | 284700 |

Numeric Summary of Shares

``` r
# Numeric summary of log(shares)
log_shares_summary <- world_news_data %>% summarise(Min. = min(log(shares)),
                                                Q1 = quantile(log(shares), 0.25),
                                                Median = median(log(shares)),
                                                Mean = mean(log(shares)),
                                                SD = sd(log(shares)),
                                                Q3 = quantile(log(shares), 0.75),
                                                Max = max(log(shares)))

knitr::kable(log_shares_summary, digits=3, caption="Numeric Summary of log(Shares)")
```

|  Min. |    Q1 | Median |  Mean |   SD |   Q3 |    Max |
| ----: | ----: | -----: | ----: | ---: | ---: | -----: |
| 3.555 | 6.718 |  7.003 | 7.203 | 0.83 | 7.55 | 12.559 |

Numeric Summary of log(Shares)

In addition to the histograms, we can see that the coefficient of
variation (CV) is much larger in `shares` compared to `log(shares)`.
This can make `shares` harder to predict. So we will continue to do EDA
using `log(shares)` and will fit the models on `log(shares)` since that
can also help reduce the impact of some extreme values of `shares`.

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

![](WorldAnalysis_files/figure-gfm/John%20EDA1-1.png)<!-- -->

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
ggplot(world_news_data, aes(x = as.factor(num_keywords), y = log(shares))) +
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
ggplot(world_news_data, aes(x = num_imgs, y = log(shares))) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title="Scatterplot of Number of Images vs log(shares)")
```

![](WorldAnalysis_files/figure-gfm/Joey%20EDA%20img%20video-1.png)<!-- -->

``` r
# Scatterplot of num_videos and log(shares)
ggplot(world_news_data, aes(x = num_videos, y = log(shares))) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title="Scatterplot of Number of Videos vs log(shares)")
```

![](WorldAnalysis_files/figure-gfm/Joey%20EDA%20img%20video-2.png)<!-- -->

We can also examine the correlation coefficient (r) to observe the
strength of the association. For absolute values of r, we can considered
0-0.19 as very weak, 0.2-0.39 as weak, 0.4-0.59 as moderate, 0.6-0.79 as
strong and 0.8-1 as very strong correlation.

``` r
# Correlation Coefficient (r) of num_imgs vs log(shares) 
cor(x = world_news_data$num_imgs, y = log(world_news_data$shares))
```

    ## [1] 0.1304567

``` r
# Correlation Coefficient (r) of num_videos vs log(shares) 
cor(x = world_news_data$num_videos, y = log(world_news_data$shares))
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
cor1 <- world_news_data %>% select(contains("tokens"), contains("words"))
cormatrix <- round(cor(cor1), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](WorldAnalysis_files/figure-gfm/John%20EDA7-1.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of keywords 
cor2 <- world_news_data %>% select(contains("keywords"), contains("kw"))
cormatrix <- round(cor(cor2), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](WorldAnalysis_files/figure-gfm/John%20EDA7-2.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of "positve" and
# "negative" connotation 
cor3 <- world_news_data %>% select(contains("positive"), contains("negative"))
cormatrix <- round(cor(cor3), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](WorldAnalysis_files/figure-gfm/John%20EDA7-3.png)<!-- -->

``` r
# Create a correlogram for predictors that measure attributes of article titles
cor4 <- world_news_data %>% select(contains("title"))
cormatrix <- round(cor(cor4), 2)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

![](WorldAnalysis_files/figure-gfm/John%20EDA7-4.png)<!-- -->

# Modeling

``` r
set.seed(31415)

partition <- createDataPartition(y = world_news_data$shares, 
                                 p= 0.7, 
                                 list = FALSE)
world_news_train <- world_news_data[partition,]
world_news_test <- world_news_data[-partition,]
```

…Placeholder for description of Linear model…

## Linear Model \#1

``` r
lm1Fit <- lm(log(shares) ~ n_tokens_title +
                     n_non_stop_words + 
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
    ## lm(formula = log(shares) ~ n_tokens_title + n_non_stop_words + 
    ##     num_hrefs + num_imgs + num_videos + average_token_length + 
    ##     kw_min_min + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_avg_sharess + 
    ##     weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
    ##     weekday_is_thursday + weekday_is_friday + LDA_00 + LDA_02 + 
    ##     global_subjectivity + title_sentiment_polarity + I(n_tokens_title^2) + 
    ##     I(num_videos^2) + I(average_token_length^2) + I(kw_min_avg^2) + 
    ##     I(kw_avg_avg^2) + I(self_reference_avg_sharess^2) + I(title_sentiment_polarity^2) + 
    ##     num_hrefs:num_imgs + kw_min_min:weekday_is_tuesday + n_tokens_title:num_imgs, 
    ##     data = world_news_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4534 -0.4405 -0.1163  0.3011  5.0505 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      7.328e+00  2.176e-01  33.681  < 2e-16 ***
    ## n_tokens_title                  -6.580e-02  3.474e-02  -1.894 0.058300 .  
    ## n_non_stop_words                 1.365e+01  2.703e+00   5.049 4.57e-07 ***
    ## num_hrefs                        9.835e-03  1.366e-03   7.198 6.88e-13 ***
    ## num_imgs                         5.343e-02  1.126e-02   4.745 2.13e-06 ***
    ## num_videos                       3.690e-02  9.923e-03   3.718 0.000202 ***
    ## average_token_length            -5.539e+00  1.118e+00  -4.953 7.51e-07 ***
    ## kw_min_min                       1.452e-03  2.080e-04   6.981 3.27e-12 ***
    ## kw_min_avg                      -2.566e-04  4.441e-05  -5.778 7.96e-09 ***
    ## kw_max_avg                      -2.003e-05  7.118e-06  -2.814 0.004903 ** 
    ## kw_avg_avg                       2.836e-04  3.351e-05   8.464  < 2e-16 ***
    ## self_reference_avg_sharess       7.922e-06  1.234e-06   6.418 1.49e-10 ***
    ## weekday_is_monday               -2.396e-01  3.804e-02  -6.299 3.22e-10 ***
    ## weekday_is_tuesday              -2.655e-01  3.774e-02  -7.034 2.23e-12 ***
    ## weekday_is_wednesday            -3.059e-01  3.704e-02  -8.259  < 2e-16 ***
    ## weekday_is_thursday             -2.706e-01  3.696e-02  -7.320 2.82e-13 ***
    ## weekday_is_friday               -2.364e-01  3.870e-02  -6.107 1.08e-09 ***
    ## LDA_00                           3.098e-01  1.109e-01   2.794 0.005219 ** 
    ## LDA_02                          -1.989e-01  5.601e-02  -3.551 0.000387 ***
    ## global_subjectivity              4.362e-01  1.314e-01   3.319 0.000910 ***
    ## title_sentiment_polarity         1.797e-01  4.341e-02   4.141 3.50e-05 ***
    ## I(n_tokens_title^2)              4.027e-03  1.580e-03   2.548 0.010856 *  
    ## I(num_videos^2)                 -1.117e-03  3.893e-04  -2.870 0.004123 ** 
    ## I(average_token_length^2)        5.417e-01  1.157e-01   4.681 2.91e-06 ***
    ## I(kw_min_avg^2)                  1.024e-07  2.200e-08   4.654 3.32e-06 ***
    ## I(kw_avg_avg^2)                 -1.045e-08  3.852e-09  -2.712 0.006713 ** 
    ## I(self_reference_avg_sharess^2) -1.394e-11  2.449e-12  -5.693 1.31e-08 ***
    ## I(title_sentiment_polarity^2)    1.498e-01  7.145e-02   2.096 0.036124 *  
    ## num_hrefs:num_imgs              -3.094e-04  9.603e-05  -3.221 0.001283 ** 
    ## kw_min_min:weekday_is_tuesday   -1.005e-03  4.388e-04  -2.291 0.021979 *  
    ## n_tokens_title:num_imgs         -3.376e-03  1.027e-03  -3.285 0.001024 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7781 on 5869 degrees of freedom
    ## Multiple R-squared:  0.1195, Adjusted R-squared:  0.115 
    ## F-statistic: 26.55 on 30 and 5869 DF,  p-value: < 2.2e-16

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

…Placeholder for description of Random Forest model…

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

…Placeholder for description of Boosted Tree Model…

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

## Comparison

``` r
prediction <- predict(lm1Fit, newdata = world_news_train)
lm1 <- round(postResample(prediction, log(world_news_train$shares)), 3)

prediction <- predict(lm2Fit, newdata = world_news_train)
lm2 <- round(postResample(prediction, log(world_news_train$shares)), 3)

prediction <- predict(rfFit, newdata = world_news_train)
rf <- round(postResample(prediction, log(world_news_train$shares)), 3)

prediction <- predict(boostedFit, newdata = world_news_train)
boost <- round(postResample(prediction, log(world_news_train$shares)), 3)

compareFits <- data.frame(lm1, lm2, rf, boost)
names(compareFits) <- c("Linear Model 1", 
                        "Linear Model 2",
                        "Random Forest Model",
                        "Boosted Tree Model")

compareFitsLong <- data.frame(t(compareFits))

knitr::kable(compareFitsLong)
```

|                     |  RMSE | Rsquared |   MAE |
| :------------------ | ----: | -------: | ----: |
| Linear Model 1      | 0.776 |    0.120 | 0.546 |
| Linear Model 2      | 0.798 |    0.070 | 0.567 |
| Random Forest Model | 0.333 |    0.951 | 0.229 |
| Boosted Tree Model  | 0.741 |    0.207 | 0.525 |

We will choose the model with the lowest RMSE.

``` r
# Sort the compareFitsLong table by ascending RMSE and pick the first row as the model
modelChosen <- compareFitsLong %>% arrange(RMSE) %>% filter(row_number()==1) %>% row.names()

# Based on the model name, fit the corresponding model to the test data
if (modelChosen == "Linear Model 1"){
    prediction <- predict(lm1Fit, newdata=world_news_test)
   }else if (modelChosen == "Linear Model 2"){
      prediction <- predict(lm2Fit, newdata=world_news_test)
   }else if (modelChosen == "Random Forest Model"){
      prediction <- predict(rfFit, newdata=world_news_test)
   }else if (modelChosen == "Boosted Tree Model"){
      prediction <- predict(boostedFit, newdata=world_news_test)
   }

# Fit results of the final chosen model 
finalModel <- data.frame(round(postResample(prediction, log(world_news_test$shares)), 3))
```

Our final model is the Random Forest Model. When we fit the model on the
test data we get RMSE=0.775, Rsquared=0.146, and MAE=0.545.
