Final Exam
================
Emmanuel M. Fe Benito
2025-12-14

``` r
df <- read.csv("C:/Users/Emmanuel/Downloads/Alzheimers Mice Data.csv")

df$AD_Status <- as.factor(df$AD_Status)
df$Treatment <- as.factor(df$Treatment)

head(df)
```

    ##   AD_Status Treatment Training Memory
    ## 1         1         1       12     10
    ## 2         1         1       15     12
    ## 3         1         1       13     13
    ## 4         1         1       12     10
    ## 5         1         1       14     13
    ## 6         1         2       15     13

``` r
train_stats <- df %>%
  group_by(AD_Status, Treatment) %>%
  get_summary_stats(Training, type = "mean_sd")

print(train_stats)
```

    ## # A tibble: 8 × 6
    ##   AD_Status Treatment variable     n  mean    sd
    ##   <fct>     <fct>     <fct>    <dbl> <dbl> <dbl>
    ## 1 1         1         Training     5  13.2  1.30
    ## 2 1         2         Training     5  15.8  1.30
    ## 3 1         3         Training     5  15.2  1.92
    ## 4 1         4         Training     5  13.6  1.14
    ## 5 2         1         Training     5  15.4  1.82
    ## 6 2         2         Training     5  15.8  1.79
    ## 7 2         3         Training     5  15.2  1.92
    ## 8 2         4         Training     5  13.6  1.14

``` r
train_outliers <- df %>%
  group_by(AD_Status, Treatment) %>%
  identify_outliers(Training)

if(nrow(train_outliers) == 0) {
  print("No extreme outliers detected.")
} else {
  print(train_outliers)
}
```

    ## [1] "No extreme outliers detected."

``` r
model_train <- aov(Training ~ AD_Status * Treatment, data = df)
shapiro_test(residuals(model_train))
```

    ## # A tibble: 1 × 3
    ##   variable               statistic p.value
    ##   <chr>                      <dbl>   <dbl>
    ## 1 residuals(model_train)     0.964   0.221

``` r
leveneTest(Training ~ AD_Status * Treatment, data = df)
```

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##       Df F value Pr(>F)
    ## group  7  0.4346 0.8731
    ##       32

``` r
anova_train_results <- aov(Training ~ AD_Status * Treatment, data = df)
summary(anova_train_results)
```

    ##                     Df Sum Sq Mean Sq F value Pr(>F)  
    ## AD_Status            1   3.03   3.025   1.216 0.2784  
    ## Treatment            3  28.28   9.425   3.789 0.0197 *
    ## AD_Status:Treatment  3   9.08   3.025   1.216 0.3198  
    ## Residuals           32  79.60   2.488                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
etaSquared(anova_train_results, type = 2)
```

    ##                         eta.sq eta.sq.part
    ## AD_Status           0.02521359   0.0366112
    ## Treatment           0.23567410   0.2621089
    ## AD_Status:Treatment 0.07564076   0.1023400

Bonus

``` r
mem_stats <- df %>%
  group_by(AD_Status, Treatment) %>%
  get_summary_stats(Memory, type = "mean_sd")

print(mem_stats)
```

    ## # A tibble: 8 × 6
    ##   AD_Status Treatment variable     n  mean    sd
    ##   <fct>     <fct>     <fct>    <dbl> <dbl> <dbl>
    ## 1 1         1         Memory       5  11.6 1.52 
    ## 2 1         2         Memory       5  13.2 1.48 
    ## 3 1         3         Memory       5  12.4 2.07 
    ## 4 1         4         Memory       5  11.2 1.30 
    ## 5 2         1         Memory       5   8.6 0.894
    ## 6 2         2         Memory       5   7.6 1.95 
    ## 7 2         3         Memory       5   8.2 0.837
    ## 8 2         4         Memory       5   6.6 2.07

``` r
mem_outliers <- df %>%
  group_by(AD_Status, Treatment) %>%
  identify_outliers(Memory)

print(mem_outliers)
```

    ## # A tibble: 1 × 6
    ##   AD_Status Treatment Training Memory is.outlier is.extreme
    ##   <fct>     <fct>        <int>  <int> <lgl>      <lgl>     
    ## 1 1         2               14     11 TRUE       FALSE

``` r
model_mem <- aov(Memory ~ AD_Status * Treatment, data = df)
shapiro_test(residuals(model_mem))
```

    ## # A tibble: 1 × 3
    ##   variable             statistic p.value
    ##   <chr>                    <dbl>   <dbl>
    ## 1 residuals(model_mem)     0.967   0.282

``` r
leveneTest(Memory ~ AD_Status * Treatment, data = df)
```

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##       Df F value Pr(>F)
    ## group  7  0.8275 0.5722
    ##       32

``` r
anova_mem_results <- aov(Memory ~ AD_Status * Treatment, data = df)
summary(anova_mem_results)
```

    ##                     Df Sum Sq Mean Sq F value   Pr(>F)    
    ## AD_Status            1 189.22  189.22  75.313 6.45e-10 ***
    ## Treatment            3  14.48    4.83   1.920    0.146    
    ## AD_Status:Treatment  3   8.67    2.89   1.151    0.344    
    ## Residuals           32  80.40    2.51                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
etaSquared(anova_mem_results, type = 2)
```

    ##                         eta.sq eta.sq.part
    ## AD_Status           0.64631543  0.70180807
    ## Treatment           0.04944070  0.15256917
    ## AD_Status:Treatment 0.02963026  0.09738984
