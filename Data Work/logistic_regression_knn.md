---
title: "Logistic Regression"
author: "Daniel Fuller"
date: "2024-09-23"
output:
      html_document:
        keep_md: true
---


``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(tidymodels)
library(sjPlot)
library(finalfit)
library(knitr)
library(gtsummary)
library(mlbench)
library(vip)
library(rsample)
library(tune)
library(recipes)
library(yardstick)
library(parsnip)
library(glmnet)
library(themis)
library(kknn)
library(microbenchmark)
```

# Logistic Regression - The Machine Learning Way

## Research question and data

We are using an imputed (ie. no missing data) version of the CanPath student dataset [https://canpath.ca/student-dataset/](https://canpath.ca/student-dataset/). The nice thing about this dataset is that it's pretty big in terms of sample size, has lots of variables, and we can use it for free. 

Our research question is:  

- **Can we develop a model that will predict type 2 diabetes**

### Reading in data

Here are reading in data and getting organized to run our models. 


``` r
data <- read_csv("mice_all_imp.csv")
```

```
## Rows: 41187 Columns: 93
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (1): ID
## dbl (92): ADM_STUDY_ID, SDC_GENDER, SDC_AGE_CALC, SDC_MARITAL_STATUS, SDC_ED...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
data <- data %>% mutate_at(3, factor)
data <- data %>% mutate_at(5:6, factor)
data <- data %>% mutate_at(8:12, factor)
data <- data %>% mutate_at(15:81, factor)
data <- data %>% mutate_at(83:93, factor)

table(data$DIS_DIAB_EVER)
```

```
## 
##     0     1     2 
## 36714  3114  1359
```

``` r
data <- data %>%
	mutate(diabetes = case_when(
		DIS_DIAB_EVER == 0 ~ 0,
		DIS_DIAB_EVER == 1 ~ 1,
		DIS_DIAB_EVER == 2 ~ 0)) %>%
		mutate(diabetes = as.factor(diabetes))

table(data$DIS_DIAB_EVER, data$diabetes)
```

```
##    
##         0     1
##   0 36714     0
##   1     0  3114
##   2  1359     0
```

``` r
data$DIS_DIAB_EVER <- NULL
```

#### Simplifying the data

Here I'm simplifying the data because the models take a long time to run and for teaching purposes we need models that run in a reasonable amount of time. 


``` r
data_small <- select(data, diabetes, SDC_AGE_CALC, SDC_EDU_LEVEL, PM_BMI_SR, HS_GEN_HEALTH, WRK_FULL_TIME, SMK_CIG_EVER, SDC_INCOME, PA_TOTAL_SHORT, HS_ROUTINE_VISIT_EVER, PSE_ADULT_WRK_DURATION, DIS_RESP_SLEEP_APNEA_EVER, SDC_EDU_LEVEL_AGE, SDC_GENDER)
```

## Machine Learning - Logistic Regression 

In a machine learning approach, in general, our interest is less on the specific associations we see between individual variables and the outcome and more on the overall performance of the model in terms of predicting the outcome. You might remember this like AIC, BIC, or -2Log-Likelihood, or Pseudo-R2 for model fit in logistic regression. 

### Resampling - Data Split approach (Part 1)

More machine learning we need a way to split the data into a training set and a test set. There are a few different approaches too this. Here we are going to use an 70/30 split with 70% of the data going to training and 30 going to testing. This is sort of an older way to split data and I would say that a k-fold cross validation is probably more in line with modern practice. We will test this out later.  


``` r
# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(10)

data_split <- initial_split(data_small, prop = 0.70, strata = diabetes)

# Create data frames for the two sets:
train_data <- training(data_split)
table(train_data$diabetes)
```

```
## 
##     0     1 
## 26640  2190
```

``` r
test_data  <- testing(data_split)
table(test_data$diabetes)
```

```
## 
##     0     1 
## 11433   924
```

Now we have split the data, we want to create the model for the training data and save it so it can be applied to the testing set. This is basically exactly what we did before. __Note that we only run the model on the training data__ Not all of the data like would in a traditional logistic regression. Here we won't get the exact same result as our original logistic regression because we don't have the same data. We expect there will be some variation but that the results should relatively similar. 

**Another note. I've added variables to this model compared to our previous model. The previous model did a very poor job of predicting diabetes overall. In fact, it had a sensitivity of ZERO! Meaning it did not predict a single case of diabetes in the test set. That's bad so I've added variables to try and increase our prediction ability. This is a key difference in typical etiologic epidemiology versus machine learning focused analyses. 

### Running the regression


``` r
table(data$diabetes)
```

```
## 
##     0     1 
## 38073  3114
```

``` r
logistic_model <- logistic_reg() %>%
        set_engine("glm") %>%
        set_mode("classification") %>% 
        fit(diabetes ~ ., data = train_data)
```

### Test the trained model

Once we `train the model` we want to understand how well our trained model works on new data the model has not seen. This is where the testing data comes in. We can use the `predict` feature for this. What we are doing here is predicting if someone has diabetes (yes/no) from the model we trained using the training data, on the testing data. We had 4293 observations in the training with 4077 people with on diabetes and 216 people with diabetes. Much of this example comes from [https://medium.com/the-researchers-guide/modelling-binary-logistic-regression-using-tidymodels-library-in-r-part-1-c1bdce0ac055](https://medium.com/the-researchers-guide/modelling-binary-logistic-regression-using-tidymodels-library-in-r-part-1-c1bdce0ac055)

The code below outputs the predict class `diabetes (yes/no)` for the test data. 


``` r
pred_class <- predict(logistic_model,
                      new_data = test_data,
                      type = "class")
table(pred_class$.pred_class)
```

```
## 
##     0     1 
## 12348     9
```

``` r
table(test_data$diabetes)
```

```
## 
##     0     1 
## 11433   924
```

Our model predicts that we have 4206 people with diabetes and 4 people with diabetes. Not looking good for our model! 

Now we want to generated the predicted probabilities for the model. That is, how well does our model think it does for each person. 


``` r
pred_prob <- predict(logistic_model,
                      new_data = test_data,
                      type = "prob")
head(pred_prob)
```

```
## # A tibble: 6 × 2
##   .pred_0 .pred_1
##     <dbl>   <dbl>
## 1   0.975  0.0246
## 2   0.928  0.0715
## 3   0.979  0.0206
## 4   0.936  0.0638
## 5   0.972  0.0281
## 6   0.774  0.226
```

This is not very informative in terms of results but we will discuss this more later. 

Now we want to combine all of our results into one dataframe and just do a quick check. 


``` r
diabetes_results <- test_data %>%
  select(diabetes) %>%
  bind_cols(pred_class, pred_prob)

head(diabetes_results)
```

```
## # A tibble: 6 × 4
##   diabetes .pred_class .pred_0 .pred_1
##   <fct>    <fct>         <dbl>   <dbl>
## 1 0        0             0.975  0.0246
## 2 0        0             0.928  0.0715
## 3 0        0             0.979  0.0206
## 4 0        0             0.936  0.0638
## 5 0        0             0.972  0.0281
## 6 0        0             0.774  0.226
```

Here we can see the first 6 rows of data data all negative for diabetes and are predicted as negative. The model is very confident in these predictions, with over 90% negative prediction in all six observations. 

### Model evaluation

There are a number of different methods we must use to evaluate machine learning models. We will walk through those. 

#### Confusion Matrix

We can generate a confusion matrix by using the `conf_mat()` function by supplying the final data frame (`diabetes_results`), the truth column `diabetes` and predicted class `.pred_class` in the estimate attribute.

A confusion matrix is sort of a 2x2 table with the true values on one side and predicted values in another column. If we look on the diagonal we see when the model correctly predicts the values `yes/no` and off diagonal is when the model does not predict the correct value. So this model correctly predicts that 4075 cases of diabetes and incorrectly predicts that 212 people do not have diabetes when they do have it. The model correctly predicts 4 cases of diabetes. It also incorrectly predicts that two people who do not have diabetes do have diabetes. 


``` r
conf_mat(diabetes_results, truth = diabetes,
         estimate = .pred_class)
```

```
##           Truth
## Prediction     0     1
##          0 11428   920
##          1     5     4
```

#### Accuracy

We can calculate the classification accuracy by using the `accuracy()` function by supplying the final data frame `diabetes_results`, the truth column `diabetes` and predicted class `.pred_class` in the estimate attribute. The model classification accuracy on test dataset is about ~94%. This looks good but it's a bit of fake result as we will see later. 


``` r
accuracy(diabetes_results, truth = diabetes,
         estimate = .pred_class)
```

```
## # A tibble: 1 × 3
##   .metric  .estimator .estimate
##   <chr>    <chr>          <dbl>
## 1 accuracy binary         0.925
```

#### Sensitivity

The sensitivity (also known as __Recall__) of a classifier is the ratio between what was correctly identified as positive (True Positives) and all positives (False Negative + True Positive).

__Sensitivity = TP / FN + TP__

The sensitivity value is 1.0 indicating that we are able to correctly detect 100% of the positive values. 


``` r
sens(diabetes_results, truth = diabetes,
    estimate = .pred_class)
```

```
## # A tibble: 1 × 3
##   .metric .estimator .estimate
##   <chr>   <chr>          <dbl>
## 1 sens    binary         1.000
```

#### Specificity

Specificity of a classifier is the ratio between what was classified as negative (True Negatives) and all negative values (False Positive + True Native)

__Specificity = TN / FP + TN__

The specificity value is 0.004. Meaning that we correctly classify 0.4% of the negative values, which is pretty terrible. 


``` r
spec(diabetes_results, truth = diabetes,
    estimate = .pred_class)
```

```
## # A tibble: 1 × 3
##   .metric .estimator .estimate
##   <chr>   <chr>          <dbl>
## 1 spec    binary       0.00433
```

#### Precision

What percent of values are correctly classified as positive (True Positives) out of all positives (True Positive + False Positive)?

__Precision = TP / TP + FP__

The precision is 0.94, meaning we identify 81.8% of true positives compared to all positives. 


``` r
precision(diabetes_results, truth = diabetes,
    estimate = .pred_class)
```

```
## # A tibble: 1 × 3
##   .metric   .estimator .estimate
##   <chr>     <chr>          <dbl>
## 1 precision binary         0.925
```

#### F-Score

F-score is the mean of precision and sensitivity. The value ranges from 1 (the best score) and 0 (the worst score). F-score gives us the balance between precision and sensitivity. The F1 score is about 0.97, which indicates that the trained model has a classification strength of 97%.


``` r
f_meas(diabetes_results, truth = diabetes,
       estimate = .pred_class)
```

```
## # A tibble: 1 × 3
##   .metric .estimator .estimate
##   <chr>   <chr>          <dbl>
## 1 f_meas  binary         0.961
```

#### ROC Curve

The ROC curve is plotted with `sensitivity` against `1 - Specificity`, where `sensitivity` is on the y-axis and `1 - Specificity` is on the x-axis. A line is drawn diagonally to denote 50–50 partitioning of the graph. If the curve is more close to the line, lower the performance of the classifier, which is no better than a mere random guess.

You can generate a ROC Curve using the `roc_curve()` function where you need to supply the truth column `diabetes` and predicted probabilities for the positive class `.pred_pos`.

Our model has got a ROC-AUC score of 0.227 indicating a good model that cannot distinguish between patients with diabetes and no diabetes.


``` r
roc_auc(diabetes_results,
        truth = diabetes,
        .pred_0)
```

```
## # A tibble: 1 × 3
##   .metric .estimator .estimate
##   <chr>   <chr>          <dbl>
## 1 roc_auc binary         0.719
```

``` r
roc_curve <- diabetes_results %>%
  roc_curve(truth = diabetes, .pred_0) %>%
  autoplot()

plot(roc_curve)
```

![](logistic_regression_knn_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

#### All the metrics 

We can produce all of the metrics using the `metric_set` function. 


``` r
metrics <- metric_set(accuracy, sens, spec, precision, recall, f_meas)

all_metrics_lr <- metrics(diabetes_results,
               truth = diabetes,
               estimate = .pred_class)
               
kable(all_metrics_lr)
```



|.metric   |.estimator | .estimate|
|:---------|:----------|---------:|
|accuracy  |binary     | 0.9251436|
|sens      |binary     | 0.9995627|
|spec      |binary     | 0.0043290|
|precision |binary     | 0.9254940|
|recall    |binary     | 0.9995627|
|f_meas    |binary     | 0.9611034|

#### Feature Importance

Feature importance is the one way that ML models examine which variables are important to the predictions overall. It's not super common to see, except for people like Epi folks who think about specific associations between variables. 


``` r
coeff <- tidy(logistic_model) %>% 
  arrange(desc(abs(estimate))) %>% 
  filter(abs(estimate) > 0.5)

kable(coeff)
```



|term           |  estimate| std.error|  statistic| p.value|
|:--------------|---------:|---------:|----------:|-------:|
|(Intercept)    | -3.638060| 0.5831930|  -6.238175|       0|
|HS_GEN_HEALTH5 | -1.932474| 0.1354990| -14.261910|       0|
|HS_GEN_HEALTH4 | -1.672357| 0.1154920| -14.480284|       0|
|HS_GEN_HEALTH3 | -0.781757| 0.1101688|  -7.095992|       0|

#### Plot of feature importance


``` r
ggplot(coeff, aes(x = term, y = estimate, fill = term)) + geom_col() + coord_flip()
```

![](logistic_regression_knn_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

#### Model interpretation

So now we have to interpret the model. General guidelines to think about the bias variance trade off and weather our model performs well. Based on the evaluation metrics how do we fell about this model? 
Typically in ML types of problems a model with less than 80-90% accuracy is consider ok, but it depends a bit on the problem. Our model has an accuracy of 95%... maybe that's good. HOWEVER, when we look at the sensitivity it's 1 and the specificity is 0.4%. A sensitivity of 1 (perfect) is suspect and our specificity is very very bad.

Overall, this model is not very good. We don't have a sufficient number of features (variables) to do a good job with prediction. We have a high bias, our model underfits the data. The variance is also high. 

### Up-Sampling

OK. So our model is terrible. There are a number of reasons for this that we are going to explore now. These are standard machine learning explorations that we would normally do as part of machine learning analysis workflow. First, we are going to explore up-sampling. One of the main problems with the diabetes data we have is the prevalence of diabetes is relatively low in the dataset. This is good normally in biostatistics approaches as we want the OR to approximate the RR in a case control study. BUT that's terrible for prediction. 

One thing that we can do is up-scale the lowest class (or classes) or the outcome variable. There are a bunch of different methods to do this and we are using the `themis` package [https://themis.tidymodels.org/reference/index.html](https://themis.tidymodels.org/reference/index.html). Here we are using the `step_upsample()` function. We only want to use the up scaling methods on the training set. We don't use it on the test set because that would create a false model performance. 

#### Up-sampling Example


``` r
### Showing the distribution of diabetes (yes/no) in the real data
table(train_data$diabetes)
```

```
## 
##     0     1 
## 26640  2190
```

``` r
ggplot(train_data) + 
    geom_bar(aes(diabetes))
```

![](logistic_regression_knn_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

``` r
### Creating a recipe were we upsample the diabetes yes category to be 50% of the diabetes no category. This is an arbitrary number and you will need to play with this.

diabetes_rec_oversamp <- 
  recipe(diabetes ~ ., data = train_data) %>%
  step_smotenc(diabetes, over_ratio = 0.9) %>%
  step_unknown() %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% ### Mean center and standardize (z-score) the numeric predictors
  step_zv(all_predictors()) 

### Visualization of the 30% ratio
recipe(~., train_data) %>%
  step_upsample(diabetes, over_ratio = 0.9) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  ggplot(aes(diabetes)) +
  geom_bar()
```

![](logistic_regression_knn_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

Here we upscale the `diabetes-Yes` category to 50% of the of the `diabetes-No` category. The figures show the differences but we go from 1514 cases of diabetes in the training set to over ~12000 cases of diabetes. 

**Up-scaling regression**

Setup the recipe and metrics. Here we are specifying the model we want to use. 


``` r
logistic_m <- logistic_reg(
                mode = "classification",
                engine = "glm"
              )
```

Logistic regression results based on up-scaling. Here we setup the workflow. A workflow must includ the following

* A Recipe `add_recipe` which is how we tell the workflow to process the data. 
* A Model `add_model` which specifies the model paramaters

Once we save the workflow we can run the same model in different ways. More on this later. 


``` r
diabetes_wflow_oversamp <- 
  workflow() %>% 
  add_model(logistic_m) %>% 
  add_recipe(diabetes_rec_oversamp)

diabetes_wflow_oversamp
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: logistic_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 5 Recipe Steps
## 
## • step_smotenc()
## • step_unknown()
## • step_dummy()
## • step_normalize()
## • step_zv()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

Now we will actually fit the model to the data using the recipe and `fit` command. 


``` r
diabetes_fit_oversamp <- 
  diabetes_wflow_oversamp %>% 
  fit(data = train_data)
```


``` r
diabetes_fit_oversamp %>% 
  extract_fit_parsnip() %>% 
  tidy()
```

```
## # A tibble: 30 × 5
##    term                   estimate std.error statistic   p.value
##    <chr>                     <dbl>     <dbl>     <dbl>     <dbl>
##  1 (Intercept)             -0.137    0.00993    -13.8  4.63e- 43
##  2 SDC_AGE_CALC             0.384    0.0118      32.4  6.86e-231
##  3 PM_BMI_SR                0.183    0.0103      17.8  6.42e- 71
##  4 PA_TOTAL_SHORT          -0.0869   0.0102      -8.56 1.11e- 17
##  5 PSE_ADULT_WRK_DURATION   0.0643   0.0112       5.72 1.05e-  8
##  6 SDC_EDU_LEVEL_AGE        0.0265   0.0112       2.36 1.83e-  2
##  7 SDC_EDU_LEVEL_X1         0.0899   0.0368       2.44 1.45e-  2
##  8 SDC_EDU_LEVEL_X2         0.524    0.129        4.06 4.93e-  5
##  9 SDC_EDU_LEVEL_X3         0.328    0.0900       3.65 2.61e-  4
## 10 SDC_EDU_LEVEL_X4         0.603    0.138        4.36 1.31e-  5
## # ℹ 20 more rows
```

``` r
diabetes_aug_oversamp <- 
  augment(diabetes_fit_oversamp, test_data)

diabetes_fit_oversamp_all_metrics <- metrics(diabetes_aug_oversamp,
               truth = diabetes,
               estimate = .pred_class)
               
kable(diabetes_fit_oversamp_all_metrics)
```



|.metric   |.estimator | .estimate|
|:---------|:----------|---------:|
|accuracy  |binary     | 0.7068868|
|sens      |binary     | 0.7173095|
|spec      |binary     | 0.5779221|
|precision |binary     | 0.9546037|
|recall    |binary     | 0.7173095|
|f_meas    |binary     | 0.8191171|

Here we have dramatically improved our specificity from 0.004 to 0.28. Overall, our accuracy and other metrics have gone down... which is good. This model is much better and less suspect than our previous model. Our up-sampling has done well here. We could test more up-sampling but you get the idea here. 


``` r
kable(all_metrics_lr)
```



|.metric   |.estimator | .estimate|
|:---------|:----------|---------:|
|accuracy  |binary     | 0.9251436|
|sens      |binary     | 0.9995627|
|spec      |binary     | 0.0043290|
|precision |binary     | 0.9254940|
|recall    |binary     | 0.9995627|
|f_meas    |binary     | 0.9611034|

# K-Nearest Neighbour


``` r
knn_m <- nearest_neighbor(neighbors = tune()) |>
                set_mode("classification") |> 
                set_engine("kknn")

k_tuning <- grid_regular(
              neighbors(range = c(2,10)
              ))

ctrl <- control_resamples(save_pred = TRUE)

cross_validation <- vfold_cv(train_data, v = 5)
```

* A Recipe `add_recipe` which is how we tell the workflow to process the data. 
* A Model `add_model` which specifies the model paramaters

Once we save the workflow we can run the same model in different ways. More on this later. 


``` r
diabetes_wflow_oversamp_knn <- 
  workflow() %>% 
  add_model(knn_m) %>% 
  add_recipe(diabetes_rec_oversamp)

diabetes_wflow_oversamp_knn
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: nearest_neighbor()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 5 Recipe Steps
## 
## • step_smotenc()
## • step_unknown()
## • step_dummy()
## • step_normalize()
## • step_zv()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## K-Nearest Neighbor Model Specification (classification)
## 
## Main Arguments:
##   neighbors = tune()
## 
## Computational engine: kknn
```

``` r
knn_tune <- tune_grid(
              diabetes_wflow_oversamp_knn, 
              resamples = cross_validation,
              grid = k_tuning, 
              control = ctrl
)

knn_tune |> collect_metrics()
```

```
## # A tibble: 9 × 7
##   neighbors .metric     .estimator  mean     n std_err .config        
##       <int> <chr>       <chr>      <dbl> <int>   <dbl> <chr>          
## 1         2 accuracy    binary     0.794     5 0.00136 pre0_mod1_post0
## 2         2 brier_class binary     0.192     5 0.00133 pre0_mod1_post0
## 3         2 roc_auc     binary     0.579     5 0.00332 pre0_mod1_post0
## 4         6 accuracy    binary     0.771     5 0.00234 pre0_mod2_post0
## 5         6 brier_class binary     0.179     5 0.00155 pre0_mod2_post0
## 6         6 roc_auc     binary     0.612     5 0.00239 pre0_mod2_post0
## 7        10 accuracy    binary     0.764     5 0.00242 pre0_mod3_post0
## 8        10 brier_class binary     0.182     5 0.00153 pre0_mod3_post0
## 9        10 roc_auc     binary     0.624     5 0.00230 pre0_mod3_post0
```

``` r
best_neighbors <- knn_tune |>
                    select_best(metric = "accuracy") |> 
                    pull(neighbors)


knn_best_train <- diabetes_wflow_oversamp_knn |>
                    finalize_workflow(
                      select_best(x = knn_tune, metric = "accuracy")
                    )

knn_best_train
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: nearest_neighbor()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 5 Recipe Steps
## 
## • step_smotenc()
## • step_unknown()
## • step_dummy()
## • step_normalize()
## • step_zv()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## K-Nearest Neighbor Model Specification (classification)
## 
## Main Arguments:
##   neighbors = 2
## 
## Computational engine: kknn
```

``` r
knn_best_train_fit <- knn_best_train |> last_fit(data_split)

knn_best_train_fit |> collect_metrics()
```

```
## # A tibble: 3 × 4
##   .metric     .estimator .estimate .config        
##   <chr>       <chr>          <dbl> <chr>          
## 1 accuracy    binary         0.808 pre0_mod0_post0
## 2 roc_auc     binary         0.600 pre0_mod0_post0
## 3 brier_class binary         0.181 pre0_mod0_post0
```

## Session Info


``` r
sessionInfo()
```

```
## R version 4.5.1 (2025-06-13)
## Platform: aarch64-apple-darwin20
## Running under: macOS Tahoe 26.2
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRblas.0.dylib 
## LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## time zone: America/Regina
## tzcode source: internal
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] microbenchmark_1.5.0 kknn_1.4.1           themis_1.0.3        
##  [4] glmnet_4.1-10        Matrix_1.7-3         vip_0.4.1           
##  [7] mlbench_2.1-6        gtsummary_2.4.0      knitr_1.50          
## [10] finalfit_1.1.0       sjPlot_2.9.0         yardstick_1.3.2     
## [13] workflowsets_1.1.1   workflows_1.3.0      tune_2.0.1          
## [16] tailor_0.1.0         rsample_1.3.1        recipes_1.3.1       
## [19] parsnip_1.3.3        modeldata_1.5.1      infer_1.0.9         
## [22] dials_1.4.2          scales_1.4.0         broom_1.0.10        
## [25] tidymodels_1.4.1     lubridate_1.9.4      forcats_1.0.1       
## [28] stringr_1.6.0        dplyr_1.1.4          purrr_1.2.0         
## [31] readr_2.1.6          tidyr_1.3.1          tibble_3.3.0        
## [34] ggplot2_4.0.1        tidyverse_2.0.0     
## 
## loaded via a namespace (and not attached):
##  [1] Rdpack_2.6.4        rlang_1.1.6         magrittr_2.0.4     
##  [4] furrr_0.3.1         compiler_4.5.1      vctrs_0.6.5        
##  [7] lhs_1.2.0           crayon_1.5.3        pkgconfig_2.0.3    
## [10] shape_1.4.6.1       fastmap_1.2.0       backports_1.5.0    
## [13] labeling_0.4.3      utf8_1.2.6          rmarkdown_2.30     
## [16] prodlim_2025.04.28  tzdb_0.5.0          nloptr_2.2.1       
## [19] bit_4.6.0           xfun_0.54           jomo_2.7-6         
## [22] cachem_1.1.0        jsonlite_2.0.0      pan_1.9            
## [25] parallel_4.5.1      R6_2.6.1            bslib_0.9.0        
## [28] stringi_1.8.7       RColorBrewer_1.1-3  parallelly_1.45.1  
## [31] boot_1.3-31         rpart_4.1.24        jquerylib_0.1.4    
## [34] Rcpp_1.1.0          iterators_1.0.14    future.apply_1.20.0
## [37] igraph_2.2.1        splines_4.5.1       nnet_7.3-20        
## [40] timechange_0.3.0    tidyselect_1.2.1    rstudioapi_0.17.1  
## [43] yaml_2.3.10         timeDate_4051.111   codetools_0.2-20   
## [46] listenv_0.10.0      lattice_0.22-7      withr_3.0.2        
## [49] S7_0.2.1            evaluate_1.0.5      future_1.68.0      
## [52] survival_3.8-3      pillar_1.11.1       mice_3.18.0        
## [55] foreach_1.5.2       reformulas_0.4.2    generics_0.1.4     
## [58] vroom_1.6.6         hms_1.1.4           minqa_1.2.8        
## [61] globals_0.18.0      class_7.3-23        glue_1.8.0         
## [64] ROSE_0.0-4          tools_4.5.1         data.table_1.17.8  
## [67] lme4_1.1-37         gower_1.0.2         grid_4.5.1         
## [70] rbibutils_2.4       ipred_0.9-15        nlme_3.1-168       
## [73] cli_3.6.5           DiceDesign_1.10     lava_1.8.2         
## [76] gtable_0.3.6        GPfit_1.0-9         sass_0.4.10        
## [79] digest_0.6.39       farver_2.1.2        htmltools_0.5.8.1  
## [82] lifecycle_1.0.4     hardhat_1.4.2       mitml_0.4-5        
## [85] sparsevctrs_0.3.4   bit64_4.6.0-1       MASS_7.3-65
```

