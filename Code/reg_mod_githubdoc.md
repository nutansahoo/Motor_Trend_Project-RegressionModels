---
title: "Regression Models (Motor Trend Project)"
author: "Nutan Sahoo"
date: "21 September 2017"
output: rmarkdown::github_document
  
geometry: margin=1in
---



## Executive Summary
The purpose of this project is to explore the impact of a set of variables like horsepower, transmission configuration, engine cylinder configuration, etc. on the mileage `mpg` (Miles per Gallon). In particular we have the following objectives:---

1. To find which one of Automatic or Manual Transmission is better for `mpg`.
2. To Quantify the `mpg` difference b/w auto and manual transmissions.
3. To come up with a model to predict the mileage of a given car.
4. To find how a 1000lbs increase in car's weight will change the fuel efficiency.


Firstly, I plotted boxplots of  `mpg` against various categorical variables to visualise how `mpg` changes with changing levels and also to examine the spread and mean of the different levels of the factor. We visualise a correlation matrix using `chart.Correlation` function in `PerformanceAnalytics` Package. The variables which had high cor. with the mpg were selected as regressor variables for our regression model. Out of all possible regression models which can be made from the combination of those regressors, we select the best model using `stepAIC` function from `MASS` package on the basis on Akaike's Information Criteria, Other criteria such as adjusted R^2^, Mean Square of Residuals, Generalised Variance Inflation factors were also taken into consideration.

*****************
## Exploratory Analysis 
We plot various box plots to visualise the distribution of `mpg` by various groups of the categorical variable.^[**All plots given in appendix**] 
In plot-1 **(see appendix for all plots)** we see that the `mpg` of cars with automatic transmission is much lower than manual. We can check if the difference b/w their mean values are statistically significant through a two-samples independent t-test.



```r
aggregate(mpg~am, data = mtcars, mean)
```

```
##   am      mpg
## 1  0 17.14737
## 2  1 24.39231
```

```r
t.test(mpg~am, data=mtcars)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  mpg by am
## t = -3.7671, df = 18.332, p-value = 0.001374
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -11.280194  -3.209684
## sample estimates:
## mean in group 0 mean in group 1 
##        17.14737        24.39231
```


**p value <0.05**, we reject the null hypothesis that the true mean difference is equal to zero. Hence, the difference is statistically significant and the cars with automatic transmission have a lower `mpg` on an average. Plot-2 shows that generally, `mpg` of cars with `S` engine configuration is much higher. We can confirm this using a t-test as shown above. 


```r
t.test(mpg~vs, data= mtcars)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  mpg by vs
## t = -4.6671, df = 22.716, p-value = 0.0001098
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -11.462508  -4.418445
## sample estimates:
## mean in group 0 mean in group 1 
##        16.61667        24.55714
```

```r
##t = -4.6671, df = 22.716, p-value = 0.0001098
```

p value is 0.0001, we reject the null hypothesis that the true mean difference b/w cars with different engine configuration is 0. The difference is statistically significant. The configuration of the engine significantly affects the mileage.
By looking at plot-3 it is safe to assume, higher th number of cylinders in the car, lower is the `mpg`. Such definite conclusions cannot be drawn by looking at plot-4 & plot-5 but we can perform t-tests for it.


```r
head(mtcars,3)
```

```
##                mpg cyl disp  hp drat    wt  qsec vs am gear carb
## Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## Datsun 710    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
```

```r
dim(mtcars)
```

```
## [1] 32 11
```

```r
names(mtcars)
```

```
##  [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
## [11] "carb"
```

For buliding the model we will randomly split the mtcars data into training and test set using `split.sample` function in `caTools` package.^[Since, data set is very small, I have just taken the last five(i.e. 15%) rows in the test set].


```r
#install.packages("caTools")
library(caTools)
set.seed(88)       #to fix the result that we get as we get different results everytime we run it

spl = sample(1:nrow(data), size=0.85* nrow(data))

train = data[spl,]

test = data[-spl,]
#this is the method for splitting the data set into training and test data when the outcome variable is
#continuous
```

```r
mtcars1<- mtcars[-(28:32), ] #the training set
test_data<- mtcars[28:32, ]  #test data set
test_data
```

```
##                 mpg cyl  disp  hp drat    wt qsec vs am gear carb
## Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.9  1  1    5    2
## Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.5  0  1    5    4
## Ferrari Dino   19.7   6 145.0 175 3.62 2.770 15.5  0  1    5    6
## Maserati Bora  15.0   8 301.0 335 3.54 3.570 14.6  0  1    5    8
## Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.6  1  1    4    2
```

**************

## Regression Analysis
We have graphically seen that Manual is better for `mpg`. Now we will qantify the difference between them. 

```r
mtcars1$cyl<- factor(mtcars1$cyl)
mtcars1$vs<- factor(mtcars1$vs)
mtcars1$am<- factor(mtcars1$am)
mtcars1$carb<- factor(mtcars1$carb)
mtcars1$gear<- factor(mtcars1$gear)
```

There are various methods for choosing a subset of variables for the best regression model which can explain variability in response variable well. There are 2 classes of algorithms for that:---

1. All possible regression approach 
2. sequential selection
    i) forward selection 
    ii) backward selection
    
    
Models are selected on the basis of adj. R^2^, MS~Res~, Mallow's statistic and/or AIC.
Here I will use the 'all possible regression approach' algorithm and choose the best model on the basis of AIC using `StepAIC` funcion. 

### Selecting the Best Model


```r
#install.packages("MASS")
library(MASS)
stepAIC(lm(mpg~., mtcars1), direction = "both") #direction can be forward, backward or both 
```

```
## Start:  AIC=58.36
## mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb
## 
##        Df Sum of Sq     RSS    AIC
## - cyl   2    0.5244  77.711 54.543
## - qsec  1    1.3899  78.577 56.842
## - am    1    2.0239  79.211 57.059
## - vs    1    3.3084  80.495 57.494
## <none>               77.187 58.361
## - wt    1    6.1828  83.369 58.441
## - hp    1    6.2913  83.478 58.476
## - gear  2   13.0445  90.231 58.577
## - carb  3   21.9188  99.105 59.109
## - disp  1   10.0829  87.270 59.675
## - drat  1   24.4139 101.601 63.781
## 
## Step:  AIC=54.54
## mpg ~ disp + hp + drat + wt + qsec + vs + am + gear + carb
## 
##        Df Sum of Sq     RSS    AIC
## - qsec  1     0.975  78.686 52.880
## - am    1     2.272  79.983 53.322
## - vs    1     3.685  81.396 53.794
## <none>               77.711 54.543
## - gear  2    14.013  91.724 55.019
## - wt    1     8.006  85.717 55.191
## - hp    1     9.495  87.206 55.656
## - disp  1    13.237  90.948 56.790
## + cyl   2     0.524  77.187 58.361
## - carb  3    36.381 114.092 58.912
## - drat  1    28.307 106.018 60.930
## 
## Step:  AIC=52.88
## mpg ~ disp + hp + drat + wt + vs + am + gear + carb
## 
##        Df Sum of Sq     RSS    AIC
## - am    1     2.925  81.611 51.865
## - vs    1     3.430  82.116 52.032
## <none>               78.686 52.880
## - wt    1     7.356  86.042 53.293
## - gear  2    15.910  94.595 53.852
## - hp    1    10.714  89.400 54.327
## + qsec  1     0.975  77.711 54.543
## - disp  1    12.295  90.981 54.800
## + cyl   2     0.109  78.577 56.842
## - drat  1    31.300 109.986 59.922
## - carb  3    50.314 128.999 60.227
## 
## Step:  AIC=51.87
## mpg ~ disp + hp + drat + wt + vs + gear + carb
## 
##        Df Sum of Sq     RSS    AIC
## - vs    1     0.580  82.191 50.057
## - wt    1     4.872  86.483 51.431
## <none>               81.611 51.865
## - hp    1     7.895  89.506 52.359
## - disp  1     9.458  91.069 52.826
## + am    1     2.925  78.686 52.880
## - gear  2    16.904  98.515 52.948
## + qsec  1     1.628  79.983 53.322
## + cyl   2     0.181  81.430 55.805
## - drat  1    39.399 121.010 60.501
## - carb  3    60.038 141.649 60.753
## 
## Step:  AIC=50.06
## mpg ~ disp + hp + drat + wt + gear + carb
## 
##        Df Sum of Sq     RSS    AIC
## <none>               82.191 50.057
## - hp    1     7.949  90.140 50.549
## - gear  2    16.535  98.726 51.006
## + qsec  1     0.784  81.406 51.798
## + vs    1     0.580  81.611 51.865
## - wt    1    12.873  95.064 51.985
## + am    1     0.075  82.116 52.032
## - disp  1    17.622  99.812 53.301
## + cyl   2     0.091  82.100 54.027
## - drat  1    40.417 122.608 58.855
## - carb  3    61.085 143.276 59.061
```

```
## 
## Call:
## lm(formula = mpg ~ disp + hp + drat + wt + gear + carb, data = mtcars1)
## 
## Coefficients:
## (Intercept)         disp           hp         drat           wt  
##     6.43785      0.03444     -0.03613      5.56520     -2.27712  
##       gear4        gear5        carb2        carb3        carb4  
##     4.49717      2.95981     -4.03391     -1.42888     -7.24308
```

```r
m1<- lm(mpg ~ disp + hp + drat + wt + vs + am + gear + carb, data=mtcars1)
#checking for multicollinearity using vif and clorrelation chart
#install.packages("car")
library(car)
vif(m1)
```

```
##           GVIF Df GVIF^(1/(2*Df))
## disp  58.92572  1        7.676309
## hp    23.34290  1        4.831449
## drat  14.24762  1        3.774602
## wt    22.09888  1        4.700944
## vs    24.57621  1        4.957440
## am    23.80195  1        4.878724
## gear  36.26722  2        2.454023
## carb 144.39082  3        2.290463
```

```r
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(mtcars1[,c(1,3,4,5,6,7)], histogram =T)
```

![plot of chunk unnamed-chunk-42](README_figs/README-unnamed-chunk-42-1.png)

StepAIC fn. will form models with all possible combinations of regressors. Then model is selected on the basis of AIC (Akaike's Information Criteria);

The variance inflation factor (VIF), which assesses how much the variance of an estimated regression coefficient increases if your predictors are correlated.  If no factors are correlated, the VIFs will all be 1. A VIF between 5 and 10 indicates high correlation that may be problematic. And if the VIF goes above 10, you can assume that the regression coefficients are poorly estimated due to multicollinearity.
Remove highly correlated predictors from the model.  If you have two or more factors with a high VIF, remove one from the model. Because they supply redundant information, removing one of the correlated factors usually doesn't drastically reduce the R-squared.  Consider using stepwise regression, best subsets regression, or specialized knowledge of the data set to remove these variables.

We remove `disp` as it is highly correlated with all the other regressors along with other regressors which had high vif. We fit various other models and select the best of them:

```r
m2<- lm(mpg~ hp+drat+wt+am+carb-1, mtcars1)
extractAIC(m2)
```

```
## [1]  8.00000 51.25171
```

```r
summary(m2)$coef 
```

```
##           Estimate Std. Error      t value    Pr(>|t|)
## hp    -0.023422257 0.01556769 -1.504543172 0.148884027
## drat   4.979435055 1.74005818  2.861648600 0.009984374
## wt    -0.003874243 1.15347572 -0.003358756 0.997355121
## am0    7.775902513 8.60531624  0.903616124 0.377513564
## am1    9.979994455 8.74368339  1.141394765 0.267887780
## carb2 -2.127666831 1.36934339 -1.553786172 0.136734494
## carb3 -2.531807306 1.92848074 -1.312850705 0.204870209
## carb4 -5.842635235 2.02749628 -2.881699602 0.009555014
```

```r
#the coef are not significant and vif is also somewhat high
```
Variables like `hp`(horsepower), `wt` are expected to be significant but are not. Hence, problem due to multicollinearity still persists in the model. Hence, we remove `wt`. By looking at the correlation chart, we see that most of the regressor var. are correlated to each other. so we pick a model with less no. of regressors.

```r
m9<-  lm(mpg~am+wt, mtcars1)
m6<- lm(mpg~am+wt+hp, mtcars1)
anova(m6,m9)   #it is significant. 
```

```
## Analysis of Variance Table
## 
## Model 1: mpg ~ am + wt + hp
## Model 2: mpg ~ am + wt
##   Res.Df    RSS Df Sum of Sq     F   Pr(>F)   
## 1     23 154.23                               
## 2     24 212.61 -1    -58.38 8.706 0.007174 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
m7<- lm(mpg~am+wt+hp+cyl, mtcars1)
anova(m6,m7);  #not significant according to anova
```

```
## Analysis of Variance Table
## 
## Model 1: mpg ~ am + wt + hp
## Model 2: mpg ~ am + wt + hp + cyl
##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
## 1     23 154.23                           
## 2     21 126.14  2     28.09 2.3382 0.1211
```

```r
summary(m6)
```

```
## 
## Call:
## lm(formula = mpg ~ am + wt + hp, data = mtcars1)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.6144 -1.5918 -0.3638  1.1406  5.5011 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 32.79946    2.88463  11.370 6.43e-11 ***
## am1          2.63214    1.60467   1.640  0.11455    
## wt          -2.23388    1.03965  -2.149  0.04242 *  
## hp          -0.04513    0.01530  -2.951  0.00717 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.59 on 23 degrees of freedom
## Multiple R-squared:  0.8416,	Adjusted R-squared:  0.8209 
## F-statistic: 40.72 on 3 and 23 DF,  p-value: 2.295e-09
```

```r
vif(m6) 
```

```
##       am       wt       hp 
## 2.161742 4.201016 3.160674
```

```r
extractAIC(m6) #variance inflation factor and AIC look good.
```

```
## [1]  4.00000 55.05088
```

According to our objectives our model must contain `wt` and `am`. Hence we start from there and perform anova test to check if the added regressor add any new, relevant information to the model. `m6` is significant amongst all and it's variance inflation factor and AIC are also low. Mileage at zero weight and horsepower doesn't make any sense, we subtract the intercept.

```r
#final model
m6<- lm(mpg~am+wt+hp-1, mtcars1)
summary(m6)
```

```
## 
## Call:
## lm(formula = mpg ~ am + wt + hp - 1, data = mtcars1)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.6144 -1.5918 -0.3638  1.1406  5.5011 
## 
## Coefficients:
##     Estimate Std. Error t value Pr(>|t|)    
## am0 32.79946    2.88463  11.370 6.43e-11 ***
## am1 35.43160    1.90375  18.612 2.31e-15 ***
## wt  -2.23388    1.03965  -2.149  0.04242 *  
## hp  -0.04513    0.01530  -2.951  0.00717 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.59 on 23 degrees of freedom
## Multiple R-squared:  0.9869,	Adjusted R-squared:  0.9847 
## F-statistic: 434.1 on 4 and 23 DF,  p-value: < 2.2e-16
```

```r
#all coef are significant.
confint(m6, level = 0.95)
```

```
##           2.5 %      97.5 %
## am0 26.83215463 38.76676260
## am1 31.49339554 39.36979756
## wt  -4.38455441 -0.08320047
## hp  -0.07677241 -0.01348965
```

Let's check how accurately it can predict the mileage of the car in the `test_data`.

```r
test_data$cyl<- factor(test_data$cyl)
test_data$vs<- factor(test_data$vs)
test_data$am<- factor(test_data$am)
test_data$carb<- factor(test_data$carb)
test_data$gear<- factor(test_data$gear)


predict(m6, test_data) ; test_data
```

```
##   Lotus Europa Ford Pantera L   Ferrari Dino  Maserati Bora     Volvo 142E 
##       26.95193       16.43561       21.34583       12.33776       24.30214
```

```
##                 mpg cyl  disp  hp drat    wt qsec vs am gear carb
## Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.9  1  1    5    2
## Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.5  0  1    5    4
## Ferrari Dino   19.7   6 145.0 175 3.62 2.770 15.5  0  1    5    6
## Maserati Bora  15.0   8 301.0 335 3.54 3.570 14.6  0  1    5    8
## Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.6  1  1    4    2
```

```r
1-(abs(predict(m6, test_data)-test_data[1]))/test_data[1]
```

```
##                      mpg
## Lotus Europa   0.8865768
## Ford Pantera L 0.9597713
## Ferrari Dino   0.9164555
## Maserati Bora  0.8225173
## Volvo 142E     0.8643862
```

```r
#comparing predictions through mean and standard deviation of accuracy
mean((1-(abs(predict(m6, test_data)-test_data[1]))/test_data[1])[,1]);mean((1-(abs(predict(m7,
test_data)-test_data[1]))/test_data[1])[,1])                                                                                    
```

```
## [1] 0.8899414
```

```
## [1] 0.9041645
```

```r
sd((1-(abs(predict(m6, test_data)-test_data[1]))/test_data[1])[,1]);sd((1-(abs(predict(m7,
   test_data)-test_data[1]))/test_data[1])[,1])                                                                                    
```

```
## [1] 0.05193654
```

```
## [1] 0.07878624
```

Predictions from m6 and the actual mileage of the cars in test data are very close with an average accuracy of abput 89%; model m6 is parsimonious and interpretable too.

### Checking basic assumptions of regression

```r
res<- m6$residuals
shapiro.test(res)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  res
## W = 0.95114, p-value = 0.2285
```

```r
#install.packages("ggfortify")
library(ggfortify)
autoplot(m6, label.size=4)
```

![plot of chunk unnamed-chunk-48](README_figs/README-unnamed-chunk-48-1.png)

**p>0.05** hence we fail to reject the null hypothesis that residuals are normally distributed.
By looking at diagnostic plots, we can say that the residuals are homoscedastic. The scatter points in QQ Plot also seem to lie on the line, thereby confirming their normality. 
Basic assumptions of regression are met. 


We can finally, say that **compared to cars which had automatic transmission(0) we would expect mileage of cars with manual transmission(1) 2.6 miles per gallon more on average given values of other regressor variables remain same**. By looking at the coefficient of the weight variable in the final model, we can conclude that a 1000lbs increase in the weight a car will decrease the mileage by 2.234 miles per gallon.


**************

## Appendix
This section includes all the above mentioned Plots.


```r
layout(matrix(c(1,2,1,2),2,2, byrow = TRUE))
boxplot(mpg~am,data=mtcars,col=c("red", "turquoise"),xlab="transmission type",ylab="miles per gallon", 
        names= c("Automatic","Manual"),main="Plot-1")
boxplot(mpg~vs, data= mtcars, col=c(4,"cyan"), xlab="Engine Cylinder Configuration",
        ylab="Miles/Gallon", las=TRUE, names= c("V shape", "Straight Line Shape"), main="Plot-2")
```

![ ](README_figs/README-unnamed-chunk-49-1.png)



```r
layout(matrix(c(1,2,1,2),2,2, byrow = T))
boxplot(mpg~cyl, data= mtcars, col=c("cyan",42,23), 
        ylab="Miles per Gallon", las=TRUE, main="Plot-3")
boxplot(mpg~gear, data= mtcars, col=c("cyan",42,23), xlab="No. of Gears",
        ylab="Miles per Gallon", las=T, main= "Plot-4")
```

![](README_figs/README-unnamed-chunk-50-1.png)



```r
boxplot(mpg~carb, data= mtcars, col=c("cyan",42,23,"green",600), xlab="Number of carburetors",
        ylab="Miles per Gallon", las=T, main="Plot-5")
```

![](README_figs/README-unnamed-chunk-51-1.png)


