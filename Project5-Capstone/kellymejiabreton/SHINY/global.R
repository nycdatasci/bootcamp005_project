#global
library(shiny)
library(randomForest)

movies<- read.csv("movies.csv")
x<-movies[,-7]
y<-movies[,7]

indexes <- sample(1:nrow(movies), size=0.2*nrow(movies))

test = movies[indexes,]
train = movies[-indexes,]

#`````````````````````````````````````````````````````````````````````````````
#  Logistic Model


logit_train<-glm(Oscar~.,
                 family="binomial",
                 data=train)

summary(logit_train)

#> summary(logit_train)

#Call:
#        glm(formula = Oscar ~ ., family = "binomial", data = train)

#Deviance Residuals: 
#        Min        1Q    Median        3Q       Max  
#-2.73533  -0.01780  -0.00065   0.00000   2.13581  

#Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)   
#(Intercept)        -3.393e+01  8.058e+03  -0.004   0.9966   
#audience_freshness -1.609e-01  1.051e-01  -1.530   0.1259   
#adjusted           -5.748e-09  2.424e-09  -2.371   0.0178 * 
#imdb_rating         5.413e+00  2.133e+00   2.538   0.0111 * 
#length              4.692e-02  3.826e-02   1.226   0.2201   
#rank_in_year       -6.203e-01  2.544e-01  -2.439   0.0147 * 
#ratingPG            3.581e+00  8.058e+03   0.000   0.9996   
#ratingPG-13         4.917e+00  8.058e+03   0.001   0.9995   
#ratingR             2.845e+00  8.058e+03   0.000   0.9997   
#Thriller           -1.589e-01  1.520e+00  -0.105   0.9168   
#Comedy             -6.945e-01  1.306e+00  -0.532   0.5949   
#Fantasy            -2.575e+00  1.938e+00  -1.329   0.1839   
#Sci.Fi              7.659e-01  1.880e+00   0.407   0.6838   
#Romance             4.834e+00  1.660e+00   2.911   0.0036 **
#Drama               2.069e+00  1.274e+00   1.624   0.1044   
#Family             -2.015e+01  3.009e+03  -0.007   0.9947   
#Crime              -2.941e+00  1.954e+00  -1.505   0.1324   
#Adventure          -5.640e+00  2.877e+00  -1.961   0.0499 * 
#War                 8.913e-01  1.765e+00   0.505   0.6135   
#Mystery            -2.035e+01  5.425e+03  -0.004   0.9970   
#Sport              -3.495e+00  4.336e+00  -0.806   0.4203   
#Horror             -2.008e+01  7.062e+03  -0.003   0.9977   
#Animation          -9.532e+00  6.573e+03  -0.001   0.9988   
#Music              -2.161e+01  7.878e+03  -0.003   0.9978   
#History            -2.110e+01  1.053e+04  -0.002   0.9984   
#Action             -4.485e+00  1.857e+00  -2.415   0.0157 * 
#Western             1.644e+00  2.107e+00   0.781   0.4351   
#Musical            -1.778e+01  1.720e+04  -0.001   0.9992   
#Biography           4.283e+01  1.599e+04   0.003   0.9979   
#---
#        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 184.815  on 317  degrees of freedom
#Residual deviance:  42.036  on 289  degrees of freedom
#AIC: 100.04

#Number of Fisher Scoring iterations: 20



library(car)# used for a nice influence plot

influencePlot(logit_train) 

#influencePlot(logit_train)
#StudRes        Hat        CookD
#60  -3.462274 0.09870251    0.1723697
#332 -0.137551 0.99999993 9929.3392661

oscar.predicted = round(logit_train$fitted.values) # setting the thresold 0.5
table(truth = train$Oscar, prediction = oscar.predicted) 

#table(truth = train$Oscar, prediction = oscar.predicted)
#prediction
#truth   0   1
#0     287   4
#1     5  22


287 / (287 + 4)
# [1] 0.9862543 
# using the logistic regression model, the true negative rate is 98.63%

22 / (22 + 5)
# [1] 0.8148148 
# using the logistic regression model, the true positive rate is 81.48%

# not bad, good fit. Lets see if we can get a better model
#`````````````````````````````````````````````````````````````````````````````
# logitisc model l(full data)

logit<-glm(Oscar~.,
                 family="binomial",
                 data=movies)

summary(logit)


#> summary(logit)

#Call:
#        glm(formula = Oscar ~ ., family = "binomial", data = movies)

#Deviance Residuals: 
#        Min       1Q   Median       3Q      Max  
#-2.1830  -0.1095  -0.0131   0.0000   3.3282  

#Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)   
#(Intercept)        -1.819e+01  6.929e+03  -0.003  0.99790   
#audience_freshness -4.709e-02  4.964e-02  -0.949  0.34276   
#adjusted           -2.262e-09  1.340e-09  -1.688  0.09139 . 
#imdb_rating         2.832e+00  9.521e-01   2.975  0.00293 **
#length              4.672e-03  2.056e-02   0.227  0.82021   
#rank_in_year       -2.801e-01  1.370e-01  -2.044  0.04093 * 
#ratingPG            4.583e-01  6.929e+03   0.000  0.99995   
#ratingPG-13         2.120e+00  6.929e+03   0.000  0.99976   
#ratingR             7.290e-01  6.929e+03   0.000  0.99992   
#Thriller           -6.461e-01  1.024e+00  -0.631  0.52794   
#Comedy             -7.706e-01  9.188e-01  -0.839  0.40163   
#Fantasy            -2.406e+00  1.500e+00  -1.604  0.10881   
#Sci.Fi             -1.781e+00  1.329e+00  -1.340  0.18019   
#Romance             2.432e+00  8.363e-01   2.908  0.00364 **
#Drama               1.564e+00  7.968e-01   1.962  0.04972 * 
#Family             -1.796e+01  3.257e+03  -0.006  0.99560   
#Crime              -1.043e+00  1.151e+00  -0.906  0.36490   
#Adventure          -3.346e+00  1.357e+00  -2.466  0.01366 * 
#War                -6.873e-01  1.275e+00  -0.539  0.58974   
#Mystery            -2.030e+01  5.137e+03  -0.004  0.99685   
#Sport              -1.497e+00  1.840e+00  -0.814  0.41591   
#Horror             -1.991e+01  6.764e+03  -0.003  0.99765   
#Animation          -1.585e+01  5.178e+03  -0.003  0.99756   
#Music              -2.045e+01  7.051e+03  -0.003  0.99769   
#History            -2.023e+01  1.056e+04  -0.002  0.99847   
#Action             -1.375e+00  9.421e-01  -1.460  0.14428   
#Western             2.643e+00  1.302e+00   2.029  0.04241 * 
#Musical            -2.131e+01  1.172e+04  -0.002  0.99855   
#Biography           4.103e+01  1.383e+04   0.003  0.99763   
#---
#        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 241.462  on 396  degrees of freedom
#Residual deviance:  83.401  on 368  degrees of freedom
#AIC: 141.4

#Number of Fisher Scoring iterations: 20


influencePlot(logit) 

#> influencePlot(logit)
#       StudRes        Hat        CookD
#148 0.0004550633 0.98755808 5.598229e-07
#278 3.7922232150 0.01287593 1.154158e-01
#352 1.3993063411 0.77719605 2.038766e-01

oscar.predicted2 = round(logit$fitted.values) # setting the thresold 0.5
table(truth = movies$Oscar, prediction = oscar.predicted2) 

#      prediction
#truth   0   1
#0      355   6
#1      11  25

355/(355+6)
#[1] 0.9833795  returning a true negatives of 98.34%


25/(25+11)

#[1] 0.6944444  returning a True positive of 69.44%

#`````````````````````````````````````````````````````````````````````````````
# Reduce logistic regression

reduced_movies<- movies[,c("adjusted", "imdb_rating", "Oscar", "rank_in_year",
                        "Romance", "Drama", "Adventure", "Western")]

reduced_logit<-glm(Oscar~.,
           family="binomial",
           data=reduced_movies)


summary(reduced_logit)


#Call:
#        glm(formula = Oscar ~ ., family = "binomial", data = reduced_movies)

#Deviance Residuals: 
#        Min       1Q   Median       3Q      Max  
#-1.6472  -0.2172  -0.0837  -0.0293   3.2924  

#Coefficients:
#                        Estimate Std. Error z value Pr(>|z|)    
#       (Intercept)  -1.782e+01  3.367e+00  -5.293 1.20e-07 ***
#        adjusted     -1.113e-09  7.524e-10  -1.479  0.13908    
#       imdb_rating   2.046e+00  4.198e-01   4.873 1.10e-06 ***
#        rank_in_year -1.478e-01  9.257e-02  -1.596  0.11041    
#       Romance       2.518e+00  5.951e-01   4.232 2.32e-05 ***
#        Drama         2.370e+00  5.515e-01   4.297 1.73e-05 ***
#        Adventure    -2.209e+00  9.367e-01  -2.358  0.01835 *  
#        Western       2.660e+00  1.009e+00   2.637  0.00837 ** 
#        ---
#        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 241.46  on 396  degrees of freedom
#Residual deviance: 124.59  on 389  degrees of freedom
#AIC: 140.59

#Number of Fisher Scoring iterations: 8

oscar.predicted3 = round(reduced_logit$fitted.values) # setting the thresold 0.5
table(truth = movies$Oscar, prediction = oscar.predicted3) 

#       prediction
#truth     0   1
#0       356   5
#1        19  17

#> 17/(19+17)
#[1] 0.4722222

#After fitting a reduced model with only the significant coefficients 
#I increased my type II error, and reduced my true positives rate by over 20% to 47.22%.

# lets try a different algorithm.

#`````````````````````````````````````````````````````````````````````````````
#  RANDOM FOREST


rf.movies = randomForest(Oscar ~ ., data = movies, importance = TRUE)
rf.movies


#Call:
#        randomForest(formula = Oscar ~ ., data = movies, importance = TRUE) 
#Type of random forest: regression
#Number of trees: 500
#No. of variables tried at each split: 8

#Mean of squared residuals: 0.05642605
#% Var explained: 31.57


summary(rf.movies)

importance(rf.movies) 
varImpPlot(rf.movies)

#%IncMSE IncNodePurity
#audience_freshness 12.29886563    4.07436641
#adjusted            8.47912350    3.15131200
#imdb_rating        15.68661041    4.20447805
#length              0.33754735    2.67120976
#rank_in_year        7.00178021    2.40810200
#rating              2.56819062    1.00284187
#Thriller            0.01695103    0.31314263
#Comedy              2.01605864    0.38655635
#Fantasy             2.33183205    0.07643370
#Sci.Fi              3.41241801    0.24579216
#Romance            10.47785360    2.26775505
#Drama              17.46724968    3.80720362
#Family              0.89476542    0.03815302
#Crime               5.77820489    0.50084083
#Adventure           5.88839722    0.73507148
#War                -1.17380229    0.28187196
#Mystery             2.20316240    0.14160114
#Sport               1.84376516    0.12291332
#Horror              3.32054662    0.14019360
#Animation          -0.38511777    0.04943280
#Music               2.70107959    0.13151827
#History             0.39651554    0.14189788
#Action              2.02209140    0.39661613
#Western            -1.66543358    0.42810065
#Musical             0.00000000    0.01652251
#Biography          13.50823335    1.52073797

set.seed(0)
oob.err = numeric(8)
for (mtry in 1:8) {
        fit = randomForest(Oscar~., data=train, mtry = mtry)# 
        oob.err[mtry] = fit$err.rate[500,1]#we want the last tree
        cat("We're performing iteration", mtry, "\n")# tells us where we are in the process to know if our computers gave up
}

plot(1:8, oob.err, pch = 16, type = "b",# type b says plot a line and a dot
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Misclassification Rate",
     main = "Random Forest OOB Error Rates\nby # of Variables")

table(round(predict(rf.movies, train, type = "class")), train$Oscar)

#    0    1
# 0 291   1
# 1   0  26

291/(291+1)
#[1] 0.9965753  True negative of 99.66% on the training set

26/26
# 1  True positive of 100% on the training set

table(round(predict(rf.movies, test, type = "class")), test$Oscar)

#  0   1
#0 70  4
#1  0  5

70/(70+4) 
#  [1] 0.9459459 True negative of 94.6% on the test set

5/5
# 1  True positive of 100% on the test set

# this is a great fit

#`````````````````````````````````````````````````````````````````````````````
#  RANDOM FOREST Adjusting the trees

rf.movies_reduce = randomForest(Oscar ~ ., data = train,
                              importance = TRUE, mtry=8, ntree=100)
rf.movies_reduce

#Call:
#        randomForest(formula = Oscar ~ ., data = train, importance = TRUE,      mtry = 8, ntree = 100) 
#Type of random forest: regression
#Number of trees: 100
#No. of variables tried at each split: 8

#Mean of squared residuals: 0.04744287
#% Var explained: 38.94


table(round(predict(rf.movies_reduce, train, type = "class")), train$Oscar)


#   0    1
#0 291   1
#1   0  26

291/292
26/26


table(round(predict(rf.movies_reduce, test, type = "class")), test$Oscar)
   0  1
0 68  6
1  2  3

68/(68+6)

3/5

#`````````````````````````````````````````````````````````````````````````````
#  RANDOM FOREST fitting the full dataset to use as a final model

rf.movies_full = randomForest(Oscar ~ ., data = movies,
                              importance = TRUE, mtry=8, ntree=500)
rf.movies_full


#Call:
#        randomForest(formula = Oscar ~ ., data = movies, importance = TRUE,      mtry = 8, ntree = 500) 
#Type of random forest: regression
#Number of trees: 500
#No. of variables tried at each split: 8

#Mean of squared residuals: 0.05520778
#% Var explained: 33.05

table(round(predict(rf.movies_full, movies, type = "class")), movies$Oscar)
#     0    1
#0  361   4
#1    0  32

361/365

#[1] 0.9890411 98.9% true negatives, and 100% true positives


