---
title: "Assignment4"
author: "Neve/Viva/Yaohan"
date: "2024-04-21"
output: 
  html_document:
    keep_md: yes
    toc: yes
    theme: flatly
    toc_float: yes
    code_folding: hide
    number_sections: no
  pdf_document:
    toc: yes
---

<style>
.kable thead tr th, .table thead tr th {
  text-align: left !important;}
table.kable, table.table {
  width: 100% !important;}
  body {
  line-height: 1.6;
  font-size: 16px
}
</style>



# Discussion

- Sensitivity：high cost, ruin others life
- Specificity：let go of criminals

# Plot of Variables


```r
# Recode dependent variable and reclassify categorical variables
dat <- dat %>%
  mutate(Recidivism_Within_3years = as.character(Recidivism_Within_3years)) %>%
  mutate(Recidivism_Within_3years = as.factor(recode(Recidivism_Within_3years,
    `FALSE` = "0",
    `TRUE` = "1"))) %>%
  mutate(Education_Level = factor(case_when(
    Education_Level %in% c('At least some college') ~ 'At least some college',
    Education_Level %in% c('Less than HS diploma', 'High School Diploma') ~ 'High School or Less'),
    levels = c('High School or Less', 'At least some college'))) %>%
  mutate(Prior_Arrest_Episodes_Felony = factor(case_when(
    Prior_Arrest_Episodes_Felony %in% c('0', '1', '2') ~ '0-2',
    Prior_Arrest_Episodes_Felony %in% c('3', '4', '5', '6', '7', '8', '9') ~ '3 to 9',
    Prior_Arrest_Episodes_Felony %in% c('10 or more') ~ '10 or more'),
    levels = c('0-2', '3 to 9', '10 or more'))) %>%
  mutate(Delinquency_Reports = factor(case_when(
    Delinquency_Reports %in% c('0') ~ '0',
    Delinquency_Reports %in% c('1', '2', '3', '4 or more') ~ '1 or more'))) %>%
  mutate(Program_Attendances = factor(case_when(
    Program_Attendances %in% c('0') ~ '0',
    Program_Attendances %in% c('1', '2', '3', '4', '5', '6', '7', '8', '9') ~ '1 to 9',
    Program_Attendances %in% c('10 or more') ~ '10 or more'))) %>%
  mutate(Program_UnexcusedAbsences = factor(case_when(
    Program_UnexcusedAbsences %in% c('0') ~ '0',
    Program_UnexcusedAbsences %in% c('1', '2', '3 or more') ~ '1 or more')))
```

**Dependent Variable:**  
- Recidivism_Within_3years

**Categorical Variables:**
- Gender  
- Race  
- Age_at_Release  
- Education_Level (Reclassified)  
- Dependents  
- Prior_Arrest_Episodes_Felony (Reclassified)  
- Condition_MH_SA  
- Condition_Cog_Ed  
- Condition_Other  
- Delinquency_Reports (Reclassified)  
- Program_Attendances (Reclassified)  
- Program_UnexcusedAbsences (Reclassified)  
- Employment_Exempt


```r
# Plot categorical variables
## Percent
dat %>%
  select(Recidivism_Within_3years,
         Gender,
         Race,
         Age_at_Release,
         Education_Level,
         Dependents,
         Prior_Arrest_Episodes_Felony,
         Condition_MH_SA,
         Condition_Cog_Ed,
         Condition_Other,
         Delinquency_Reports,
         Program_Attendances,
         Program_UnexcusedAbsences,
         Employment_Exempt) %>%
  gather(Variable, value, -Recidivism_Within_3years) %>%
  count(Variable, value, Recidivism_Within_3years) %>%
  group_by(Variable, value) %>%
  mutate(percent = round(n/sum(n)*100, digits = 2)) %>%
  ungroup() %>%
    ggplot(., aes(value, percent, fill = Recidivism_Within_3years)) +   
        geom_bar(position = "dodge", stat="identity") +
        facet_wrap(~Variable, ncol = 4, scales="free") +
        scale_fill_manual(values = palette2) +
        labs(x="Recidivism", y="Percentage",
             title = "Feature associations with the likelihood of Recidivism Within Three Years",
             subtitle = "Categorical features") +
        theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Assignment4_files/figure-html/categorical_plot-1.png)<!-- -->

```r
## Count
dat %>%
  select(Recidivism_Within_3years,
         Gender,
         Race,
         Age_at_Release,
         Education_Level,
         Dependents,
         Prior_Arrest_Episodes_Felony,
         Condition_MH_SA,
         Condition_Cog_Ed,
         Condition_Other,
         Delinquency_Reports,
         Program_Attendances,
         Program_UnexcusedAbsences,
         Employment_Exempt) %>%
  gather(Variable, value, -Recidivism_Within_3years) %>%
  count(Variable, value, Recidivism_Within_3years) %>%
    ggplot(., aes(value, n, fill = Recidivism_Within_3years)) +   
        geom_bar(position = "dodge", stat="identity") +
        facet_wrap(~Variable, ncol = 4, scales="free") +
        scale_fill_manual(values = palette2) +
        labs(x="Recidivism", y="Value",
             title = "Feature associations with the likelihood of Recidivism Within Three Years",
             subtitle = "Categorical features") +
        theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Assignment4_files/figure-html/categorical_plot-2.png)<!-- -->

**Continuous Variables:**
- Supervision_Risk_Score_First  
- Percent_Days_Employed  
- Prison_Years (Categorical -> Continuous)  
- Prior_Arrest_Episodes_Misd (Categorical -> Continuous)  
- Prior_Conviction_Episodes_Felony (Categorical -> Continuous)  
- Prior_Conviction_Episodes_Misd (Categorical -> Continuous)


```r
# Transform categorical variables into continuous variables
dat <- dat %>%
  mutate(Percent_Days_Employed = round(Percent_Days_Employed * 100, digit = 2)) %>%
  mutate(Prison_Years = case_when(
    Prison_Years == "Less than 1 year" ~ "1",
    Prison_Years == "1-2 years" ~ "2",
    Prison_Years == "Greater than 2 to 3 years" ~ "3",
    Prison_Years == "More than 3 years" ~ "4")) %>%
  mutate(Prison_Years = as.numeric(Prison_Years)) %>%
  mutate(Prior_Arrest_Episodes_Misd = case_when(
    Prior_Arrest_Episodes_Misd == "6 or more" ~ "6",
    TRUE ~ Prior_Arrest_Episodes_Misd)) %>%
  mutate(Prior_Arrest_Episodes_Misd = as.numeric(Prior_Arrest_Episodes_Misd)) %>%
  mutate(Prior_Conviction_Episodes_Felony = case_when(
    Prior_Conviction_Episodes_Felony == "3 or more" ~ "3",
    TRUE ~ Prior_Conviction_Episodes_Felony)) %>%
  mutate(Prior_Conviction_Episodes_Felony = as.numeric(Prior_Conviction_Episodes_Felony)) %>%
  mutate(Prior_Conviction_Episodes_Misd = case_when(
    Prior_Conviction_Episodes_Misd == "4 or more" ~ "4",
    TRUE ~ Prior_Conviction_Episodes_Misd)) %>%
  mutate(Prior_Conviction_Episodes_Misd = as.numeric(Prior_Conviction_Episodes_Misd))

# Plot continuous variables
## Mean
dat %>%
  select(Supervision_Risk_Score_First,
         Percent_Days_Employed,
         Prison_Years,
         Prior_Arrest_Episodes_Misd,
         Prior_Conviction_Episodes_Felony,
         Prior_Conviction_Episodes_Misd,
         Recidivism_Within_3years) %>%
  filter_all(all_vars(!is.na(.))) %>%
  gather(Variable, value, -Recidivism_Within_3years) %>%
    ggplot(aes(Recidivism_Within_3years, value, fill=Recidivism_Within_3years)) + 
      geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
      facet_wrap(~Variable, scales = "free") +
      scale_fill_manual(values = palette2) +
      labs(x="Recidivism", y="Value", 
           title = "Feature associations with the likelihood of Recidivism Within Three Years",
           subtitle = "Continuous features") +
       theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Assignment4_files/figure-html/continuous_plot-1.png)<!-- -->

```r
## Density
dat %>%
   select(Supervision_Risk_Score_First,
         Percent_Days_Employed,
         Prison_Years,
         Prior_Arrest_Episodes_Misd,
         Prior_Conviction_Episodes_Felony,
         Prior_Conviction_Episodes_Misd,
         Recidivism_Within_3years) %>%
    filter_all(all_vars(!is.na(.))) %>%
    gather(Variable, value, -Recidivism_Within_3years) %>%
    ggplot() + 
    geom_density(aes(value, color=Recidivism_Within_3years), fill = "transparent") + 
    facet_wrap(~Variable, scales = "free") +
    scale_fill_manual(values = palette2) +
    labs(title = "Feature distributions of Recidivism Within Three Years vs. NO Recidivism Within Three Years",
         subtitle = "Continuous features") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Assignment4_files/figure-html/continuous_plot-2.png)<!-- -->

# Model Develop


```r
# Initial variable selection
## Build dataset
dat <- dat %>%
  select(Gender, Race, Age_at_Release, Education_Level, Dependents, Prior_Arrest_Episodes_Felony,
         Condition_MH_SA, Condition_Cog_Ed, Condition_Other, Delinquency_Reports, Program_Attendances,
         Program_UnexcusedAbsences, Employment_Exempt, Supervision_Risk_Score_First, Percent_Days_Employed,
         Prison_Years, Prior_Arrest_Episodes_Misd, Prior_Conviction_Episodes_Felony, Prior_Conviction_Episodes_Misd,
         Recidivism_Within_3years) %>%
filter_all(all_vars(!is.na(.)))

## Split dataset
set.seed(1)
trainIndex <- createDataPartition(dat$Recidivism_Within_3years, p = .50,
                                  list = FALSE,
                                  times = 1)
dat.train <- dat[trainIndex,]
dat.test  <- dat[-trainIndex,]

## Remove insignificant variables based on sensitivity and specificity
mod1 <- glm(Recidivism_Within_3years ~ .,
                 data = dat.train,
                 na.action = na.exclude,
                 family = binomial("logit"))

testProbs <- data.frame(Outcome = as.factor(dat.test$Recidivism_Within_3years),
                        Probs = predict(mod1, dat.test, type= "response")) %>% ### Test probabilities
  mutate(predOutcome  = as.factor(ifelse(Probs > 0.5 , "1", "0"))) ### Set thresholds
100 * prop.table(table(Observed = testProbs$Outcome, Predicted = testProbs$predOutcome), margin = 1)
```

```
##         Predicted
## Observed        0        1
##        0 54.07767 45.92233
##        1 19.24921 80.75079
```

```r
### Remove Race
mod2 <- glm(Recidivism_Within_3years ~ . -Race,
                 data = dat.train,
                 na.action = na.exclude,
                 family = binomial("logit"))

testProbs <- data.frame(Outcome = as.factor(dat.test$Recidivism_Within_3years),
                        Probs = predict(mod2, dat.test, type= "response")) %>% ### Test probabilities
  mutate(predOutcome  = as.factor(ifelse(Probs > 0.5 , "1", "0"))) ### Set thresholds
100 * prop.table(table(Observed = testProbs$Outcome, Predicted = testProbs$predOutcome), margin = 1)
```

```
##         Predicted
## Observed       0       1
##        0 53.8835 46.1165
##        1 19.0300 80.9700
```

```r
### Remove Delinquency_Reports
mod3 <- glm(Recidivism_Within_3years ~ . -Race -Delinquency_Reports,
                 data = dat.train,
                 na.action = na.exclude,
                 family = binomial("logit"))

testProbs <- data.frame(Outcome = as.factor(dat.test$Recidivism_Within_3years),
                        Probs = predict(mod3, dat.test, type= "response")) %>% ### Test probabilities
  mutate(predOutcome  = as.factor(ifelse(Probs > 0.5 , "1", "0"))) ### Set thresholds
100 * prop.table(table(Observed = testProbs$Outcome, Predicted = testProbs$predOutcome), margin = 1)
```

```
##         Predicted
## Observed        0        1
##        0 53.82524 46.17476
##        1 18.96150 81.03850
```

```r
### Remove Employment_Exempt
mod4 <- glm(Recidivism_Within_3years ~ . -Race -Delinquency_Reports -Employment_Exempt,
                 data = dat.train,
                 na.action = na.exclude,
                 family = binomial("logit"))

testProbs <- data.frame(Outcome = as.factor(dat.test$Recidivism_Within_3years),
                        Probs = predict(mod4, dat.test, type= "response")) %>% ### Test probabilities
  mutate(predOutcome  = as.factor(ifelse(Probs > 0.5 , "1", "0"))) ### Set thresholds
100 * prop.table(table(Observed = testProbs$Outcome, Predicted = testProbs$predOutcome), margin = 1)
```

```
##         Predicted
## Observed        0        1
##        0 54.01942 45.98058
##        1 18.92040 81.07960
```

```r
### Remove Prior_Conviction_Episodes_Felony
mod5 <- glm(Recidivism_Within_3years ~ . -Race -Delinquency_Reports -Employment_Exempt
            -Prior_Conviction_Episodes_Felony,
                 data = dat.train,
                 na.action = na.exclude,
                 family = binomial("logit"))

testProbs <- data.frame(Outcome = as.factor(dat.test$Recidivism_Within_3years),
                        Probs = predict(mod5, dat.test, type= "response")) %>% ### Test probabilities
  mutate(predOutcome  = as.factor(ifelse(Probs > 0.5 , 1, 0))) ### Set thresholds
100 * prop.table(table(Observed = testProbs$Outcome, Predicted = testProbs$predOutcome), margin = 1)
```

```
##         Predicted
## Observed        0        1
##        0 53.94175 46.05825
##        1 18.79710 81.20290
```

# Result Interpretation


```r
stargazer(mod5, type = 'text', star.cutoffs = c(0.05, 0.01, 0.001))
```

```
## 
## ====================================================================
##                                             Dependent variable:     
##                                        -----------------------------
##                                          Recidivism_Within_3years   
## --------------------------------------------------------------------
## GenderM                                          0.446***           
##                                                   (0.061)           
##                                                                     
## Age_at_Release23-27                              -0.419***          
##                                                   (0.089)           
##                                                                     
## Age_at_Release28-32                              -0.761***          
##                                                   (0.094)           
##                                                                     
## Age_at_Release33-37                              -0.970***          
##                                                   (0.099)           
##                                                                     
## Age_at_Release38-42                              -1.182***          
##                                                   (0.107)           
##                                                                     
## Age_at_Release43-47                              -1.351***          
##                                                   (0.113)           
##                                                                     
## Age_at_Release48 or older                        -1.708***          
##                                                   (0.112)           
##                                                                     
## Education_LevelAt least some college              -0.105            
##                                                   (0.055)           
##                                                                     
## Dependents1                                        0.094            
##                                                   (0.057)           
##                                                                     
## Dependents2                                        0.005            
##                                                   (0.060)           
##                                                                     
## Dependents3 or more                                0.048            
##                                                   (0.053)           
##                                                                     
## Prior_Arrest_Episodes_Felony3 to 9               0.508***           
##                                                   (0.058)           
##                                                                     
## Prior_Arrest_Episodes_Felony10 or more           1.043***           
##                                                   (0.078)           
##                                                                     
## Condition_MH_SA                                  0.351***           
##                                                   (0.046)           
##                                                                     
## Condition_Cog_Ed                                  -0.042            
##                                                   (0.043)           
##                                                                     
## Condition_Other                                    0.030            
##                                                   (0.045)           
##                                                                     
## Program_Attendances1 to 9                        -0.163***          
##                                                   (0.048)           
##                                                                     
## Program_Attendances10 or more                    -0.644***          
##                                                   (0.072)           
##                                                                     
## Program_UnexcusedAbsences1 or more               0.274***           
##                                                   (0.060)           
##                                                                     
## Supervision_Risk_Score_First                     0.073***           
##                                                   (0.010)           
##                                                                     
## Percent_Days_Employed                            -0.011***          
##                                                  (0.0005)           
##                                                                     
## Prison_Years                                     -0.066**           
##                                                   (0.020)           
##                                                                     
## Prior_Arrest_Episodes_Misd                       0.078***           
##                                                   (0.014)           
##                                                                     
## Prior_Conviction_Episodes_Misd                   0.097***           
##                                                   (0.021)           
##                                                                     
## Constant                                           0.043            
##                                                   (0.130)           
##                                                                     
## --------------------------------------------------------------------
## Observations                                      12,451            
## Log Likelihood                                  -7,283.818          
## Akaike Inf. Crit.                               14,617.640          
## ====================================================================
## Note:                                  *p<0.05; **p<0.01; ***p<0.001
```
1. Model Result 

2. Most Risky Person


# ROC Curve

The ROC curve, gives us another visual "goodness of fit" metric. One that is a bit more tricky. You want to have a curve that is "above" the y=x line, which is where your prediction rates for positives and negatives are "no better than a coin flip". If it's too "square" - you are probably over fit. The Area-Under-The-Curve or "AUC" calculation below will help guide your understanding of the ROC curve


```r
pROC::auc(testProbs$Outcome, testProbs$Probs)
```

```
## Area under the curve: 0.7437
```


```r
# function from the book
iterateThresholds <- function(data, observedClass, predictedProbs, group) {
  observedClass <- enquo(observedClass)
  predictedProbs <- enquo(predictedProbs)
  group <- enquo(group)
  x = .01
  all_prediction <- data.frame()
  
  if (missing(group)) {
  
    while (x <= 1) {
    this_prediction <- data.frame()
    
    this_prediction <-
      data %>%
      mutate(predclass = ifelse(!!predictedProbs > x, 1,0)) %>%
      count(predclass, !!observedClass) %>%
      summarize(Count_TN = sum(n[predclass==0 & !!observedClass==0]),
                Count_TP = sum(n[predclass==1 & !!observedClass==1]),
                Count_FN = sum(n[predclass==0 & !!observedClass==1]),
                Count_FP = sum(n[predclass==1 & !!observedClass==0]),
                Rate_TP = Count_TP / (Count_TP + Count_FN),
                Rate_FP = Count_FP / (Count_FP + Count_TN),
                Rate_FN = Count_FN / (Count_FN + Count_TP),
                Rate_TN = Count_TN / (Count_TN + Count_FP),
                Accuracy = (Count_TP + Count_TN) / 
                           (Count_TP + Count_TN + Count_FN + Count_FP)) %>%
      mutate(Threshold = round(x,2))
    
    all_prediction <- rbind(all_prediction,this_prediction)
    x <- x + .01
  }
  return(all_prediction)
  }
  else if (!missing(group)) { 
   while (x <= 1) {
    this_prediction <- data.frame()
    
    this_prediction <-
      data %>%
      mutate(predclass = ifelse(!!predictedProbs > x, 1,0)) %>%
      group_by(!!group) %>%
      count(predclass, !!observedClass) %>%
      summarize(Count_TN = sum(n[predclass==0 & !!observedClass==0]),
                Count_TP = sum(n[predclass==1 & !!observedClass==1]),
                Count_FN = sum(n[predclass==0 & !!observedClass==1]),
                Count_FP = sum(n[predclass==1 & !!observedClass==0]),
                Rate_TP = Count_TP / (Count_TP + Count_FN),
                Rate_FP = Count_FP / (Count_FP + Count_TN),
                Rate_FN = Count_FN / (Count_FN + Count_TP),
                Rate_TN = Count_TN / (Count_TN + Count_FP),
                Accuracy = (Count_TP + Count_TN) / 
                           (Count_TP + Count_TN + Count_FN + Count_FP)) %>%
      mutate(Threshold = round(x, 2))
    
    all_prediction <- rbind(all_prediction, this_prediction)
    x <- x + .01
  }
  return(all_prediction)
  }
}
```



```r
ggplot(testProbs, aes(d = as.numeric(Outcome), m = Probs)) +
  geom_roc(n.cuts = 50, labels = T, labelsize = 3,labelround = 2,colour = "#FE9900") +
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC Curve - clickModel") +
    theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Assignment4_files/figure-html/roc_curve-1.png)<!-- -->

```r
# choose the threshold: sensitivity + specificity increase
whichThreshold <- 
  iterateThresholds(data=testProbs, observedClass = Outcome, predictedProbs = Probs)

whichThreshold.nocost <- whichThreshold %>%
  dplyr::select(Rate_TP, Rate_TN, Threshold)%>%
  rename(Sensitivity = Rate_TP, Specificity = Rate_TN)

whichThreshold.nocost %>%
  gather(Variable, Rate, -Threshold) %>%
  ggplot(.,aes(Threshold, Rate, colour = Variable)) +
  geom_point() +
  scale_colour_manual(values = palette2) +    
  labs(title = "Specificity and Sensitivity by threshold",
       y = "Rate") +
  guides(colour=guide_legend(title = "Rate"))+
        theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
```

![](Assignment4_files/figure-html/roc_curve-2.png)<!-- -->

```r
# -> result: threshold 0.59, Rate_TP ≈ Rate_TN ≈ Accuracy ≈ 0.68
```

# Confusion Matrix

Each threshold (e.g. a probability above which a prediction is a "click" and below which it's a "no click") has it's own rate of error. These errors can be classified in four ways for a binary model.

A "confusion matrix" for the threshold of 50% shows us the rate at which we got True Positives (aka Sensitivity), False Positives, True Negatives (aka Specificity) and False Negatives for that threshold.


```r
testProbs <- 
  testProbs %>%
  mutate(predOutcome  = as.factor(ifelse(testProbs$Probs > 0.59 , 1, 0)))

caret::confusionMatrix(testProbs$predOutcome, testProbs$Outcome, 
                       positive = "1")
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    0    1
##          0 3510 2313
##          1 1640 4986
##                                                
##                Accuracy : 0.6825               
##                  95% CI : (0.6742, 0.6906)     
##     No Information Rate : 0.5863               
##     P-Value [Acc > NIR] : < 0.00000000000000022
##                                                
##                   Kappa : 0.3578               
##                                                
##  Mcnemar's Test P-Value : < 0.00000000000000022
##                                                
##             Sensitivity : 0.6831               
##             Specificity : 0.6816               
##          Pos Pred Value : 0.7525               
##          Neg Pred Value : 0.6028               
##              Prevalence : 0.5863               
##          Detection Rate : 0.4005               
##    Detection Prevalence : 0.5323               
##       Balanced Accuracy : 0.6823               
##                                                
##        'Positive' Class : 1                    
## 
```
1. cell meaning

2. accuracy

3. sensitivity and specificity


# Error and Equity Discussion


```r
testProbs2 <- 
  data.frame(class = as.numeric(as.character(dat.test$Recidivism_Within_3years)),
             probs = predict(mod5, dat.test, type = "response"),
             Race = dat.test$Race,
             Gender = dat.test$Gender)%>%
  mutate(Race.Gender = paste(Race, Gender, sep = "-"))

# plot error by race
testProbs.thresholds <- 
  iterateThresholds(data=testProbs2, observedClass = class, 
                    predictedProbs = probs, group = Race)

filter(testProbs.thresholds, Threshold == .59)  %>%
  dplyr::select(Accuracy, Race, starts_with("Rate")) %>%
  gather(Variable, Value, -Race) %>%
  ggplot(aes(Variable, Value, fill = Race)) +
  geom_bar(aes(fill = Race), position = "dodge", stat = "identity",colour = "white") +
  geom_text(aes(label = sprintf("%.2f", Value)),                      # Add labels
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = palette2) +
  labs(title="Confusion matrix rates by race",
       subtitle = "50% threshold", x = "Outcome",y = "Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Assignment4_files/figure-html/race-1.png)<!-- -->

```r
# plot error by race and gender
testProbs.thresholds <- 
  iterateThresholds(data=testProbs2, observedClass = class, 
                    predictedProbs = probs, group = Race.Gender)

filter(testProbs.thresholds, Threshold == .59)  %>%
  dplyr::select(Accuracy, Race.Gender, starts_with("Rate")) %>%
  gather(Variable, Value, -Race.Gender) %>%
  ggplot(aes(Variable, Value, fill = Race.Gender)) +
  geom_bar(aes(fill = Race.Gender), position = "dodge", stat = "identity",colour = "white") +
  geom_text(aes(label = sprintf("%.2f", Value)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = palette4) +
  labs(title="Confusion matrix rates by race and gender",
       subtitle = "50% threshold", x = "Outcome",y = "Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Assignment4_files/figure-html/race and gender-1.png)<!-- -->


# Cost-Benifit Analysis

- Average cost of keeping someone in prison for a year: $40,000  
- Average cost of arrest and trial for a recidivist: $10,000   
- Average cost of a crime committed by a recidivist: $50,000
- Average compensation for wrongful incarceration: $50,000

**True negative Cost**: "An individual is predicted not to recidivate and indeed does not recidivate." - No cost
**True positive Cost**: "An individual is predicted to recidivate and then does recidivate." - $40k 
**False negative Cost**: "An individual is predicted not to recidivate but then does recidivate." - $100k
**False positive Cost**: "An individual is predicted to recidivate but does not actually recidivate." - $50k


```r
whichThreshold.cost <- whichThreshold %>%
  dplyr::select(starts_with("Count"), Threshold) %>%
  mutate(Cost.TN = Count_TN*0,
         Cost.TP = Count_TP*40,
         Cost.FN = Count_FN*100,
         Cost.FP = Count_FP*50,
         Cost.all = Cost.TN + Cost.TP + Cost.FN + Cost.FP)%>%
  select(starts_with("Cost"), Threshold)%>%
  gather(Variable, Cost, -Threshold)

whichThreshold.cost %>%
  ggplot(.,aes(Threshold, Cost, colour = Variable)) +
  geom_point() +
  scale_colour_manual(values = palette5) +    
  labs(title = "Cost by confusion matrix type and threshold",
       y = "Cost") +
  guides(colour=guide_legend(title = "Confusion Matrix"))+
        theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
```

![](Assignment4_files/figure-html/cost-1.png)<!-- -->

```r
threshold.cost <- whichThreshold.cost%>%
  filter(Variable == 'Cost.all')

#-> lowest cost when threshold = 0.49
```


# Conclusion




