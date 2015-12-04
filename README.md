# 6215
---
title: "Multivariate_analysis"
author: "Chunmei Gao"
date: "November 29, 2015"
output: html_document
---

```{r}
library(corrplot)
twin <- read.csv("~/Documents/6215-Applied Multivariance Model/project/nmtwins.csv")
#View(twin)
str(twin)
dim(twin)
```

```{r}
#seperate the data into two groups
identical <- twin[twin$zygosity==1,]
fraternal <- twin[twin$zygosity==2,]

# describe the data:

hist(twin$moed)
hist(twin$faed)
hist(twin$faminc)
hist(twin$english)

# checking the fraternal twin sex
for(i in 1:839)
twin[which(twin$zygosity==2),2]

# data cleaning
# missing value:


```

```{r}
# test multivariate normal distribution: QQ plot(for sigle variable)  Chi-square (for multi variables)

source("http://www.stat.wmich.edu/wang/561/codes/R/chisqplot.R")
chisqplot(twin[,7:11]) 
# a few points above the line indicate large distances or outlying observations

qqnorm(twin$english)
qqline(twin$english, col="red")

qqnorm(twin$math)
qqline(twin$math, col="red")

qqnorm(twin$socsci)
qqline(twin$socsci, col="red")

qqnorm(twin$natsci)
qqline(twin$natsci, col="red")

qqnorm(twin$vocab)
qqline(twin$vocab, col="red")
# from the qq plot for each score variables, we didint see a significant diviation from normal distribution.
## ??why the multi-normal has some divations? correlation? outlier?

```


```{r}
# PCA
#library(corrplot)
corrplot(cor(twin[,7:11]),method="number")
fit <- princomp(twin[,7:11], cor=TRUE)
summary(fit)
loadings(fit)
fit$scores[1:10,]
plot(fit,type="lines")
```
library(rpart)
df$y_rf<-as.factor(df$y)
kdd_randomForest_v1 <- randomForest(y_rf~school_latitude+
                      school_longitude+
                      school_metro+
                      teacher_prefix+
                      teacher_teach_for_america+
                      teacher_ny_teaching_fellow+
                      total_price_excluding_optional_support+
                      students_reached_imp+
                      eligible_double_your_impact_match+
                      eligible_almost_home_match+
                      poverty_level+
                      primary_focus_area+
                      resource_type+
                      essay_length+
                      title_length+
                      title_y_pp+
                      essay_y_pp+                      
                      agg_item_amount_total+
                      agg_cnt+                 
                      books_cnt+
                      books_item_amount_total+
                      supplies_item_amount_total+
                      other_item_amount_total+
                      tech_item_amount_total+
                      teacher_acctid_cnt+
                      schoolid_cnt+
                      teacher_acctid_y_prev_rate_cred+
                      schoolid_y_prev_rate_cred_cap+ 
                      school_district_y_prev_rate+                    
                      school_county_y_prev_rate+
                      schoolid_y2_prev_rate+                 
                      teacher_acctid_y2_prev_rate+                            
                      schoolid_y3_prev_rate+                 
                      teacher_acctid_y3_prev_rate+                            
                      school_district_y3_prev_rate+
                      schoolid_y4_prev_rate+                                        
                      schoolid_y5_prev_rate+                 
                      teacher_acctid_y5_prev_rate+
                      schoolid_y11_prev+
                      teacher_acctid_y11_prev+
                      schoolid_y12_prev+
                      teacher_acctid_y12_prev+
                      schoolid_y13_prev+
                      teacher_acctid_y13_prev,                
                    data = df[split,],
                   na.action = na.roughfix)
summary(kdd_randomForest_v1)
df$pred_randomForest<- predict(kdd_randomForest_v1, newdata=df, n.trees=650, type="response")
summary(df$pred_gbm)
df_test_randomForest <- df[df$split=="test",c("projectid","pred_randomForest")]
names(df_test_randomForest)[names(df_test_randomForest)=="pred_randomForest"] <- "is_exciting"
names(df_test_randomForest)
write.csv(df_test_randomForest[,c("projectid","is_exciting")],"model1.csv",row.names=F)
