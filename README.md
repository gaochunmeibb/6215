# 6215
why it doesnt work?
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
