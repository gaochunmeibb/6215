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



twins3=twins[,-c(1,3,4,5,6)]
head(twins3)
df1=twins3[c(twins3$sex==1),]
head(df1)
df2=twins3[c(twins3$sex==2),]
z1=df1[,2:6]
z2=df2[,2:6]
#using t-test to see whether there exists significant
#difference of mean between the sexes.
x1bar=colMeans(z1)
x2bar=colMeans(z2)
n1=nrow(z1)
n2=nrow(z2)
p=5

S1=var(z1)
S2=var(z2)
Spool=((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
T2=t(x1bar-x2bar)%*%solve((S1/n1+S2/n2))%*%(x1bar-x2bar)
T2
#[1,] 442.6469
c2 = (n1+n2-2)*p*qf(df1=p,df2=n1+n2-p-1,0.96)/(n1+n2-p-1)
c2
#[1] 11.7024
pval = 1-pf(T2*(n1+n2-p-1)/((n1+n2-2)*p), p, n1+n2-p-1)
pval
#[1,]    0

#Since T2>c2, the conclusion could be drawn that there exists significant
#difference between different sexes.

#using ANOVA 
twin[1:10,]
twin2[1:10,]
head(twin2)
x=twin2[,7:11]
y=twin2[,12:16]
colMeans(twin2)
d=(x+y)/2
d=as.matrix(d)
sex=as.factor(twin2$sex)
zygosity=as.factor(twin2$zygosity)

fit=manova(d~sex*zygosity)
summary(fit)

#Df   Pillai approx F num Df den Df Pr(>F)    
#sex            1 0.272730   62.326      5    831 <2e-16 ***
#  zygosity       1 0.009962    1.672      5    831 0.1387    
#sex:zygosity   1 0.004803    0.802      5    831 0.5483    
#Residuals    835                                           
#---
#  Signif. codes:  0 °Æ***°Ø 0.001 °Æ**°Ø 0.01 °Æ*°Ø 0.05 °Æ.°Ø 0.1 °Æ °Ø 1





twins2=read.csv("C:/Users/lyf/Desktop/twins-in-rows2.csv")
twins2=twins2[,-1]
dat.dist <- dist(twins2,method = "euclidean")
dat.hclust3 <- hclust(dat.dist, method="complete")
plot(dat.hclust3)

names(df_test_randomForest)
write.csv(df_test_randomForest[,c("projectid","is_exciting")],"model1.csv",row.names=F)
