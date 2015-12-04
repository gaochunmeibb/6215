---
title: "Multivariate_analysis"
author: "Chunmei Gao"
date: "November 29, 2015"
output: html_document
---

```{r}
library(corrplot)
twins <- read.csv("~/Documents/6215-Applied Multivariance Model/project/nmtwins.csv")
#View(twins)
str(twins)
dim(twins)
```

```{r}
#seperate the data into two groups
identical <- twins[twins$zygosity==1,]
fraternal <- twins[twins$zygosity==2,]

# describe the data:

hist(twins$moed)
hist(twins$faed)
hist(twins$faminc)
hist(twins$english)

# checking the fraternal twins sex
for(i in 1:839)
twins[which(twins$zygosity==2),2]

# data cleaning
# missing value:
sum(is.na(twins$moed))
sum(is.na(twins$faed))
sum(is.na(twins$faminc))

```

```{r}
# test multivariate normal distribution: QQ plot(for sigle variable)  Chi-square (for multi variables)

source("http://www.stat.wmich.edu/wang/561/codes/R/chisqplot.R")
chisqplot(twins[,7:11]) 
# a few points above the line indicate large distances or outlying observations

qqnorm(twins$english)
qqline(twins$english, col="red")

qqnorm(twins$math)
qqline(twins$math, col="red")

qqnorm(twins$socsci)
qqline(twins$socsci, col="red")

qqnorm(twins$natsci)
qqline(twins$natsci, col="red")

qqnorm(twins$vocab)
qqline(twins$vocab, col="red")
# from the qq plot for each score variables, we didint see a significant diviation from normal distribution.
## ??why the multi-normal has some divations? correlation? outlier?

```


```{r}
# PCA
#library(corrplot)
corrplot(cor(twins[,7:11]),method="number")
fit <- princomp(twins[,7:11], cor=TRUE)
summary(fit)
loadings(fit)
fit$scores[1:10,]
plot(fit,type="lines")
```


```{r, echo=FALSE}
plot(cars)
```


```{r}
##-----------------
##    Haoqi     ##
##-------------------
# I need a confidence interval
twins3=twins[,-c(1,3,4,5,6)]
head(twins3)
df1=twins3[c(twins3$sex==1),]
head(df1)
df2=twins3[c(twins3$sex==2),]
z1=df1[,2:6]
z2=df2[,2:6]
#using t-test to see whether there exists significant (ch5 co)
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
twins[1:10,]
twins2[1:10,]
head(twins2)
x=twins2[,7:11]
y=twins2[,12:16]
colMeans(twins2)
d=(x+y)/2
d=as.matrix(d)
sex=as.factor(twins2$sex)
zygosity=as.factor(twins2$zygosity)

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
```

```{r}
##-----------------
##    kelly      ##
##-------------------
twins=read.csv("nmtwins.csv")
head(twins)
twins[1:50,]
rep(c(1,2),times=3)
nrow(twins)
1678/2
index=rep(c(1,2),times=839)
df=data.frame(twins,index)
head(df)
df1=df[df$index==1,]
head(df1)
table(df1$pairnum)
df2=df[df$index==2,]
head(df2)
dfnew=data.frame(df1,df2)
head(dfnew)
dfnew=dfnew[,-c(12,13,15,24)]#…æµÙindex,pairnum,zygosity
head(dfnew)
library(plyr)
dfnew=rename(dfnew,c("sex.1"="sex2","moed.1"="moed2","faed.1"="faed2", "faminc.1"="faminc2","english.1" = "english2", "math.1" = "math2", "socsci.1" = "socsci2","natsci.1"="natsci2", "vocab.1"="vocab2"))
head(dfnew)
write.csv(dfnew,file="twins-in-row.csv",row.names=F)
dfnew=read.csv("twins-in-row.csv")

dfnew1=dfnew[,-c(12,13,14,15)]
head(dfnew1)
write.csv(dfnew1,"twins-in-rows2.csv",row.names=F)
table(dfnew$sex,dfnew$sex2)
table(dfnew$moed,dfnew$moed2)
table(dfnew$faed,dfnew$faed2)
table(dfnew$faminc,dfnew$faminc2)
----------------------------------------------
  ############################
  ##PCA for the test scores##
  ###########################
twin=read.csv("nmtwins.csv")
twin[1:20,]
twin2=read.csv("twins-in-rows2.csv")
twin2[1:50,]
#PCA
library(corrplot)
corrplot(cor(twin[,7:11]),method="number") #plot the correlation matrix
fit <- princomp(twin[,7:11], cor=TRUE)
summary(fit)
##Importance of components:
##                         Comp.1    Comp.2     Comp.3     Comp.4     Comp.5
##Standard deviation     1.8748378 0.7362136 0.62628756 0.57828468 0.46510586
##Proportion of Variance 0.7030034 0.1084021 0.07844722 0.06688264 0.04326469
##Cumulative Proportion  0.7030034 0.8114055 0.88985267 0.95673531 1.00000000
loadings(fit)
##Loadings:
##         Comp.1 Comp.2 Comp.3 Comp.4 Comp.5
##english -0.434 -0.401  0.780  0.143 -0.150
##math    -0.426  0.647  0.211 -0.596       
##socsci  -0.470 -0.191 -0.481        -0.709
##natsci  -0.445  0.433 -0.129  0.753  0.177
##vocab   -0.459 -0.444 -0.315 -0.226  0.665

##Comp.1 Comp.2 Comp.3 Comp.4 Comp.5
##SS loadings       1.0    1.0    1.0    1.0    1.0
##Proportion Var    0.2    0.2    0.2    0.2    0.2
##Cumulative Var    0.2    0.4    0.6    0.8    1.0
fit$scores[1:10,]
plot(fit,type="lines")#indicate 1 component
#reduction data
-0.434*twin$english-0.426*twin$math
PC1=-0.434*twin$english-0.426*twin$math-0.470*twin$socsci-0.445*twin$natsci-0.459*twin$vocab
PC2= -0.401*twin$english+ 0.647*twin$math+0.433*twin$natsci--0.444*twin$vocab
mean(PC2)
-------------------------------------------------------------------
####################################################################################
##the difference in absolute difference between identical twins and fraternal twins##
####################################################################################
twin[1:10,]
twin2[1:10,]
x=twin2[,7:11]
nrow(x)
y=twin2[,12:16]
nrow(y)
z=abs(x-y)#the absolute difference of scores between twins
z
nrow(z)
colMeans(z)
df=data.frame(twin2,z)#dataset we will use
df[1:10,]
z1=df[c(df$zygosity==1),17:21]#data who are identical twins
head(z1)
nrow(z1)##509 ∂‘Õ¨¬—À´∞˚Ã•
z2=df[c(df$zygosity==2),17:21]#data who are not identical twins
nrow(z2) ##330 ∂‘“Ï¬—À´∞˚Ã•
head(z2)

nrow(z1)
nrow(z2)
##two absokute difference comparison test
x1bar=colMeans(z1)
x1bar
x2bar=colMeans(z2)
x2bar
n1=nrow(z1)
n1
n2=nrow(z2)
n2
p=5
#assuming that the variance is equal within two groups
S1=var(z1)
S2=var(z2)
Spool=((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
T2=t(x1bar-x2bar)%*%solve((1/n1+1/n2)*Spool)%*%(x1bar-x2bar)
T2
##[,1]
##[1,] 118.1061
T2a=(n1+n2-2)*p*qf(df1=p,df2=n1+n2-p-1,0.96)/(n1+n2-p-1)
T2a 
##[1] 11.76112
#T2>T2a we can reject H0 that two population mean vectors are equivalent
#assuming that the variance is unequal within two groups
T22=t(x1bar-x2bar)%*%solve(S1/n1+S2/n2)%*%(x1bar-x2bar)
T22 
##[,1]
##[1,] 98.26564
pval = 1-pf(T22*(n1+n2-p-1)/((n1+n2-2)*p), p, n1+n2-p-1)
pval
t.test(z1[1],z2[1])
t.test(z1[2],z2[2])
t.test(z1[3],z2[3])
t.test(z1[4],z2[4])
t.test(z1[5],z2[5])
t.test(z1,z2)
nrow(z)
--------------------------------------------------------------------
twin[1:10,]
twin2[1:10,]
head(twin2)
x=twin2[,7:11]
y=twin2[,12:16]
colMeans(twin2)
d=(x+y)/2 #the mean scores between twins
colMeans(d)
df=data.frame(twin2,d)#dataset we will use
df[1:10,]
d1=df[c(df$zygosity==1),17:21]#data who are identical twins
head(d1)
d2=df[c(df$zygosity==2),17:21]#data who are not identical twins
head(d2)
nrow(d1)
nrow(d2)
##two means comparison test
x1bar=colMeans(d1)
x1bar
x2bar=colMeans(d2)
x2bar
n1=nrow(d1)
n2=nrow(d2)
p=5
#assuming that the variance is equal within two groups
S1=var(d1)
S2=var(d2)
Spool=((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
T2=t(x1bar-x2bar)%*%solve((1/n1+1/n2)*Spool)%*%(x1bar-x2bar)
T2
##    [,1]
##[1,] 8.503705
T2a=(n1+n2-2)*p*qf(df1=p,df2=n1+n2-p-1,0.96)/(n1+n2-p-1)
T2a 
##[1] 11.76112
#T2<T2a we cannot reject H0 that two population mean vectors are equivalent
#assuming that the variance is unequal within two groups
T22=t(x1bar-x2bar)%*%solve(S1/n1+S2/n2)%*%(x1bar-x2bar)
T22 
##    [,1]
##[1,] 8.783256
t.test(d1,d2)
t.test(d1[1],d2[1])
t.test(d1[2],d2[2])
t.test(d1[3],d2[3])
t.test(d1[4],d2[4])
t.test(d1[5],d2[5])
##∂‘Õ¨¬—“Ï¬—À´∞˚Ã•µƒ5∏ˆ∑÷ ˝◊ˆmanova
nrow(d)
head(twin2)
nrow(twin2)
Y1=d
Y1=as.matrix(Y1)
Y1
x=as.factor(twin2$zygosity)
fit=manova(Y1 ~ x)
summary.aov(fit)             # univariate ANOVA tables
summary(fit, test = "Wilks") # ANOVA table of Wilks' lambda
summary(fit)                # same F statistics as single-df terms  
##Df   Pillai approx F num Df den Df Pr(>F)
##x           1 0.010058   1.6926      5    833 0.1338
##Residuals 837    
-------------------------------------------------------------------------
  #########################################
 ## social class * zygosity interaction##
  ########################################
Y2=as.matrix(z)
zygosity=as.factor(twin2$zygosity)
motheredu=as.factor(twin2$moed)
fatheredu=as.factor(twin2$faed)
faminc=as.factor(twin2$faminc)

fit1=manova(Y1~zygosity*motheredu*fatheredu*faminc)#mean value as response
summary(fit1)
##                                      Df  Pillaiapprox F num Df den Df    Pr(>F)    
##zygosity                              1 0.01043   1.1192      5    531  0.349015    
##motheredu                             5 0.15139   3.3409     25   2675 4.445e-08 ***
##atheredu                             5 0.15530   3.4300     25   2675 2.006e-08 ***
##faminc                                6 0.08280   1.5014     30   2675  0.039428 *  
##zygosity:motheredu                    5 0.03386   0.7295     25   2675  0.831474    
##zygosity:fatheredu                    5 0.03237   0.6971     25   2675  0.864970    
##motheredu:fatheredu                  21 0.20434   1.0855    105   2675  0.263135    
##zygosity:faminc                       6 0.06308   1.1394     30   2675  0.275207    
##motheredu:faminc                     26 0.24992   1.0826    130   2675  0.251162    
##fatheredu:faminc                     27 0.28271   1.1875    135   2675  0.073397 .  
##zygosity:motheredu:fatheredu         19 0.19637   1.1511     95   2675  0.154011    
##zygosity:motheredu:faminc            21 0.25501   1.3692    105   2675  0.008477 ** 
##zygosity:fatheredu:faminc            22 0.19996   1.0130    110   2675  0.445685    
##motheredu:fatheredu:faminc           40 0.34123   0.9796    200   2675  0.566690    
##zygosity:motheredu:fatheredu:faminc  23 0.23482   1.1463    115   2675  0.141050    
##Residuals                           535                                             
##---
##  Signif. codes:  0 °Æ***°Ø 0.001 °Æ**°Ø 0.01 °Æ*°Ø 0.05 °Æ.°Ø 0.1 °Æ °Ø 1
fit2=manova(Y2~zygosity*motheredu*fatheredu*faminc)#difference as response
summary(fit2)
##                                      Df  Pillai approx F num Df den Df  Pr(>F)    
##zygosity                              1 0.19791  26.2039      5    531 < 2e-16 ***
##motheredu                             5 0.05249   1.1352     25   2675 0.29196    
##fatheredu                             5 0.04349   0.9388     25   2675 0.55030    
##faminc                                6 0.07822   1.4171     30   2675 0.06612 .  
##zygosity:motheredu                    5 0.06081   1.3173     25   2675 0.13432    
##zygosity:fatheredu                    5 0.04741   1.0244     25   2675 0.42937    
##motheredu:fatheredu                  21 0.14685   0.7709    105   2675 0.95848    
##zygosity:faminc                       6 0.08280   1.5015     30   2675 0.03942 *  
##motheredu:faminc                     26 0.25557   1.1084    130   2675 0.19454    
##fatheredu:faminc                     27 0.27819   1.1674    135   2675 0.09558 .  
##zygosity:motheredu:fatheredu         19 0.20339   1.1940     95   2675 0.10036    
##zygosity:motheredu:faminc            21 0.23366   1.2489    105   2675 0.04668 *  
##zygosity:fatheredu:faminc            22 0.26074   1.3379    110   2675 0.01207 *  
##motheredu:fatheredu:faminc           40 0.42221   1.2336    200   2675 0.01717 *  
##zygosity:motheredu:fatheredu:faminc  23 0.26721   1.3133    115   2675 0.01575 *  
##  Residuals                           535                                           
##---
##  Signif. codes:  0 °Æ***°Ø 0.001 °Æ**°Ø 0.01 °Æ*°Ø 0.05 °Æ.°Ø 0.1 °Æ °Ø 1
-----------------------------------------------------------------------
  
  
  ##use the reduction data
  ##absolute difference between different types of twins
df=data.frame(twin,PC1)
head(df)
index=rep(c(1,2),times=839)
df=data.frame(df,index)
head(df)
pc11=df[df$index==1,12]
pc12=df[df$index==2,12]
t.test(pc11,pc12,var.equal=F)
##  Welch Two Sample t-test

##data:  pc11 and pc12
##t = 1.7539, df = 1675.902, p-value = 0.07964
##alternative hypothesis: true difference in means is not equal to 0
##95 percent confidence interval:
##  -0.099589  1.783050
##sample estimates:
##  mean of x mean of y 
##-45.31259 -46.15433 

var.test(pc11,pc12)
##F test to compare two variances

##data:  pc11 and pc12
##F = 1.0154, num df = 838, denom df = 838, p-value = 0.8246
##alternative hypothesis: true ratio of variances is not equal to 1
##95 percent confidence interval:
##  0.8867573 1.1627916
##sample estimates:
##  ratio of variances 
##1.015438 

---------------------------------------------------------------------------------
twin[1:20,]
twin2[1:20,]
y=as.matrix(twin[,7:11])
x=as.matrix(twin[,2:6])
lm=lm(y~sex+ zygosity+moed+faed+ faminc,data=twin)
summary(lm)

x=as.matrix(twin[,7:11])
library(outliers)
chisq.out.test(x)
chisq.out.test(x,opposite=TRUE)

```
