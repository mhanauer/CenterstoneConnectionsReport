---
title: "Untitled"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load the packages
```{r}
library(effects)
library(effects)
require(sandwich)
require(foreign)
require(MASS)
library(AER)
library(mvnmle)
library(MissMech)
library(plyr)
library(Amelia)
library(Hmisc)
library(ggplot2)
library(gtools)
```
Load the data
Impute data
Get all the variables that you want
Change all of the 97's, 98, and 99's to NA
Assign them to the correct type
Check the distributons of everything and make changes
Figure out a way to turn a variable like month into a factor and run multiple of them
JAILTIME_N = 99 means they did not go to jail which needs to be changed to 0

Need to drop employment variable.  Need to include religion

Best I can do for age is 2018 - AGE

Need to get all the substance use data and create an aggregate score for baseline and 3 months.

Then create the aggregate score of substance use for baseline and 3 month.

Then make sure the names make sense.  

Then impute the data.  

Then get the correct averages and then look at the percentage change for substance use.

Variables that I want: 

White, DOB (turn into age), Living (4-9), total substance use score, education, employed, family support variables, mental medical costs, substance use medical costs, and gender
```{r}
ConnBCA = data.frame(apply(GPRAAll, 2, function(x)(ifelse(x == 97, NA, ifelse(x == 98, NA, ifelse(x == 99, 0, ifelse( x == -99, 0,x)))))))


ConnBCA = data.frame(ConnBCA$Gender.x, ConnBCA$White.x, ConnBCA$Education.x, ConnBCA$Education.y, ConnBCA$Employed.x, ConnBCA$Employed.y,ConnBCA$IllegalDrugs.x, ConnBCA$Cocaine.x, ConnBCA$Marijuana.x, ConnBCA$Heroin.x,  ConnBCA$Morphine.x,  ConnBCA$Dilaudid.x, ConnBCA$Demerol.x, ConnBCA$Percocet.x, ConnBCA$Darvon.x, ConnBCA$Codeine.x, ConnBCA$OxyContin.x, ConnBCA$Methadone.x, ConnBCA$Hallucinogens.x,  ConnBCA$Meth.x,  ConnBCA$Benzos.x, ConnBCA$Barbiturates.x, ConnBCA$GHB.x, ConnBCA$Ketamine.x, ConnBCA$OtherTranq.x, ConnBCA$Inhalants.x, ConnBCA$OtherDrugs.x, ConnBCA$IllegalDrugs.y, ConnBCA$Cocaine.y, ConnBCA$Marijuana.y, ConnBCA$Heroin.y,  ConnBCA$Morphine.y,  ConnBCA$Dilaudid.y, ConnBCA$Demerol.y, ConnBCA$Percocet.y, ConnBCA$Darvon.y, ConnBCA$Codeine.y, ConnBCA$OxyContin.y, ConnBCA$Methadone.y, ConnBCA$Hallucinogens.y,  ConnBCA$Meth.y,  ConnBCA$Benzos.y, ConnBCA$Barbiturates.y, ConnBCA$GHB.y,ConnBCA$Ketamine.y, ConnBCA$OtherTranq.y, ConnBCA$Inhalants.y, ConnBCA$OtherDrugs.y, ConnBCA$FamilySupport.x, ConnBCA$SelfHelp.x, ConnBCA$FamilySupport.y, ConnBCA$SelfHelp.y, ConnBCA$InPhysicalTimes.x, ConnBCA$InMentalTimes.x, ConnBCA$InAlcoholTimes.x, ConnBCA$OutPhysicalTimes.x, ConnBCA$OutMentalTimes.x, ConnBCA$OutAlcoholTimes.x, ConnBCA$ERPhysicalTimes.x, ConnBCA$ERMentalTimes.x, ConnBCA$ERAlcoholTimes.x, ConnBCA$InPhysicalTimes.y, ConnBCA$InMentalTimes.y, ConnBCA$InAlcoholTimes.y, ConnBCA$OutPhysicalTimes.y, ConnBCA$OutMentalTimes.y, ConnBCA$OutAlcoholTimes.y, ConnBCA$ERPhysicalTimes.y, ConnBCA$ERMentalTimes.y, ConnBCA$ERAlcoholTimes.y) 


```

1. Grab the correct variables from the data set
2. get colMeans so that you can include the na's.  Then try mass deletion from original data set and include new one



Getting the substance use totals here

```{r}
totalSubUseBase= ConnBCA[,7:27]; head(totalSubUseBase)
write.csv(totalSubUseBase, "totalSubUseBase.csv", row.names = FALSE)
totalSubUseBase = read.csv("totalSubUseBase.csv", header= TRUE)
totalSubUseBase = rowMeans(totalSubUseBase, na.rm = TRUE)


totalSubUseMonth6= ConnBCA[,28:48]; head(totalSubUseMonth6)
write.csv(totalSubUseMonth6, "totalSubUseMonth6.csv", row.names = FALSE)
totalSubUseMonth6 = read.csv("totalSubUseMonth6.csv", header = TRUE)
totalSubUseMonth6 = rowMeans(totalSubUseMonth6)


ConnBCA[,c(7:27, 28:48)] = NULL

ConnBCA$totalSubUseBase = totalSubUseBase
ConnBCA$totalSubUseMonth6 = totalSubUseMonth6

```

Now we have a family support construct

```{r}
fsBase = data.frame(ConnBCA$ConnBCA.SelfHelp.x, ConnBCA$ConnBCA.FamilySupport.x)
write.csv(fsBase, "fsBase.csv", row.names = FALSE)
fsBase = read.csv("fsBase.csv", header= TRUE)
fsBase = rowMeans(fsBase, na.rm = TRUE)


fsMonth6 = data.frame(ConnBCA$ConnBCA.SelfHelp.y, ConnBCA$ConnBCA.FamilySupport.y)
write.csv(fsMonth6, "fsMonth6.csv", row.names = FALSE)
fsMonth6 = read.csv("fsMonth6.csv", header= TRUE)
fsMonth6 = rowMeans(fsMonth6, na.rm = TRUE)


ConnBCA[c(7:10)] = NULL
head(ConnBCA)
```

Now mental health medical use
```{r}
mHMUseBase = ConnBCA[c(7:15)]
head(mHMUseBase)
write.csv(mHMUseBase, "mHMUseBase.csv", row.names = FALSE)
mHMUseBase = read.csv("mHMUseBase.csv", header= TRUE)
mHMUseBase = rowMeans(mHMUseBase, na.rm = TRUE)


mHMUseMonth6 = ConnBCA[c(16:24)]
head(mHMUseMonth6)
write.csv(mHMUseMonth6, "mHMUseMonth6.csv", row.names = FALSE)
mHMUseMonth6 = read.csv("mHMUseMonth6.csv", header= TRUE)
mHMUseMonth6 = rowMeans(mHMUseMonth6, na.rm = TRUE)


ConnBCA[c(7:15, 16:24)] = NULL

ConnBCA$mHMUseBase = mHMUseBase
ConnBCA$mHMUseMonth6 = mHMUseMonth6
ConnBCA$fsBase = fsBase
ConnBCA$fsMonth6 = fsMonth6
head(ConnBCA)
```


Now recombine the data into one data frame and rename everything
Can center stuff so do that 

Impute the data.  Don't specifcy, don't really care that much about other values only values the outcome. 

Cannot get compare.density to work not sure why would be helpful.
```{r}
head(CCPEBCA)
m=10

# Full model
a.out = amelia(x = CCPEBCA, m = 10)


#a.out = amelia(x = CCPEBCA, m=m, ords = c("CCPEBCA.RSKCIG.x", "CCPEBCA.RSKMJ.x", "CCPEBCA.RSKALC.x", "CCPEBCA.REL_IMP.x", "CCPEBCA.HINCOMEO_N.x", "CCPEBCA.YOB.x", "CCPEBCA.RSKCIG.y", "CCPEBCA.RSKMJ.y", "CCPEBCA.RSKALC.y", "CCPEBCA.REL_IMP.y", "CCPEBCA.HINCOMEO_N.y"), noms = c("CCPEBCA.R_WHITE_N.x", "CCPEBCA.SEX_PR.x", "CCPEBCA.GENDER.x"))
CCPEBCA1 = data.frame(na.omit(a.out$imputations$imp1))
CCPEBCA2 = data.frame(na.omit(a.out$imputations$imp2))
CCPEBCA3 = data.frame(na.omit(a.out$imputations$imp3))
CCPEBCA4 = data.frame(na.omit(a.out$imputations$imp4))
CCPEBCA5 = data.frame(na.omit(a.out$imputations$imp5))
CCPEBCA6 = data.frame(na.omit(a.out$imputations$imp6))
CCPEBCA7 = data.frame(na.omit(a.out$imputations$imp7))
CCPEBCA8 = data.frame(na.omit(a.out$imputations$imp8))
CCPEBCA9 = data.frame(na.omit(a.out$imputations$imp9))
CCPEBCA10 = data.frame(na.omit(a.out$imputations$imp10))



summary(a.out)
disperse(a.out, dims = 1, m = 5)
compare.density(a.out, var = "CCPEBCATotalBase")
compare.density(a.out, var = "CCPEBCATotalMonth3")
#overimpute(a.out, var = "CCPEBCATotalMonth3")
# Number of people after imputation
dim(CCPEBCA1)
```
Now get rid of any with 0 substance use on baseline, because they do not qualify
Will need to get the average number of people from the data sets to get the number of people for the BCA, because the number of zeros will be different for each data set, because zeros will be imputed differenetly.
```{r}
CCPEBCA1 = subset(CCPEBCA1, CCPEBCATotalBase > 0)
CCPEBCA2 = subset(CCPEBCA2, CCPEBCATotalBase > 0)
CCPEBCA3 = subset(CCPEBCA3, CCPEBCATotalBase > 0)
CCPEBCA4 = subset(CCPEBCA4, CCPEBCATotalBase > 0)
CCPEBCA5 = subset(CCPEBCA5, CCPEBCATotalBase > 0)
CCPEBCA6 = subset(CCPEBCA6, CCPEBCATotalBase > 0)
CCPEBCA7 = subset(CCPEBCA7, CCPEBCATotalBase > 0)
CCPEBCA8 = subset(CCPEBCA8, CCPEBCATotalBase > 0)
CCPEBCA9 = subset(CCPEBCA9, CCPEBCATotalBase > 0)
CCPEBCA10 = subset(CCPEBCA10, CCPEBCATotalBase > 0)
n = mean(dim(CCPEBCA1)[1], dim(CCPEBCA2)[1], dim(CCPEBCA3)[1], dim(CCPEBCA4)[1], dim(CCPEBCA5)[1], dim(CCPEBCA6)[1], dim(CCPEBCA7)[1], dim(CCPEBCA8)[1], dim(CCPEBCA9)[1], dim(CCPEBCA10)[1]); n

```


Now combine and get means and sds
```{r}
mean1 =apply(CCPEBCA1, 2, mean)
mean2 =apply(CCPEBCA2, 2, mean)
mean3 =apply(CCPEBCA3, 2, mean)
mean4 =apply(CCPEBCA4, 2, mean)
mean5 =apply(CCPEBCA5, 2, mean)
mean6 =apply(CCPEBCA6, 2, mean)
mean7 =apply(CCPEBCA7, 2, mean)
mean8 =apply(CCPEBCA8, 2, mean)
mean9 =apply(CCPEBCA9, 2, mean)
mean10 =apply(CCPEBCA10, 2, mean)


sd1 = apply(CCPEBCA1, 2, sd)
sd2 = apply(CCPEBCA2, 2, sd)
sd3 = apply(CCPEBCA3, 2, sd)
sd4 = apply(CCPEBCA4, 2, sd)
sd5 = apply(CCPEBCA5, 2, sd)
sd6 = apply(CCPEBCA6, 2, sd)
sd7 = apply(CCPEBCA7, 2, sd)
sd8 = apply(CCPEBCA8, 2, sd)
sd9 = apply(CCPEBCA9, 2, sd)
sd10 = apply(CCPEBCA10, 2, sd)


allMeans = t(as.matrix(cbind(mean1, mean2, mean3, mean4, mean5, mean6, mean7, mean8, mean9, mean10)))

allSDs = t(as.matrix(cbind(sd1, sd2, sd3, sd4, sd5, sd6, sd7, sd8, sd9, sd10)))

allMeansSDsCom = mi.meld(q = allMeans, se = allSDs)
allMeansSDsCom = t(as.data.frame(allMeansSDsCom))
allMeansSDsCom = data.frame(allMeansSDsCom[15:16,]) 
allMeansSDsCom = t(data.frame(allMeansSDsCom))
allMeansSDsCom = data.frame(allMeansSDsCom)
percentChange = (allMeansSDsCom$q.mi.CCPEBCATotalMonth3-allMeansSDsCom$q.mi.CCPEBCATotalBase)/allMeansSDsCom$q.mi.CCPEBCATotalBase; percentChange
```










