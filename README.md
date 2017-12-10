##### Goal 1 Objective D ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### 
## Grab total number of people with a part ID.  Then subset to only people in 2017 for QPRY2Q4.  Need to get the year and month
setwd("C:/Users/Matthew.Hanauer/Desktop/")
head(GPRAAll)
Goal1ObjectiveConn = data.frame(GPRAAll$ParticipantID)
Goal1ObjectiveConn = 
Goal1ObjectiveConn  = dim(Goal1ObjectiveConn)
Goal1ObjectiveConn = data.frame(Goal1ObjectiveConn[1])
colnames("Goal1ObjectiveConn") = c("Total Enrolled")
write.csv(Goal1ObjectiveConn, "Goal1ObjectiveConn.csv", row.names = FALSE)
### Goal 3 Objective A ####### ####################################################################################
# Need to find the substance abuse variables and look at reductions AnyAlcohol
# If reusing CCPE get rid of BaseMonth6, BaseMonth6, change goal and objective to the correct one
library("plyr")
Goal3ObjectiveA = data.frame(GPRAAll$AnyAlcohol.x, GPRAAll$AnyAlcohol.y)
head(Goal3ObjectiveAConn)
summary(Goal3ObjectiveAConn) 
Goal3ObjectiveA  = data.frame(apply(Goal3ObjectiveA, 2, function(x){ifelse(x == -99, NA, x)}))
Goal3ObjectiveA  = na.omit(Goal3ObjectiveA)
dim(Goal3ObjectiveA)
# I think I alter this data to have 1's and zero's 
Goal3ObjectiveA  = data.frame(apply(Goal3ObjectiveA, 2, function(x){ifelse(x > 0, 1, 0)}))
head(Goal3ObjectiveA)
summary(Goal3ObjectiveA)
# Want to see baseline go down so see X go down relative to y so x less y
wilcox.test(Goal3ObjectiveA$GPRAAll.AnyAlcohol.y,Goal3ObjectiveA$GPRAAll.AnyAlcohol.x, paired = TRUE, alternative  =c("less"))

Goal3ObjectiveA = t(data.frame(colMeans(Goal3ObjectiveA)))
colnames(Goal3ObjectiveA) = c("Base", "Month6")
Goal3ObjectiveA = data.frame(Goal3ObjectiveA)
Goal3ObjectiveA$Difference = (Goal3ObjectiveA$Month6-Goal3ObjectiveA$Base)/Goal3ObjectiveA$Base
Goal3ObjectiveA = round(Goal3ObjectiveA,2)
write.csv(Goal3ObjectiveA, "Goal3ObjectiveA.csv", row.names = FALSE) 


####### Goal 3 Objective B ####### #############################################################################
# Grab PHQ-9 and GAD-7 scores for this indicator.
head(PHQ9All)
head(PHQ9All$PHQ9Total)
# Get BaseMonth6 and then repeat for BaseMonth12
PHQ9BaseMonth6 = data.frame(PHQ9All$PHQ9Total.x, PHQ9All$PHQ9Total.y)
summary(PHQ9BaseMonth6)
dim(PHQ9BaseMonth6)
PHQ9BaseMonth6 = data.frame(na.omit(PHQ9BaseMonth6))
colnames(PHQ9BaseMonth6) =c("Base", "Month6")
dim(PHQ9BaseMonth6)
head(PHQ9BaseMonth6)
wilcox.test(PHQ9BaseMonth6$Month6, PHQ9BaseMonth6$Base, paired = TRUE, alternative = c("less"))
PHQ9BaseMonth6 = colMeans(PHQ9BaseMonth6)
PHQ9BaseMonth6 = data.frame(t(PHQ9BaseMonth6))
PHQ9BaseMonth6$Difference = (PHQ9BaseMonth6$Month6-PHQ9BaseMonth6$Base)/ PHQ9BaseMonth6$Base
PHQ9BaseMonth6 = round(PHQ9BaseMonth6,2)
write.csv(PHQ9BaseMonth6, "PHQ9BaseMonth6.csv", row.names = FALSE)

## Just replace PHQ9 with GAD7
head(GAD7All)
GAD7BaseMonth6 = data.frame(GAD7All$GAD7Total.x, GAD7All$GAD7Total.y)
summary(GAD7BaseMonth6)
dim(GAD7BaseMonth6)
GAD7BaseMonth6 = data.frame(na.omit(GAD7BaseMonth6))
colnames(GAD7BaseMonth6) =c("Base", "Month6")
dim(GAD7BaseMonth6)
head(GAD7BaseMonth6)
wilcox.test(GAD7BaseMonth6$Month6, GAD7BaseMonth6$Base, paired = TRUE, alternative = c("less"))
GAD7BaseMonth6 = colMeans(GAD7BaseMonth6)
GAD7BaseMonth6 = data.frame(t(GAD7BaseMonth6))
GAD7BaseMonth6$Difference = (GAD7BaseMonth6$Month6-GAD7BaseMonth6$Base)/ GAD7BaseMonth6$Base
GAD7BaseMonth6 = round(GAD7BaseMonth6,2)
write.csv(GAD7BaseMonth6, "GAD7BaseMonth6.csv", row.names = FALSE)

#### 
