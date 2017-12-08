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


