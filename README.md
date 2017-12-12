##### Goal 1 Objective D ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### 
## Grab total number of people with a part ID.  Then subset to only people in 2017 for QPRY2Q4.  Need to get the year and month
setwd("C:/Users/Matthew.Hanauer/Desktop/")
head(GPRAAll)
Goal1ObjectiveConn = data.frame(GPRAAll$ParticipantID)
Goal1ObjectiveConn = 
Goal1ObjectiveConn  = dim(Goal1ObjectiveConn)
Goal1ObjectiveConn = data.frame(Goal1ObjectiveConn[1])
colnames(Goal1ObjectiveConn) = c("Total Enrolled")
write.csv(Goal1ObjectiveConn, "Goal1ObjectiveConn.csv", row.names = FALSE)
### Goal 3 Objective A ####### ####################################################################################
# Need to find the substance abuse variables and look at reductions AnyAlcohol
# If reusing CCPE get rid of BaseMonth6, BaseMonth6, change goal and objective to the correct one
Goal3ObjectiveA = data.frame(GPRAAll$AnyAlcohol.x, GPRAAll$AnyAlcohol.y)

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

# Get BaseMonth6 and then repeat for BaseMonth12
PHQ9BaseMonth6 = data.frame(PHQ9All$PHQ9Total.x, PHQ9All$PHQ9Total.y)
summary(PHQ9BaseMonth6)
dim(PHQ9BaseMonth6)
PHQ9BaseMonth6 = data.frame(na.omit(PHQ9BaseMonth6))
colnames(PHQ9BaseMonth6) =c("Base", "Month6")
dim(PHQ9BaseMonth6)
head(PHQ9BaseMonth6)
write.csv(PHQ9BaseMonth6, "PHQ9BaseMonth6.csv", row.names = FALSE)
PHQ9BaseMonth6 = read.csv("PHQ9BaseMonth6.csv", header = TRUE)
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

#### Goal 3 Objective C ######## ######## ################################################
## First need to figure who is a yes for the inpaitent stuff and who is a no.  Include the parID.  Then 
# Once I know who is a yes and no with part ID for all of the variables that I want to combine, then 
# I get rid of the missing data.  Then once the missing data is gone I merge the times variables by ID.  Then I know
# if there is a missing value for the times variable it is a no and not a missing and I can put a zero.
# What variables to include.  Not using DrugRelated, because almost all of the data is coded as missing. 
# So instead just using general variable TimesCrime as the crime indicator.

Goal3ObjectiveCBaseMonth6 = data.frame(ParticipantID = GPRAAll$ParticipantID, GPRAAll$InAlcohol.x, GPRAAll$InAlcohol.y, GPRAAll$OutAlcohol.x, GPRAAll$OutAlcohol.y, GPRAAll$ERAlcohol.x, GPRAAll$ERAlcohol.y)
summary(Goal3ObjectiveCBaseMonth6)
Goal3ObjectiveCBaseMonth6 = na.omit(Goal3ObjectiveCBaseMonth6)

Goal3ObjectiveCBaseMonth6Times = data.frame(ParticipantID = GPRAAll$ParticipantID, GPRAAll$TimesCrime.x,GPRAAll$TimesCrime.y, GPRAAll$InAlcoholTimes.x, GPRAAll$InAlcoholTimes.y, GPRAAll$OutAlcoholTimes.x, GPRAAll$OutAlcoholTimes.y, GPRAAll$ERAlcoholTimes.x, GPRAAll$ERAlcoholTimes.y)
Goal3ObjectiveCBaseMonth6 = merge(Goal3ObjectiveCBaseMonth6 , Goal3ObjectiveCBaseMonth6Times , by = "ParticipantID", all.x = TRUE)
head(Goal3ObjectiveCBaseMonth6)
summary(Goal3ObjectiveCBaseMonth6)
dim(Goal3ObjectiveCBaseMonth6)
Goal3ObjectiveCBaseMonth6 = data.frame(Goal3ObjectiveCBaseMonth6[,7:15])
# You can just sum across with na.rm = TRUE and the NA will be treated as zeros so you don't have to get rid of them.
Goal3ObjectiveCBaseMonth6 = data.frame(apply(Goal3ObjectiveCBaseMonth6, 2, function(x){ifelse(x == -99.0, 0,x)}))
summary(Goal3ObjectiveCBaseMonth6)
write.csv(Goal3ObjectiveCBaseMonth6, "Goal3ObjectiveCBaseMonth6.csv", row.names = FALSE)
Goal3ObjectiveCBaseMonth6  = read.csv("Goal3ObjectiveCBaseMonth6.csv", header = TRUE)
summary(Goal3ObjectiveCBaseMonth6)
### You need to change the Refused response to 0.
Goal3ObjectiveCBase = data.frame(Goal3ObjectiveCBaseMonth6$GPRAAll.TimesCrime.x, Goal3ObjectiveCBaseMonth6$GPRAAll.InAlcoholTimes.x, Goal3ObjectiveCBaseMonth6$GPRAAll.OutAlcoholTimes.x, Goal3ObjectiveCBaseMonth6$GPRAAll.ERAlcoholTimes.x)
Goal3ObjectiveCBase = data.frame(apply(Goal3ObjectiveCBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCBase) = c("SABase")

Goal3ObjectiveCMonth6= data.frame(Goal3ObjectiveCBaseMonth6$GPRAAll.TimesCrime.y,Goal3ObjectiveCBaseMonth6$GPRAAll.InAlcoholTimes.y, Goal3ObjectiveCBaseMonth6$GPRAAll.OutAlcoholTimes.y, Goal3ObjectiveCBaseMonth6$GPRAAll.ERAlcoholTimes.y)
write.csv(Goal3ObjectiveCMonth6, "Goal3ObjectiveCMonth6.csv", row.names = FALSE)
Goal3ObjectiveCMonth6 = read.csv("Goal3ObjectiveCMonth6.csv", header = TRUE)
Goal3ObjectiveCMonth6= data.frame(apply(Goal3ObjectiveCMonth6, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCMonth6) = c("SAMonth6")

Goal3ObjectiveCBaseMonth6 = data.frame(Goal3ObjectiveCMonth6, Goal3ObjectiveCBase)
colnames(Goal3ObjectiveCBaseMonth6) = c("SAMonth6", "SABase")
summary(Goal3ObjectiveCBaseMonth6)
wilcox.test(Goal3ObjectiveCBaseMonth6$SAMonth6, Goal3ObjectiveCBaseMonth6$SABase, paired = TRUE, alternative = c("less"))
## Can report the means, but they are statistically significantly different.
Goal3ObjectiveCBaseMonth6 = data.frame(t(colMeans(Goal3ObjectiveCBaseMonth6)))

Goal3ObjectiveCBaseMonth6$Difference = (Goal3ObjectiveCBaseMonth6$SAMonth6-Goal3ObjectiveCBaseMonth6$SABase)/ Goal3ObjectiveCBaseMonth6$SABase
Goal3ObjectiveCBaseMonth6 = round(Goal3ObjectiveCBaseMonth6, 2)
write.csv(Goal3ObjectiveCBaseMonth6, "Goal3ObjectiveCBaseMonth6.csv", row.names = FALSE)

#### Goal 3 Objective D ######## ######## ################################################
Goal3ObjectiveDBaseMonth6 = data.frame(ParticipantID = GPRAAll$ParticipantID, GPRAAll$InMental.x, GPRAAll$InMental.y, GPRAAll$OutMental.x, GPRAAll$OutMental.y, GPRAAll$ERMental.x, GPRAAll$ERMental.y)
summary(Goal3ObjectiveDBaseMonth6)
Goal3ObjectiveDBaseMonth6 = na.omit(Goal3ObjectiveDBaseMonth6)

Goal3ObjectiveDBaseMonth6Times = data.frame(ParticipantID = GPRAAll$ParticipantID, GPRAAll$TimesCrime.x,GPRAAll$TimesCrime.y, GPRAAll$InMentalTimes.x, GPRAAll$InMentalTimes.y, GPRAAll$OutMentalTimes.x, GPRAAll$OutMentalTimes.y, GPRAAll$ERMentalTimes.x, GPRAAll$ERMentalTimes.y)
Goal3ObjectiveDBaseMonth6 = merge(Goal3ObjectiveDBaseMonth6 , Goal3ObjectiveDBaseMonth6Times , by = "ParticipantID", all.x = TRUE)
head(Goal3ObjectiveDBaseMonth6)
summary(Goal3ObjectiveDBaseMonth6)
dim(Goal3ObjectiveDBaseMonth6)
Goal3ObjectiveDBaseMonth6 = data.frame(Goal3ObjectiveDBaseMonth6[,7:15])
# You can just sum across with na.rm = TRUE and the NA will be treated as zeros so you don't have to get rid of them.
Goal3ObjectiveDBaseMonth6 = data.frame(apply(Goal3ObjectiveDBaseMonth6, 2, function(x){ifelse(x == -99.0, 0,x)}))
summary(Goal3ObjectiveDBaseMonth6)
write.csv(Goal3ObjectiveDBaseMonth6, "Goal3ObjectiveDBaseMonth6.csv", row.names = FALSE)
Goal3ObjectiveDBaseMonth6  = read.csv("Goal3ObjectiveDBaseMonth6.csv", header = TRUE)
summary(Goal3ObjectiveDBaseMonth6)
### You need to change the Refused response to 0.
Goal3ObjectiveDBase = data.frame(Goal3ObjectiveDBaseMonth6$GPRAAll.TimesCrime.x, Goal3ObjectiveDBaseMonth6$GPRAAll.InMentalTimes.x, Goal3ObjectiveDBaseMonth6$GPRAAll.OutMentalTimes.x, Goal3ObjectiveDBaseMonth6$GPRAAll.ERMentalTimes.x)
Goal3ObjectiveDBase = data.frame(apply(Goal3ObjectiveDBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveDBase) = c("SABase")

Goal3ObjectiveDMonth6= data.frame(Goal3ObjectiveDBaseMonth6$GPRAAll.TimesCrime.y,Goal3ObjectiveDBaseMonth6$GPRAAll.InMentalTimes.y, Goal3ObjectiveDBaseMonth6$GPRAAll.OutMentalTimes.y, Goal3ObjectiveDBaseMonth6$GPRAAll.ERMentalTimes.y)
write.csv(Goal3ObjectiveDMonth6, "Goal3ObjectiveDMonth6.csv", row.names = FALSE)
Goal3ObjectiveDMonth6 = read.csv("Goal3ObjectiveDMonth6.csv", header = TRUE)
Goal3ObjectiveDMonth6= data.frame(apply(Goal3ObjectiveDMonth6, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveDMonth6) = c("SAMonth6")

Goal3ObjectiveDBaseMonth6 = data.frame(Goal3ObjectiveDMonth6, Goal3ObjectiveDBase)
colnames(Goal3ObjectiveDBaseMonth6) = c("SAMonth6", "SABase")
summary(Goal3ObjectiveDBaseMonth6)
wilcox.test(Goal3ObjectiveDBaseMonth6$SAMonth6, Goal3ObjectiveDBaseMonth6$SABase, paired = TRUE, alternative = c("less"))
## Can report the means, but they are statistically significantly different.
Goal3ObjectiveDBaseMonth6 = data.frame(t(colMeans(Goal3ObjectiveDBaseMonth6)))

Goal3ObjectiveDBaseMonth6$Difference = (Goal3ObjectiveDBaseMonth6$SAMonth6-Goal3ObjectiveDBaseMonth6$SABase)/ Goal3ObjectiveDBaseMonth6$SABase
Goal3ObjectiveDBaseMonth6 = round(Goal3ObjectiveDBaseMonth6, 2)
write.csv(Goal3ObjectiveDBaseMonth6, "Goal3ObjectiveDBaseMonth6.csv", row.names = FALSE)

#### Goal 3 Objective E ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### 
# Need to sum the following variables: TANF, IWAP, Medicaid, LegalAid, SSI, TTA, FoodBanks, HUD, Area10, RecoveryWorks, SNAP, WIC
setwd("C:/Users/Matthew.Hanauer/Desktop")
write.csv(benefitsAll, "benefitsAll.csv", row.names  = FALSE)
Goal3ObjectiveEBaseMonth6 = data.frame(benefitsAll$TANF, benefitsAll$IWAP, benefitsAll$Medicaid, benefitsAll$LegalAid, 
benefitsAll$SSI, benefitsAll$TTA, benefitsAll$FoodBanks, benefitsAll$HUD, benefitsAll$Area10, benefitsAll$RecoveryWorks, 
benefitsAll$SNAP, benefitsAll$WIC, benefitsAll$RATANF, benefitsAll$RAIWAP, benefitsAll$RAMedicaid, benefitsAll$RALegalAid, 
benefitsAll$RASSI, benefitsAll$RATTA, benefitsAll$RAFoodBanks, benefitsAll$RAHUD, benefitsAll$RAArea10, benefitsAll$RARecoveryWorks, 
benefitsAll$RASNAP, benefitsAll$RAWIC)
Goal3ObjectiveEBaseMonth6 = data.frame(na.omit(Goal3ObjectiveEBaseMonth6))
dim(Goal3ObjectiveEBaseMonth6)
# Only one person so need more data

#### Goal 4 Objective C ########### #############################################################################
# Grab employed variable.  Then 1 and 2 are 1 for employed, 8, 9, and 10 are NA everything else is 0.
Goal4ObjectiveC = data.frame(GPRAAll$Employed.x, GPRAAll$Employed.y)
Goal4ObjectiveC = data.frame(apply(Goal4ObjectiveC , 2, function(x){ifelse(x == 1, 1, ifelse(x == 2, 1, ifelse(x == 8, NA, ifelse(x == 9, NA, ifelse(x == 10, NA, 0)))))}))
Goal4ObjectiveC = data.frame(na.omit(Goal4ObjectiveC))
dim(Goal4ObjectiveC)
Goal4ObjectiveC = data.frame(t(apply(Goal4ObjectiveC , 2, sum)))
colnames(Goal4ObjectiveC)= c("Base", "Month6")
Goal4ObjectiveC$Difference = (Goal4ObjectiveC$Month6-Goal4ObjectiveC$Base)/Goal4ObjectiveC$Base
write.csv(Goal4ObjectiveC, "Goal4ObjectiveC.csv", row.names = FALSE)
### Goal 4 Objective D ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
# Could create a composite score:  If you want to include OverallHealth it can be done, just is a little trickier
# So adding to binary variables FamilySupport and SelfHelp where two means yes to both 1 means yes to one and zero means no to both
# No variables like SatisfiedHealth, because there is not enough data
Goal4ObjectiveD =  data.frame(GPRAAll$FamilySupport.x, GPRAAll$SelfHelp.x, GPRAAll$FamilySupport.y, GPRAAll$SelfHelp.y)
Goal4ObjectiveD = data.frame(apply(Goal4ObjectiveD, 2, function(x){ifelse(x == 3, NA, ifelse(x == 4,NA, ifelse(x == 2, 0, x)))}))
Goal4ObjectiveD = na.omit(Goal4ObjectiveD)
dim(Goal4ObjectiveD)
summary(Goal4ObjectiveD)

Goal4ObjectiveDBase = data.frame(Goal4ObjectiveD[,1:2])
Goal4ObjectiveDBase = data.frame(apply(Goal4ObjectiveDBase, 1, sum))
colnames(Goal4ObjectiveDBase) = c("Base")
head(Goal4ObjectiveDBase)

Goal4ObjectiveDMonth6 = data.frame(Goal4ObjectiveD[,3:4])
Goal4ObjectiveDMonth6 = data.frame(apply(Goal4ObjectiveDMonth6, 1, sum))
colnames(Goal4ObjectiveDMonth6) = c("Month6")
head(Goal4ObjectiveDMonth6)

Goal4ObjectiveD = data.frame(Goal4ObjectiveDBase, Goal4ObjectiveDMonth6)
head(Goal4ObjectiveD)
wilcox.test(Goal4ObjectiveD$Month6, Goal4ObjectiveD$Base, paired = TRUE, alternative = c("greater"))

Goal4ObjectiveD = data.frame(t(colMeans(Goal4ObjectiveD)))
Goal4ObjectiveD$Difference = (Goal4ObjectiveD$Month6-Goal4ObjectiveD$Base)/ Goal4ObjectiveD$Base
Goal4ObjectiveD = round(Goal4ObjectiveD, 2)
write.csv(Goal4ObjectiveD, "Goal4ObjectiveD.csv", row.names = FALSE)


##### Goal 5 Objective B ###### ##########################################
### Just get demographcs from baseline: White, Black, Hispanic, Asian
Goal5ObjectiveBEth = data.frame(GPRAAll$White.x, GPRAAll$HispanicLatino.x, GPRAAll$Black.x, GPRAAll$Asian.x)
Goal5ObjectiveBEth = data.frame(apply(Goal5ObjectiveBEth, 2, function(x){ifelse(x == 2, 0, x)}))
colnames(Goal5ObjectiveBEth) = c("White", "Hispanic", "Black", "Asian")
n = dim(Goal5ObjectiveBEth)
n = n[1]
Goal5ObjectiveBEth = data.frame(apply(Goal5ObjectiveBEth, 2, sum, na.rm = TRUE))
colnames(Goal5ObjectiveBEth) = c("Total")
Goal5ObjectiveBEth$Percentage = Goal5ObjectiveBEth$Total/n
Goal5ObjectiveBEth = round(Goal5ObjectiveBEth,2)
write.csv(Goal5ObjectiveBEth, "Goal5ObjectiveBEth.csv", row.names = FALSE)

#### Project Objectives ######## ########################################################################
# 1 grab the number of people in the program
# 2 Goal3ObjectiveA = Project Objective 2
#Goal3ObjectiveB = Project Objective 3
#Goal3ObjectiveC = Project Objective 5
#Goal2ObjectiveD = Project Objective 6

#### Demographics ######### ######### ######### ######### ######### ######### ######### ######### ######### 

#Ethnicity = Goal 5 Objective B

gender = data.frame(GPRAAll$Gender.x)
n = dim(gender)
n = n[1]
gender  = count(gender)
gender$Percentage = gender$freq/n
gender

# Education
edu = data.frame(GPRAAll$Education.x)
# 0-11 is less than high school; 12 is high school; 13-15 is some college; 16 is bachelorr or higher, 17 and 18 ar vocal
#Less than high school = 1; 2 = High School; 3 = Some College, 4 = College Degree or higher; 5  = Vocational 
edu = data.frame(apply(edu, 2, function(x){ifelse(x < 12, 1, ifelse(x == 12, 2, ifelse(x == 13, 3, ifelse(x == 14, 3, ifelse(x == 15, 3, ifelse(x == 16, 4, ifelse(x == 17, 5, ifelse(x == 18, 5,x))))))))}))
edu = count(edu)
edu$Percentage = edu$freq/n
edu = round(edu, 2)
edu

######################### Centerstone Metrics ################### ############################################################################
# Goal3 Objective A for Substance Abuse
#Goal3ObjectiveB = Project Objective 3 for Depression
# ER Useage 
Goal3ObjectiveDBaseMonth6 = data.frame(ParticipantID = GPRAAll$ParticipantID, GPRAAll$ERMental.x, GPRAAll$ERMental.y, GPRAAll$ERAlcohol.x, GPRAAll$ERAlcohol.y)
summary(Goal3ObjectiveDBaseMonth6)
Goal3ObjectiveDBaseMonth6 = na.omit(Goal3ObjectiveDBaseMonth6)

Goal3ObjectiveDBaseMonth6Times = data.frame(ParticipantID = GPRAAll$ParticipantID, GPRAAll$ERMentalTimes.x, GPRAAll$ERMentalTimes.y, GPRAAll$ERAlcoholTimes.x, GPRAAll$ERAlcoholTimes.y)
Goal3ObjectiveDBaseMonth6 = merge(Goal3ObjectiveDBaseMonth6 , Goal3ObjectiveDBaseMonth6Times , by = "ParticipantID", all.x = TRUE)
head(Goal3ObjectiveDBaseMonth6)
summary(Goal3ObjectiveDBaseMonth6)
dim(Goal3ObjectiveDBaseMonth6)
Goal3ObjectiveDBaseMonth6 = data.frame(Goal3ObjectiveDBaseMonth6[,6:9])
# You can just sum across with na.rm = TRUE and the NA will be treated as zeros so you don't have to get rid of them.
Goal3ObjectiveDBaseMonth6 = data.frame(apply(Goal3ObjectiveDBaseMonth6, 2, function(x){ifelse(x == -99.0, 0,x)}))
summary(Goal3ObjectiveDBaseMonth6)
write.csv(Goal3ObjectiveDBaseMonth6, "Goal3ObjectiveDBaseMonth6.csv", row.names = FALSE)
Goal3ObjectiveDBaseMonth6  = read.csv("Goal3ObjectiveDBaseMonth6.csv", header = TRUE)
summary(Goal3ObjectiveDBaseMonth6)
### You need to change the Refused response to 0.
Goal3ObjectiveDBase = data.frame(Goal3ObjectiveDBaseMonth6$GPRAAll.ERMentalTimes.x, Goal3ObjectiveDBaseMonth6$GPRAAll.ERAlcoholTimes.x)
Goal3ObjectiveDBase = data.frame(apply(Goal3ObjectiveDBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveDBase) = c("SABase")

Goal3ObjectiveDMonth6= data.frame(Goal3ObjectiveDBaseMonth6$GPRAAll.ERMentalTimes.y, Goal3ObjectiveDBaseMonth6$GPRAAll.ERAlcoholTimes.y)
write.csv(Goal3ObjectiveDMonth6, "Goal3ObjectiveDMonth6.csv", row.names = FALSE)
Goal3ObjectiveDMonth6 = read.csv("Goal3ObjectiveDMonth6.csv", header = TRUE)
Goal3ObjectiveDMonth6= data.frame(apply(Goal3ObjectiveDMonth6, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveDMonth6) = c("SAMonth6")

Goal3ObjectiveDBaseMonth6 = data.frame(Goal3ObjectiveDMonth6, Goal3ObjectiveDBase)
colnames(Goal3ObjectiveDBaseMonth6) = c("SAMonth6", "SABase")
summary(Goal3ObjectiveDBaseMonth6)

Goal3ObjectiveDBaseMonth6 = data.frame(t(colMeans(Goal3ObjectiveDBaseMonth6)))

Goal3ObjectiveDBaseMonth6$Difference = (Goal3ObjectiveDBaseMonth6$SAMonth6-Goal3ObjectiveDBaseMonth6$SABase)/ Goal3ObjectiveDBaseMonth6$SABase
Goal3ObjectiveDBaseMonth6 = round(Goal3ObjectiveDBaseMonth6, 2)
write.csv(Goal3ObjectiveDBaseMonth6, "Goal3ObjectiveDBaseMonth6.csv", row.names = FALSE)


