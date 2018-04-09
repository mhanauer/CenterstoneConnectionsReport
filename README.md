### Goal 1 Objective B ##### 
# 4-9 are housed
Goal1ObjectiveB = data.frame(GPRAAll$Living.y)
Goal1ObjectiveB = data.frame(apply(Goal1ObjectiveB, 2, function(x){ifelse(x == 4, 1, ifelse(x == 5, 1, ifelse(x == 6, 1, ifelse(x == 7, 1, ifelse(x == 8, 1, ifelse(x == 9, 1, 0))))))}))
library(plyr)
Goal1ObjectiveB = data.frame(count(Goal1ObjectiveB))
n = data.frame(apply(Goal1ObjectiveB, 2, sum))
n = n[2,1]
Goal1ObjectiveB$Percent = Goal1ObjectiveB$freq/n
Goal1ObjectiveB

##### Goal 1 Objective D ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### 
## Grab total number of people with a part ID.  Then subset to only people in 2017 for QPRY2Q4.  Need to get the year and month
# Total
Goal1ObjectiveConn = data.frame(GPRAAll$ParticipantID)
Goal1ObjectiveConn  = dim(Goal1ObjectiveConn)
Goal1ObjectiveConn = data.frame(Goal1ObjectiveConn[1])
colnames(Goal1ObjectiveConn) = c("Total Enrolled")
write.csv(Goal1ObjectiveConn, "Goal1ObjectiveConn.csv", row.names = FALSE)

# Year 1
Goal1ObjectiveConn = data.frame(GPRAAll$ParticipantID)
Goal1ObjectiveConn = data.frame(subset(Goal1ObjectiveConn, GPRAAll.ParticipantID < 2000))
Goal1ObjectiveConn  = dim(Goal1ObjectiveConn)
Goal1ObjectiveConn = data.frame(Goal1ObjectiveConn[1])
colnames(Goal1ObjectiveConn) = c("Total Enrolled")
write.csv(Goal1ObjectiveConn, "Goal1ObjectiveConn.csv", row.names = FALSE)

### Year 2
Goal1ObjectiveConn = data.frame(GPRAAll$ParticipantID)
Goal1ObjectiveConn = data.frame(subset(Goal1ObjectiveConn, GPRAAll.ParticipantID > 1999 & GPRAAll.ParticipantID < 3000))
Goal1ObjectiveConn  = dim(Goal1ObjectiveConn)
Goal1ObjectiveConn = data.frame(Goal1ObjectiveConn[1])
colnames(Goal1ObjectiveConn) = c("Total Enrolled")
write.csv(Goal1ObjectiveConn, "Goal1ObjectiveConn.csv", row.names = FALSE)

#### Year 3
Goal1ObjectiveConn = data.frame(GPRAAll$ParticipantID)
Goal1ObjectiveConn = data.frame(subset(Goal1ObjectiveConn, GPRAAll.ParticipantID > 2999))
Goal1ObjectiveConn  = dim(Goal1ObjectiveConn)
Goal1ObjectiveConn = data.frame(Goal1ObjectiveConn[1])
colnames(Goal1ObjectiveConn) = c("Total Enrolled")
write.csv(Goal1ObjectiveConn, "Goal1ObjectiveConn.csv", row.names = FALSE)

### Goal 3 Objective A ####### ####################################################################################
# Need to find the substance abuse variables and look at reductions AnyAlcohol
DAUseAlcoholDays.x = data.frame(GPRAAll$DAUseAlcoholDays.x)
DAUseAlcoholDays.y = data.frame(GPRAAll$DAUseAlcoholDays.y)

Goal3ObjectiveA = data.frame(GPRAAll.DAUseAlcoholDays.x, DAUseAlcoholDays.y)
summary(Goal3ObjectiveA)
colnames(Goal3ObjectiveA) =c("DAUseAlcoholDays.x", "DAUseAlcoholDays.y")
Goal3ObjectiveA  = data.frame(apply(Goal3ObjectiveA, 2, function(x){ifelse(x < 0, NA, x)}))
Goal3ObjectiveA  = data.frame(Goal3ObjectiveA)
Goal3ObjectiveA  = na.omit(Goal3ObjectiveA)
dim(Goal3ObjectiveA)
summary(Goal3ObjectiveA)


# I think I alter this data to have 1's and zero's 
write.csv(Goal3ObjectiveA, "Goal3ObjectiveA.csv", row.names = FALSE)
Goal3ObjectiveA = read.csv("Goal3ObjectiveA.csv", header = TRUE)
#Goal3ObjectiveA  = data.frame(apply(Goal3ObjectiveA, 2, function(x){ifelse(x > 0, 1, 0)}))
summary(Goal3ObjectiveA)
# Want to see baseline go down so see X go down relative to y so x less y
wilcox.test(Goal3ObjectiveA$DAUseAlcoholDays.y,Goal3ObjectiveA$DAUseAlcoholDays.x, paired = TRUE, alternative  =c("less"))

Goal3ObjectiveA = t(data.frame(colMeans(Goal3ObjectiveA)))
colnames(Goal3ObjectiveA) = c("Base", "Month6")
Goal3ObjectiveA = data.frame(Goal3ObjectiveA)
Goal3ObjectiveA$Difference = (Goal3ObjectiveA$Month6-Goal3ObjectiveA$Base)/Goal3ObjectiveA$Base
Goal3ObjectiveA = round(Goal3ObjectiveA,2)


### Goal 3 Objective A for Drugs ####### ###############################################################
### Need to get rid of NAs then sum for each variable, which means separating out the variables then recombing them.
Goal3ObjectiveADrugs = data.frame(GPRAAll$DAUseIllegDrugsDays.x, GPRAAll$CocaineCrackDays.x, GPRAAll$MarijuanaHashDays.x, GPRAAll$OpiatesHeroinDays.x,  GPRAAll$OpiatesMorphineDays.x,GPRAAll$OpiatesDiluadidDays.x, GPRAAll$OpiatesDemerolDays.x, GPRAAll$OpiatesPercocetDays.x, GPRAAll$OpiatesDarvonDays.x, GPRAAll$OpiatesCodeineDays.x, GPRAAll$OpiatesOxycoDays.x, GPRAAll$NonPresMethadoneDays.x, GPRAAll$HallucPsychDays.x,  GPRAAll$MethamDays.x,  GPRAAll$BenzodiazepinesDays.x, GPRAAll$BarbituatesDays.x, GPRAAll$NonPrescGhbDays.x, GPRAAll$KetamineDays.x, GPRAAll$OtherTranquilizersDays.x, GPRAAll$InhalantsDays.x, GPRAAll$OtherIllegalDrugsDays.x, GPRAAll$DAUseIllegDrugsDays.y, GPRAAll$CocaineCrackDays.y, GPRAAll$MarijuanaHashDays.y, GPRAAll$OpiatesHeroinDays.y,  GPRAAll$OpiatesMorphineDays.y,GPRAAll$OpiatesDiluadidDays.y, GPRAAll$OpiatesDemerolDays.y, GPRAAll$OpiatesPercocetDays.y, GPRAAll$OpiatesDarvonDays.y, GPRAAll$OpiatesCodeineDays.y, GPRAAll$OpiatesOxycoDays.y, GPRAAll$NonPresMethadoneDays.y, GPRAAll$HallucPsychDays.y,  GPRAAll$MethamDays.y,  GPRAAll$BenzodiazepinesDays.y, GPRAAll$BarbituatesDays.y, GPRAAll$NonPrescGhbDays.y, GPRAAll$KetamineDays.y, GPRAAll$OtherTranquilizersDays.y, GPRAAll$InhalantsDays.y, GPRAAll$OtherIllegalDrugsDays.y)
typeof(Goal3ObjectiveADrugs)
write.csv(Goal3ObjectiveADrugs, "Goal3ObjectiveADrugs.csv", row.names = FALSE)
Goal3ObjectiveADrugs = read.csv("Goal3ObjectiveADrugs.csv", header = TRUE)
# It is ok to make everything NA zero, because we are summing.
Goal3ObjectiveADrugs = data.frame(apply(Goal3ObjectiveADrugs, 2, function(x){ifelse(x < 0, 0, x)}))
Goal3ObjectiveADrugs = data.frame(na.omit(Goal3ObjectiveADrugs))
summary(Goal3ObjectiveADrugs)
Goal3ObjectiveADrugs = data.frame(na.omit(Goal3ObjectiveADrugs))
dim(Goal3ObjectiveADrugs)
summary(Goal3ObjectiveADrugs)
# Grab first 21 for baseline then last 21 for six month sum and then bring together and look at difference
Goal3ObjectiveADrugsBase = data.frame(Goal3ObjectiveADrugs[,1:21])
summary(Goal3ObjectiveADrugsBase)
Goal3ObjectiveADrugsBase = rowSums(Goal3ObjectiveADrugsBase)

Goal3ObjectiveADrugsMonth6 = Goal3ObjectiveADrugs[,22:42]
Goal3ObjectiveADrugsMonth6 = data.frame(apply(Goal3ObjectiveADrugsMonth6, 1, sum))
colnames(Goal3ObjectiveADrugsMonth6) = c("Month6")
head(Goal3ObjectiveADrugsMonth6)
Goal3ObjectiveADrugs =data.frame(Goal3ObjectiveADrugsBase, Goal3ObjectiveADrugsMonth6)
colnames(Goal3ObjectiveADrugs) = c("Base", "Month6")
wilcox.test(Goal3ObjectiveADrugs$Month6, Goal3ObjectiveADrugs$Base, paired = TRUE, alternative = c("less"))
Goal3ObjectiveADrugs = data.frame(t(colMeans(Goal3ObjectiveADrugs)))
Goal3ObjectiveADrugs
Goal3ObjectiveADrugs$Difference  = (Goal3ObjectiveADrugs$Month6-Goal3ObjectiveADrugs$Base)/Goal3ObjectiveADrugs$Base
Goal3ObjectiveADrugs = round(Goal3ObjectiveADrugs,2)
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
# So need to change the variables names in the loading data 


Goal3ObjectiveCBaseMonth6 = data.frame(ClientID = GPRAAll$ClientID, GPRAAll$InpatientAlcoholSA.x, GPRAAll$InpatientAlcoholSA.y, GPRAAll$OutpatientAlcoholSA.x, GPRAAll$OutpatientAlcoholSA.y, GPRAAll$ERAlcoholSA.x, GPRAAll$ERAlcoholSA.y)
summary(Goal3ObjectiveCBaseMonth6)
Goal3ObjectiveCBaseMonth6 = data.frame(na.omit(Goal3ObjectiveCBaseMonth6))

Goal3ObjectiveCBaseMonth6Times = data.frame(ClientID = GPRAAll$ClientID,GPRAAll$InpatientAlcoholSANights.x, GPRAAll$InpatientAlcoholSANights.y, GPRAAll$OutpatientAlcoholSATimes.x, GPRAAll$OutpatientAlcoholSATimes.y, GPRAAll$ERAlcoholSATimes.x, GPRAAll$ERAlcoholSATimes.y)
Goal3ObjectiveCBaseMonth6 = merge(Goal3ObjectiveCBaseMonth6 , Goal3ObjectiveCBaseMonth6Times , by = "ClientID", all.x = TRUE)

head(Goal3ObjectiveCBaseMonth6)
summary(Goal3ObjectiveCBaseMonth6)
dim(Goal3ObjectiveCBaseMonth6)
Goal3ObjectiveCBaseMonth6 = data.frame(Goal3ObjectiveCBaseMonth6[,7:15])
head(Goal3ObjectiveCBaseMonth6)
Goal3ObjectiveCBaseMonth6 = data.frame(apply(Goal3ObjectiveCBaseMonth6, 2, function(x){ifelse(x == "-7", 0, x)}))


# You can just sum across with na.rm = TRUE and the NA will be treated as zeros so you don't have to get rid of them.
Goal3ObjectiveCBaseMonth6 = data.frame(apply(Goal3ObjectiveCBaseMonth6, 2, function(x){ifelse(x == -99.0, 0, ifelse(x == -7, 0,x))}))
summary(Goal3ObjectiveCBaseMonth6)
write.csv(Goal3ObjectiveCBaseMonth6, "Goal3ObjectiveCBaseMonth6.csv", row.names = FALSE)
Goal3ObjectiveCBaseMonth6  = read.csv("Goal3ObjectiveCBaseMonth6.csv", header = TRUE)
summary(Goal3ObjectiveCBaseMonth6)
### You need to change the Refused response to 0.  TimesCrime person 1 was Refused
Goal3ObjectiveCBaseMonth6 = data.frame(apply(Goal3ObjectiveCBaseMonth6, 2, function(x){ifelse(x == -1, 0, x)}))
head(Goal3ObjectiveCBaseMonth6)
summary(Goal3ObjectiveCBaseMonth6)


Goal3ObjectiveCBase = data.frame(Goal3ObjectiveCBaseMonth6$GPRAAll.InpatientAlcoholSANights.x, Goal3ObjectiveCBaseMonth6$GPRAAll.OutpatientAlcoholSATimes.x, Goal3ObjectiveCBaseMonth6$GPRAAll.ERAlcoholSATimes.x)
summary(Goal3ObjectiveCBase)
Goal3ObjectiveCBase
Goal3ObjectiveCBase = data.frame(apply(Goal3ObjectiveCBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCBase) = c("SABase")

Goal3ObjectiveCMonth6= data.frame(Goal3ObjectiveCBaseMonth6$GPRAAll.InpatientAlcoholSANights.y, Goal3ObjectiveCBaseMonth6$GPRAAll.OutpatientAlcoholSATimes.y, Goal3ObjectiveCBaseMonth6$GPRAAll.ERAlcoholSATimes.y)
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

Goal3ObjectiveDBaseMonth6 = data.frame(ClientID = GPRAAll$ClientID, GPRAAll$InpatientMental.x, GPRAAll$InpatientMental.y, GPRAAll$OutpatientMental.x, GPRAAll$OutpatientMental.y, GPRAAll$ERMental.x, GPRAAll$ERMental.y)
summary(Goal3ObjectiveDBaseMonth6)
Goal3ObjectiveDBaseMonth6 = data.frame(na.omit(Goal3ObjectiveDBaseMonth6))

Goal3ObjectiveDBaseMonth6Times = data.frame(ClientID = GPRAAll$ClientID,GPRAAll$InpatientMentalNights.x, GPRAAll$InpatientMentalNights.y, GPRAAll$OutpatientMentalTimes.x, GPRAAll$OutpatientMentalTimes.y, GPRAAll$ERMentalTimes.x, GPRAAll$ERMentalTimes.y)
Goal3ObjectiveDBaseMonth6 = merge(Goal3ObjectiveDBaseMonth6 , Goal3ObjectiveDBaseMonth6Times , by = "ClientID", all.x = TRUE)

Goal3ObjectiveDBaseMonth6 = data.frame(ClientID = GPRAAll$ClientID, GPRAAll$InpatientMental.x, GPRAAll$InpatientMental.y, GPRAAll$OutpatientMental.x, GPRAAll$OutpatientMental.y, GPRAAll$ERMental.x, GPRAAll$ERMental.y)
summary(Goal3ObjectiveDBaseMonth6)
Goal3ObjectiveDBaseMonth6 = data.frame(na.omit(Goal3ObjectiveDBaseMonth6))

Goal3ObjectiveDBaseMonth6Times = data.frame(ClientID = GPRAAll$ClientID,GPRAAll$InpatientMentalNights.x, GPRAAll$InpatientMentalNights.y, GPRAAll$OutpatientMentalTimes.x, GPRAAll$OutpatientMentalTimes.y, GPRAAll$ERMentalTimes.x, GPRAAll$ERMentalTimes.y)
Goal3ObjectiveDBaseMonth6 = merge(Goal3ObjectiveDBaseMonth6 , Goal3ObjectiveDBaseMonth6Times , by = "ClientID", all.x = TRUE)

head(Goal3ObjectiveDBaseMonth6)
summary(Goal3ObjectiveDBaseMonth6)
dim(Goal3ObjectiveDBaseMonth6)
head(Goal3ObjectiveDBaseMonth6)
Goal3ObjectiveDBaseMonth6 = data.frame(Goal3ObjectiveDBaseMonth6[,8:13])
head(Goal3ObjectiveDBaseMonth6)


# You can just sum across with na.rm = TRUE and the NA will be treated as zeros so you don't have to get rid of them.
Goal3ObjectiveDBaseMonth6 = data.frame(apply(Goal3ObjectiveDBaseMonth6, 2, function(x){ifelse(x == -99.0, 0, ifelse(x == -7, 0,x))}))
summary(Goal3ObjectiveDBaseMonth6)
write.csv(Goal3ObjectiveDBaseMonth6, "Goal3ObjectiveDBaseMonth6.csv", row.names = FALSE)
Goal3ObjectiveDBaseMonth6  = read.csv("Goal3ObjectiveDBaseMonth6.csv", header = TRUE)
summary(Goal3ObjectiveDBaseMonth6)
### You need to change the Refused response to 0.  TimesCrime person 1 was Refused
Goal3ObjectiveDBaseMonth6 = data.frame(apply(Goal3ObjectiveDBaseMonth6, 2, function(x){ifelse(x == -1, 0, x)}))
head(Goal3ObjectiveDBaseMonth6)
summary(Goal3ObjectiveDBaseMonth6)


Goal3ObjectiveDBase = data.frame(Goal3ObjectiveDBaseMonth6$GPRAAll.InpatientMentalNights.x, Goal3ObjectiveDBaseMonth6$GPRAAll.OutpatientMentalTimes.x, Goal3ObjectiveDBaseMonth6$GPRAAll.ERMentalTimes.x)
summary(Goal3ObjectiveDBase)
Goal3ObjectiveDBase
Goal3ObjectiveDBase = data.frame(apply(Goal3ObjectiveDBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveDBase) = c("SABase")

Goal3ObjectiveDMonth6= data.frame(Goal3ObjectiveDBaseMonth6$GPRAAll.InpatientMentalNights.y, Goal3ObjectiveDBaseMonth6$GPRAAll.OutpatientMentalTimes.y, Goal3ObjectiveDBaseMonth6$GPRAAll.ERMentalTimes.y)
write.csv(Goal3ObjectiveDMonth6, "Goal3ObjectiveDMonth6.csv", row.names = FALSE)
Goal3ObjectiveDMonth6 = read.csv("Goal3ObjectiveDMonth6.csv", header = TRUE)
Goal3ObjectiveDMonth6= data.frame(apply(Goal3ObjectiveDMonth6, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveDMonth6) = c("SAMonth6")

Goal3ObjectiveDBaseMonth6 = data.frame(Goal3ObjectiveDMonth6, Goal3ObjectiveDBase)
colnames(Goal3ObjectiveDBaseMonth6) = c("SAMonth6", "SABase")
summary(Goal3ObjectiveDBaseMonth6)
wilcox.test(Goal3ObjectiveDBaseMonth6$SAMonth6, Goal3ObjectiveDBaseMonth6$SABase, paired = TRUE, alternative = c("less"))
Goal3ObjectiveDBaseMonth6 = data.frame(t(colMeans(Goal3ObjectiveDBaseMonth6)))

Goal3ObjectiveDBaseMonth6$Difference = (Goal3ObjectiveDBaseMonth6$SAMonth6-Goal3ObjectiveDBaseMonth6$SABase)/ Goal3ObjectiveDBaseMonth6$SABase
Goal3ObjectiveDBaseMonth6 = round(Goal3ObjectiveDBaseMonth6, 2)
60/131 

#### Goal 3 Objective E ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### 
# Need to sum the following variables: TANF, IWAP, Medicaid, LegalAid, SSI, TTA, FoodBanks, HUD, Area10, RecoveryWorks, SNAP, WIC
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
Goal4ObjectiveC = data.frame(GPRAAll$EmployStatus.x, GPRAAll$EmployStatus.y)
write.csv(Goal4ObjectiveC, "Goal4ObjectiveC.csv", row.names = FALSE)
Goal4ObjectiveC = read.csv("Goal4ObjectiveC.csv", header = TRUE)
Goal4ObjectiveC = data.frame(na.omit(Goal4ObjectiveC))
dim(Goal4ObjectiveC)
Goal4ObjectiveC = data.frame(apply(Goal4ObjectiveC , 2, function(x){ifelse(x == 1, 1, ifelse(x == 2, 1, ifelse(x == 8, NA, ifelse(x == 9, NA, ifelse(x == 10, NA, 0)))))}))
Goal4ObjectiveC = data.frame(na.omit(Goal4ObjectiveC))
dim(Goal4ObjectiveC)
summary(Goal4ObjectiveC)
Goal4ObjectiveC = data.frame(t(apply(Goal4ObjectiveC , 2, sum)))
colnames(Goal4ObjectiveC)= c("Base", "Month6")
Goal4ObjectiveC$Difference = (Goal4ObjectiveC$Month6-Goal4ObjectiveC$Base)/Goal4ObjectiveC$Base
### Goal 4 Objective D ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
# Could create a composite score:  If you want to include OverallHealth it can be done, just is a little trickier
# So adding to binary variables FamilySupport and SelfHelp where two means yes to both 1 means yes to one and zero means no to both
# No variables like SatisfiedHealth, because there is not enough data
Goal4ObjectiveD =  data.frame(GPRAAll$AttendVoluntary.x, GPRAAll$AttendVoluntary.y, GPRAAll$InteractFamilyFriends.x, GPRAAll$InteractFamilyFriends.y)
Goal4ObjectiveD = data.frame(apply(Goal4ObjectiveD, 2, function(x){ifelse(x == 3, NA, ifelse(x == 4,NA, ifelse(x == 2, 0, ifelse(x == -99, NA, x))))}))
Goal4ObjectiveD = data.frame(na.omit(Goal4ObjectiveD))
dim(Goal4ObjectiveD)
count(Goal4ObjectiveD)
write.csv(Goal4ObjectiveD, "Goal4ObjectiveD.csv", row.names = FALSE)
Goal4ObjectiveD = read.csv("Goal4ObjectiveD.csv", header = TRUE)
count(Goal4ObjectiveD)

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
.86*131
install.packages("Rcmdr")

library(Rcmdr)

##### Goal 5 Objective B ###### ##########################################
### Just get demographcs from baseline: White, Black, Hispanic, Asian
Goal5ObjectiveBEth = data.frame(GPRAAll$RaceWhite.x, GPRAAll$HispanicLatino.x, GPRAAll$RaceBlack.x, GPRAAll$RaceAsian.x, GPRAAll$RaceAmericanIndian.x)
Goal5ObjectiveBEth = data.frame(apply(Goal5ObjectiveBEth, 2, function(x){ifelse(x == 2, 0, ifelse(x == -99, NA,ifelse(x == "<NA>", NA, x)))}))
colnames(Goal5ObjectiveBEth) = c("White", "Hispanic", "Black", "Asian", "AmericanIndian")
n = dim(Goal5ObjectiveBEth)
n = n[1]
library(psych)
white = count(Goal5ObjectiveBEth, "White")
white = white[1,2]/n; white

Black = count(Goal5ObjectiveBEth, "Black")
Black = Black[1,2]/n; Black

Asian = count(Goal5ObjectiveBEth, "Asian")
Asian = Asian[1,2]/n; Asian

AmericanIndian = count(Goal5ObjectiveBEth, "AmericanIndian")
AmericanIndian = AmericanIndian[1,2]/n; AmericanIndian

## Age ############
age = data.frame(GPRAAll$BirthYear.x)
write.csv(age, "age.csv", row.names = FALSE)
age = read.csv("age.csv", header = TRUE)
age = na.omit(age)
age
mean(age$GPRAAll.BirthYear.x)
age = 2018-(mean(age$GPRAAll.BirthYear.x)); age

## Now get military status #### ####################################################################
#ServedMilitary, ActiveDuty, Deployed, FamilyDuty
Goal5ObjectiveBMil = data.frame(GPRAAll$ServedMilitary.x, GPRAAll$ActiveDuty.x, GPRAAll$Deployed.x, GPRAAll$FamilyDuty.x)
library(plyr)
# Count 2's and 3's
Goal5ObjectiveBMil = count(GPRAAll$ServedMilitary.x)
Goal5ObjectiveBMil = subset(Goal5ObjectiveBMil, x ==2 | x ==3)
Goal5ObjectiveBMil = data.frame(apply(Goal5ObjectiveBMil, 2, sum))
colnames(Goal5ObjectiveBMil) = c("Total")
Goal5ObjectiveBMil = Goal5ObjectiveBMil[2,]
Goal5ObjectiveBMil 

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
edu = data.frame(GPRAAll$EducationYears.x)
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
Goal3ObjectiveDBaseMonth6 = data.frame(ParticipantID = GPRAAll$ClientID, GPRAAll$ERMental.x, GPRAAll$ERMental.y, GPRAAll$ERAlcoholSA.x, GPRAAll$ERAlcoholSA.y)
summary(Goal3ObjectiveDBaseMonth6)
Goal3ObjectiveDBaseMonth6 = na.omit(Goal3ObjectiveDBaseMonth6)
dim(Goal3ObjectiveDBaseMonth6)
Goal3ObjectiveDBaseMonth6Times = data.frame(ParticipantID = GPRAAll$ClientID, GPRAAll$ERMentalTimes.x, GPRAAll$ERMentalTimes.y, GPRAAll$ERAlcoholSATimes.x, GPRAAll$ERAlcoholSATimes.y)
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
Goal3ObjectiveDBase = data.frame(Goal3ObjectiveDBaseMonth6$GPRAAll.ERMentalTimes.x, Goal3ObjectiveDBaseMonth6$GPRAAll.ERAlcoholSATimes.x)
Goal3ObjectiveDBase = data.frame(apply(Goal3ObjectiveDBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveDBase) = c("SABase")

Goal3ObjectiveDMonth6= data.frame(Goal3ObjectiveDBaseMonth6$GPRAAll.ERMentalTimes.y, Goal3ObjectiveDBaseMonth6$GPRAAll.ERAlcoholSATimes.y)
write.csv(Goal3ObjectiveDMonth6, "Goal3ObjectiveDMonth6.csv", row.names = FALSE)
Goal3ObjectiveDMonth6 = read.csv("Goal3ObjectiveDMonth6.csv", header = TRUE)
Goal3ObjectiveDMonth6= data.frame(apply(Goal3ObjectiveDMonth6, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveDMonth6) = c("SAMonth6")

Goal3ObjectiveDBaseMonth6 = data.frame(Goal3ObjectiveDMonth6, Goal3ObjectiveDBase)
colnames(Goal3ObjectiveDBaseMonth6) = c("SAMonth6", "SABase")
summary(Goal3ObjectiveDBaseMonth6)
dim(Goal3ObjectiveDBaseMonth6)

Goal3ObjectiveDBaseMonth6 = data.frame(t(colMeans(Goal3ObjectiveDBaseMonth6)))

Goal3ObjectiveDBaseMonth6$Difference = (Goal3ObjectiveDBaseMonth6$SAMonth6-Goal3ObjectiveDBaseMonth6$SABase)/ Goal3ObjectiveDBaseMonth6$SABase
Goal3ObjectiveDBaseMonth6 = round(Goal3ObjectiveDBaseMonth6, 2); Goal3ObjectiveDBaseMonth6

###### Project Specific Outcomes ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
# Goal 4 Objective C Increase employment readiness
# Goal 4 Objective D Increase particpant independent living skills
# Goal 3 Objective E increase benefits enrollment
# Using housing rate data when you get it


