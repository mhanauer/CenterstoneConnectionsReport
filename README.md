##### Goal 1 Objective C ###### ################################################
# Need the total number of people in the program.  So should be from the GPRA
head(GPRAConAll)
Goal1ObjectiveCConn = data.frame(GPRAConAll$ParticipantID)
Goal1ObjectiveCConn = dim(Goal1ObjectiveCConn)
Goal1ObjectiveCConn = data.frame(Goal1ObjectiveCConn[1])
colnames(Goal1ObjectiveCConn) = c("Total Participants")
write.csv(Goal1ObjectiveCConn, "Goal1ObjectiveCConn.csv", row.names = FALSE)

