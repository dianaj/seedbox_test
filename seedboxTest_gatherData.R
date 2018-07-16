folder = "~/Desktop/JOB_SEARCH/datasciencetest-master"


#import data 
sTest <- read.csv(file = paste(folder,"/testSamples.csv", sep = ""), 
                    colClasses = c("numeric", "factor"))
levels(sTest$test_group)<- c("Ctrl", "Test")

dTest <- read.csv(file = paste(folder,"/transData.csv",sep = ""),
                 colClasses = c("numeric", "numeric", "factor", "numeric"))

levels(dTest$transaction_type)<- c("CBACK", "REBILL", "REFUND", "CANCEL")
summary(sTest)
summary(dTest)

#merge data and sample information

testt <- merge(sTest, dTest, by.x = "sample_id", all = TRUE)
testt$transaction_id[is.na(testt$transaction_type)] <- 0
testt$transaction_amount[is.na(testt$transaction_type)] <- 0
#testt$transaction_id[is.na(testt$transaction_type)] <- 0
testt$transaction_type[is.na(testt$transaction_type)] <- as.factor("CANCEL")


#quick look at data
summary(testt)
par(mfrow=c(1,1))
plot(testt$test_group, testt$transaction_type)

#we need to define a transaction "time" variable out of the transaction_id in order
#to be able to have repeated measures of our test subjects. 

# sort by sample_id and transaction_id
test <- testt[order(testt$sample_id, testt$transaction_id),]

# find a way to make consecutive months out of that id. 
for (i in 1:length(test$sample_id)){
test$t_id1[i] <- test$transaction_id[test$sample_id == test$sample_id[i]][1]
}
test$time[test$transaction_type != "CANCEL"] <- test$transaction_id[test$transaction_type != "CANCEL"] %% 
                                                test$t_id1[test$transaction_type != "CANCEL"] +1 #(i don't like things starting at 0)
test$time[test$transaction_type == "CANCEL"] <- 1
test$t_id1 <- NULL
plot(test$test_group, test$transaction_type)

round(prop.table(table(test$time[test$test_group == "Ctrl"], test$transaction_type[test$test_group == "Ctrl"])),3)
round(prop.table(table(test$time[test$test_group == "Test"], test$transaction_type[test$test_group == "Test"])),3)
#seems to have done the job.

#it's lucky the transaction id's are in ordered by sample-id and increase by 1 for each customer...
#otherwise would have had to look up some other way to make my time variable
#---------------------------------------------

#repeated measures representation of data 
#imputing CANCEL transactions
test_wide <- reshape(test[,c(1,2,4,5,6)], idvar = c("sample_id", "test_group"), timevar = "time",direction = "wide")
for(i in (2:22)){
#i<-2
test_wide[,paste("transaction_type.",i, sep = "")][(test_wide[,paste("transaction_type.",i-1, sep = "") ]!= "CANCEL" &
                                                   is.na(test_wide[,paste("transaction_type.",i, sep = "")]))] <- "CANCEL"
test_wide[,paste("transaction_amount.",i, sep = "")][(test_wide[,paste("transaction_amount.",i-1, sep = "") ]!= 0 &
                                                     is.na(test_wide[,paste("transaction_amount.",i, sep = "")]))] <- 0
}
summary(test_wide)

#need a slightly different shape for survival analysis
test_long <- reshape(test_wide, idvar = c("sample_id", "test_group"), timevar = "time",direction = "long")
summary(test_long)


#need to impute some 0 values to calculate average revenue genearted by customer
test_wide0 <- test_wide
for (i in 1:22){
test_wide0[is.na(test_wide[, paste("transaction_type.",c(i),sep = "")]),
           paste("transaction_type.",c(i),sep = "")] <- "CANCEL"
test_wide0[is.na(test_wide[, paste("transaction_amount.",c(i),sep = "")]),
           paste("transaction_amount.",c(i),sep = "")] <- 0
}
summary(test_wide0)


# need to keep last recorded sample_ID for survival test

surv_test <- test_long[!is.na(test_long$transaction_type.1),]
surv_test <- surv_test[order(surv_test$time, decreasing=TRUE),]
surv_test <- surv_test[!duplicated(surv_test$sample_id),]
surv_test$death <- surv_test$transaction_type.1 != "REBILL"
summary(surv_test)



#saving files to use later
save(sTest, file = paste(folder,"/sTest.Rdata", sep = ""))
save(dTest, file = paste(folder,"/dTest.Rdata", sep = ""))
save(test, file = paste(folder,"/test.Rdata", sep = ""))
save(test_wide, file = paste(folder,"/test_wide.Rdata", sep = ""))
save(test_long, file = paste(folder,"/test_long.Rdata", sep = ""))
save(test_wide0, file = paste(folder,"/test_wide0.Rdata", sep = ""))
save(surv_test, file = paste(folder,"/surv_test.Rdata", sep = ""))

