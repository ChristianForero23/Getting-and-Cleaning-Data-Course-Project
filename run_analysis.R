library(plyr)
# 1. Merge the training and the test sets to create one data set.

# 1.1 Reading files
xtrain<- read.table("./train/X_train.txt")
ytrain<<-read.table("./train/y_train.txt")
subject_train<- read.table("./train/subject_train.txt")
xtest<- read.table("./test/X_test.txt")
ytest<-read.table("./test/y_test.txt")
subject_test<- read.table("./test/subject_test.txt")
activity_labels =read.table("./activity_labels.txt")
features<- read.table("./features.txt")

# 4. Assigning variable names
colnames(subject_test)<-"subject_ID"
colnames(subject_train)<-"subject_ID"
colnames(ytest)<-"activity_ID"
colnames(ytrain)<- "activity_ID"
colnames(xtest)<- features[,2]
colnames(xtrain)<-features[,2]
colnames(activity_labels)<- c("activity_ID", "activityType") 

# 1.3 Merging all datasets into one set
frist<- cbind(ytrain,subject_train,xtrain)
second<-cbind(ytest,subject_test,xtest)
total<-rbind(frist,second)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
colNames <- colnames(total)
mean_and_std <- (grepl("activity_ID", colNames) |
                   grepl("subject_ID", colNames) |
                   grepl("mean..", colNames) |
                   grepl("std...", colNames)
)
set_mean_sd<- total[,mean_and_std==TRUE]

#3. Uses descriptive activity names to name the activities in the data set
setWithActivityNames <- merge(set_mean_sd, activity_labels,
                              by = "activity_ID",
                              all.x = TRUE)
setWithActivityNames <- setWithActivityNames[,c(1,2,73,3:72)]


#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidySet <- aggregate(. ~subject_ID + activity_ID, set_mean_sd, mean)
Newtidyset <- merge(tidySet, activity_labels,
                              by = "activity_ID",
                              all.x = TRUE)
Newtidyset <- Newtidyset[order(Newtidyset$subject_ID,Newtidyset$activity_ID), ]
Newtidyset<- Newtidyset[,c(2,1,3:72)]
write.table(tidySet, "tidySet.txt", row.names = FALSE)
