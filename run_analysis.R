##1.Merges the training and the test sets to create one data set.
##1.1 Download and unzip the data
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName<-"projectData.zip"
if (!file.exists(fileName)){
  download.file(fileUrl, fileName)
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(fileName) 
}
##1.2 Load data
trainX <- read.table("UCI HAR Dataset/train/X_train.txt")
trainY <- read.table("UCI HAR Dataset/train/y_train.txt")
trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
testX <- read.table("UCI HAR Dataset/test/X_test.txt")
testY <- read.table("UCI HAR Dataset/test/y_test.txt")
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
##1.3 Merge the training and test data sets
trainData <- cbind(trainSubject, trainY, trainX)
testData <- cbind(testSubject, testY, testX)
allData <- rbind(trainData, testData)

##2.Extracts only the measurements on the mean and standard deviation for each measurement.
##2.1Load features and add column names
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)[,2]
colnames(allData) <- c("subject", "activity", features)
##2.2Extract target columns with the words "mean" or "std"
targetFeatures <- grep("subject|activity|.*mean.*|.*std.*", colnames(allData))
allData<-allData[,targetFeatures]

##3.Uses descriptive activity names to name the activities in the data set
##3.1 Load activity
activity<- read.table("UCI HAR Dataset/activity_labels.txt")
##3.2 Replace activities with activity names
allData$activity <- factor(allData$activity, levels = activity[,1], labels = activity[,2])

##4.Appropriately labels the data set with descriptive variable names.
colNames<-names(allData)
colNames<-gsub("[\\(\\)-]", "", colNames)
colNames<-gsub("^f", "frequency", colNames)
colNames<-gsub("^t", "time", colNames)
colNames<-gsub("mean","Mean",colNames)
colNames<-gsub("std","Std", colNames)
names(allData)<-colNames

##5. From the data set in step 4, creates a second, independent tidy data set 
##with the average of each variable for each activity and each subject.
library(dplyr)
tidyData <- allData %>%
  group_by(subject, activity) %>%
  summarise_each(funs(mean))
write.table(tidyData, "tidyData.txt", row.names = FALSE, quote = FALSE)


