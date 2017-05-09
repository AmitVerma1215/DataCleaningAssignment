#######################ASSIGNMENT#####################

getwd()

setwd("E://Old_Doc//sas doc//Stat//JHU//Data Cleaning")

##############Downloading the Dataset#####################

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl, destfile = "Dataset.zip")

######################Unzipping the Dataset################

unzip(zipfile = "Dataset.zip")

path <- file.path("E://Old_Doc//sas doc//Stat//JHU//Data Cleaning" , "UCI HAR Dataset")

files<-list.files(path, recursive=TRUE)
files

##############reading train and test Datasets##############

######train dataset########

x_train <- read.table("UCI HAR Dataset/train/X_train.txt")

dim(x_train)
str(x_train)

y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

dim(y_train)
str(y_train)

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

dim(subject_train)
str(subject_train)

############test dataset##################

x_test <- read.table("UCI HAR Dataset/test/X_test.txt")

dim(x_test)
str(x_test)

y_test <- read.table("UCI HAR Dataset/test/y_test.txt")

dim(y_test)
str(y_test)

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

dim(subject_test)
str(subject_test)

##############reading features dataset############

features <- read.table("UCI HAR Dataset/features.txt")

dim(features)
str(features)

write.table(features, "features.csv", row.names = TRUE)

#####################reading activity dataset###############

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

dim(activity_labels)
str(activity_labels)

#####################Assigning Column Names########################
#########train dataset#################

colnames(x_train) <- features[, 2]

str(x_train)

colnames(y_train) <- "activityid"

str(y_train)

colnames(subject_train) <- "subjectid"

str(subject_train)

############test dataset###################

colnames(x_test) <- features[,2]
str(x_test)

colnames(y_test) <- "activityid"
str(y_test)

colnames(subject_test) <- "subjectid"
str(subject_test)

colnames(activity_labels) <- c("activityid", "activitytype")
str(activity_labels)

####################Merging Data sets#####################
########train dataset with same row count#################

mrg_train <- cbind(y_train, subject_train, x_train)

dim(mrg_train)
str(mrg_train)

########test dataset with same row count#################

mrg_test <- cbind(y_test, subject_test, x_test)

dim(mrg_test)
str(mrg_test)

##############append train and test dataset with same number of columns ##################

merge_All <- rbind(mrg_train, mrg_test)

dim(merge_All)
str(merge_All)

################Columns Names##############################

col_names <- colnames(merge_All)
col_names

#########################vector for Mean, Std and ids vars#################
##########defining vector by using GREPL function for those columns where we have mean or std######

mean_std <- (grepl("activityid", col_names) | grepl("subjectid", col_names) | 
                     grepl("mean..", col_names) | grepl("std..", col_names)
)

#################### Subset data from merge_All##################

setMeanAndStd <- merge_All[, mean_std == TRUE]

dim(setMeanAndStd)
str(setMeanAndStd)
names(setMeanAndStd)

##########################Descriptive activity names to names the activities##################

setWithActivityNames <- merge(setMeanAndStd, activity_labels, by = 'activityid', all.x = TRUE)

str(setWithActivityNames)
dim(setWithActivityNames)

######Creating a second, independent tidy data set with the average of each variables for each activity and subject####

secTidySet <- aggregate(. ~subjectid + activityid, setWithActivityNames, mean)

str(secTidySet)
dim(secTidySet)

secTidySet <- secTidySet[order(secTidySet$subjectid, secTidySet$activityid),]

dim(secTidySet)
str(secTidySet)

##############writing second dataset #######################

write.table(secTidySet, "secTidySet.txt", row.name = FALSE)