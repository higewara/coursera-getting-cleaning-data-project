## 1. Merges the training and the test sets to create one data set. 
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set.
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# load library
library(dplyr)
library(data.table)

# download data

filename <- "Week4.zip"
if(!file.exists(filename)) {
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl, filename, method = "curl")
}

# upzip data

if(!file.exists("UCI HAR Dataset")) {
  unzip(filename)
}

# read txt data

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

# setting data frame

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/x_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/x_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

## 1. Merges the training and the test sets to create one data set. 

X_combined <- rbind(x_test, x_train)
Y_combined <- rbind(y_test, y_train)
subject <- rbind(subject_test, subject_train)
All_combined <- cbind(subject, X_combined, Y_combined)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

Tidy_data <- All_combined %>% select(subject, code, contains("mean"), contains("std"))

## 3. Uses descriptive activity names to name the activities in the data set.

Tidy_data$code <- activities[Tidy_data$code, 2]

## 4. Appropriately labels the data set with descriptive variable names. 

names(Tidy_data)[2] = "activity"
names(Tidy_data) <- gsub("Acc", "Accelerometer", names(Tidy_data))
names(Tidy_data) <- gsub("Gyro", "Gyroscope", names(Tidy_data))
names(Tidy_data) <- gsub("BodyBody", "Body", names(Tidy_data))
names(Tidy_data) <- gsub("Mag", "Magnitude", names(Tidy_data))
names(Tidy_data) <- gsub("^t", "Time", names(Tidy_data))
names(Tidy_data) <- gsub("^f", "Frequency", names(Tidy_data))
names(Tidy_data) <- gsub("tBody", "TimeBody", names(Tidy_data))
names(Tidy_data) <- gsub("-mean()", "Mean", names(Tidy_data), ignore.case = TRUE)
names(Tidy_data) <- gsub("-std()", "STD", names(Tidy_data), ignore.case = TRUE)
names(Tidy_data) <- gsub("-freq()", "Frequency", names(Tidy_data), ignore.case = TRUE)
names(Tidy_data) <- gsub("angle", "Angle", names(Tidy_data))
names(Tidy_data) <- gsub("gravity", "Gravity", names(Tidy_data))

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Data_all <-aggregate(. ~subject + activity, Tidy_data, mean)
Data_all <- Data_all[order(Data_all$subject, Data_all$activity),]
write.table(x = Data_all, file = "data_tidy.txt", row.names = FALSE)

#checking data

str(Data_all)

#show the final data

Data_all
