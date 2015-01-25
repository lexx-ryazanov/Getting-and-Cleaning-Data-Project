##Download a file
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "./data/dataset.zip"
if(!file.exists(fileName)){
  dir.create("data")
  download.file(fileurl, destfile = fileName)
  unzip(zipfile = fileName, exdir= "./data")
}

##Read data
x_train <- read.table("./data/UCI HAR Dataset/train/x_train.txt")
x_test <- read.table("./data/UCI HAR Dataset/test/x_test.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")

subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

features <- read.table("./data/UCI HAR Dataset/features.txt")
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

##Merges the training and the test sets to create one data set
all_train <- cbind(y_train,subject_train,x_train)
all_test <- cbind(y_test,subject_test,x_test)

all_combined <- rbind(all_train,all_test)

##Extracts only the measurements on the mean and standard deviation for each measurement
names(all_combined) <- c("V1","V2",as.character(features[,2]))
meanstd <- grep("V|mean()|std()",names(all_combined))

measurements <- all_combined[,c(meanstd)]

##Uses descriptive activity names to name the activities in the data set
colnames(measurements)[1:2] <- c("activity","subject")

##Appropriately labels the data set with descriptive variable names
class(activity_labels$V2)
activity_labels$V2 <- as.character(activity_labels$V2)

for (i in 1:length(measurements[,1])) {
    measurements[i,1] <- activity_labels[measurements[i,1],2]
}

##Creates a second, independent tidy data set with the average of each variable for each activity and each subject
colnames(measurements) [2] <- "subject"

attach(measurements)
tidy_means <- aggregate(measurements[,3] ~ subject + activity,data=measurements,FUN="mean")

for (i in 4:ncol(measurements)) {
    tidy_means[,i] <- aggregate(measurements[,i] ~ subject + activity,data=measurements,FUN="mean")[,3]
}

colnames(tidy_means) <- colnames(measurements)

write.table(tidy_means,file="./data/TidyData.txt", row.names=FALSE)
