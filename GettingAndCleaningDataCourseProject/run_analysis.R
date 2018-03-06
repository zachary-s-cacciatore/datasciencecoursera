# Load library for reading text files as dataframes
library(data.table)

############################
# Preprocessing of test data
############################

#1.0 Read in labels for adding description to variables

#1.1 Read in activity labels and rename activity labels column
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
names(activity_labels) <- c("ActivityID", "Activity")

#1.2 Read in features list
feature_list <- read.table("UCI HAR Dataset/features.txt")

##########################
#2.0 Read in the test data
##########################

#2.1 Read in subject test ids and rename column 
test_subjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
names(test_subjects) <- "SubjectID"

#2.2 Read in features for test data and label test_dataset with features_list names
test_dataset <- read.table("UCI HAR Dataset/test/X_test.txt")
names(test_dataset) <- feature_list$V2

#2.3 Read in activities for test data and rename column
test_activities <- read.table("UCI HAR Dataset/test/y_test.txt")
names(test_activities) <- "ActivityID"

#2.4 Combine test data for each observation
test_set <- cbind(test_subjects, test_dataset, test_activities)

#2.5 Subset test data to only include mean, std, SubjectID, ActivityID
sub_test_set <<- test_set[, grepl("SubjectID|ActivityID|mean\\(\\)|std\\(\\)", colnames(test_set))]

#2.6 Combined subset and activity labels in final test dataset
final_test_set <- merge(sub_test_set, activity_labels, all=TRUE)

#############################
#3.0 Preprocessing training data
#############################

#3.1 Read in file with IDs of test subjects and rename column
train_subjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
names(train_subjects) <- "SubjectID"

#3.2 Read in file with features for training data and rename with features_list names
train_dataset <- read.table("UCI HAR Dataset/train/X_train.txt")
names(train_dataset) <- feature_list$V2

#3.3 Read in activities for training data and rename column
train_activities <- read.table("UCI HAR Dataset/train/y_train.txt")
names(train_activities) <- "ActivityID"

#3.4 Combine training data for each observation
train_set <- cbind(train_subjects, train_dataset, train_activities)

#3.5 Subset test data to only include mean, std, SubjectID, ActivityID
sub_train_set <- train_set[, grepl("SubjectID|ActivityID|mean\\(\\)|std\\(\\)", colnames(train_set))]

#3.6 Combined subset and activity labels in final test dataset
final_train_set <- merge(sub_train_set, activity_labels, all=TRUE)

###################################################################################################################
#4.0 Merge test and train sets then create tidy dataset with average of each variable for each activity and subject
###################################################################################################################

data <- merge(final_test_set, final_train_set, all=TRUE)

#4.1 Load library for reshaping merged data
library(reshape2)

#4.2 Melt data on values SubjectID and ActivityID
avg_columns <- colnames(data[,3:68])
data_melt <- melt(data, id=c("SubjectID", "Activity"), measure.vars = avg_columns)

#4.3 Cast melted data to create tidy dataset
tidy_dataset <- dcast(data_melt, SubjectID + Activity ~ variable, mean)

#4.4 Export tidy dataset to txt file
write.table(tidy_dataset, file = "tidydataset.txt", row.names=FALSE)
