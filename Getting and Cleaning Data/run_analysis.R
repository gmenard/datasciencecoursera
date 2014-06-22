# The following script does the following:
# 1 - Merges the training and the test sets to create one data set.
# 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3 - Uses descriptive activity names to name the activities in the data set.
# 4 - Appropriately labels the data set with descriptive activity names. 
# 5 - Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

require("reshape2")
require("plyr")

datasetDir  <- "C:/Users/Guillaume/Documents/GitHub/datasciencecoursera/Getting and Cleaning Data/UCI_HAR_Dataset"
trainDir <- paste(datasetDir, "train", sep="/")
testDir  <- paste(datasetDir, "test", sep="/")

# 1 - Merges the training and the test sets to create one data set.

test_y  <- read.table(paste(testDir, "y_test.txt", sep="/"), sep="\n", strip.white=T)
train_y <- read.table(paste(trainDir, "y_train.txt", sep="/"), sep="\n", strip.white=T)

train_x <- read.table(paste(trainDir, "X_train.txt", sep="/"), sep="\n", strip.white=T)
train_x <- ldply(strsplit(gsub(" {2,}", " ", train_x$V1), " "))
test_x  <- read.table(paste(testDir, "X_test.txt", sep="/"), sep="\n", strip.white=T)
test_x  <- ldply(strsplit(gsub(" {2,}", " ", test_x$V1), " "))

subject_train <- read.table(paste(trainDir, "subject_train.txt", sep="/"), sep="\n", strip.white=T)
subject_test  <- read.table(paste(testDir, "subject_test.txt", sep="/"), sep="\n", strip.white=T)

training_dataset <- cbind(train_y, subject_train, train_x)
test_dataset  <- cbind(test_y, subject_test, test_x)

merged_dataset <- rbind(training_dataset, test_dataset)


# 2 - Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table(paste(datasetDir, "features.txt", sep="/"), sep="\n", strip.white=T)
features <- gsub("^[0-9]+ ", "", features$V1)
features_mean_std <- grepl("mean|std", features)

merged_dataset <- merged_dataset[,c(TRUE, TRUE, features_mean_std)]


# 3 - Uses descriptive activity names to name the activities in the data set.

col_headers <- c("activity", "subject", features[features_mean_std])


# 4 - Appropriately labels the data set with descriptive activity names.

colnames(merged_dataset) <- col_headers

write.table(merged_dataset, file="tidy_dataset.txt")


# 5 - Creates a second, independent tidy data set with the average of each variable for each activity and subject.

average <- aggregate(merged_dataset[,3] ~ merged_dataset$subject + merged_dataset$activity, data = merged_dataset, FUN = mean)

for (i in 4:ncol(merged_dataset)){
  average[,i] <- aggregate(merged_dataset[,i] ~ merged_dataset$subject + merged_dataset$activity, data = merged_dataset, FUN = mean )[,3]
}

colnames(average) <- col_headers

write.table(average, file="tidy_dataset_average.txt")