#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr)
library(data.table)


# Read data
  # feature and activity
features <- read.table("UCI HAR Dataset/features.txt", colClasses = c("character"))
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("ActivityId", "Activity"))
  #training
x_train <- read.table("UCI HAR Dataset/train/x_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  #test
x_test <- read.table("UCI HAR Dataset/test/x_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")


# Binding sensor data
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)

# Label columns
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

sensor_data_mean_std <- sensor_data[,grep(".*Mean.*|.*Std.*|subject|activityId", names(sensor_data), ignore.case=TRUE)]


# 3. Uses descriptive activity names to name the activities in the data set

sensor_data_mean_std <- join(sensor_data_mean_std, activity_labels, by = "ActivityId", match = "first")
sensor_data_mean_std <- sensor_data_mean_std[,-1]


# 4. Appropriately labels the data set with descriptive names.

names(sensor_data_mean_std)<-gsub("Acc", "Accelerometer", names(sensor_data_mean_std))
names(sensor_data_mean_std)<-gsub("Gyro", "Gyroscope", names(sensor_data_mean_std))
names(sensor_data_mean_std)<-gsub("BodyBody", "Body", names(sensor_data_mean_std))
names(sensor_data_mean_std)<-gsub("Mag", "Magnitude", names(sensor_data_mean_std))
names(sensor_data_mean_std)<-gsub("^t", "Time", names(sensor_data_mean_std))
names(sensor_data_mean_std)<-gsub("^f", "Frequency", names(sensor_data_mean_std))
names(sensor_data_mean_std)<-gsub("tBody", "TimeBody", names(sensor_data_mean_std))
names(sensor_data_mean_std)<-gsub("-mean()", "Mean", names(sensor_data_mean_std), ignore.case = TRUE)
names(sensor_data_mean_std)<-gsub("-std()", "STD", names(sensor_data_mean_std), ignore.case = TRUE)
names(sensor_data_mean_std)<-gsub("-freq()", "Frequency", names(sensor_data_mean_std), ignore.case = TRUE)
names(sensor_data_mean_std)<-gsub("angle", "Angle", names(sensor_data_mean_std))
names(sensor_data_mean_std)<-gsub("gravity", "Gravity", names(sensor_data_mean_std))


# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

sensor_data_mean_std$Subject <- as.factor(sensor_data_mean_std$Subject)
sensor_data_mean_std <- data.table(sensor_data_mean_std)
tidyData <- aggregate(. ~Subject + Activity, sensor_data_mean_std, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy_Data.txt", row.names = FALSE)

