# 1 Create the data set
Question 1: Merges the training and the test sets to create one data set.

Download data to work directory/UCI HAR Dataset

Read the README.txt file. 

```{r}
readLines("UCI HAR Dataset/README.txt")
```

Then we got some useful information about the files:
'features.txt': List of all features
'activity_labels.txt': Links the class labels with their activity name
'train/X_train.txt': Training set
'train/y_train.txt': Training labels
'test/X_test.txt': Test set
'test/y_test.txt': Test labels
...


Read the data we need and bind them: 

```{r}
subject <- rbind(read.table("UCI HAR Dataset/train/subject_train.txt"),
                 read.table("UCI HAR Dataset/test/subject_test.txt"))
feature <- read.table("UCI HAR Dataset/features.txt")
activity <- rbind(read.table("UCI HAR Dataset/train/y_train.txt"),
                  read.table("UCI HAR Dataset/test/y_test.txt"))
data <- rbind(read.table("UCI HAR Dataset/train/X_train.txt"),
              read.table("UCI HAR Dataset/test/X_test.txt"))
colnames(data) <- feature$V2
mydata_1 <- data.frame(subject = subject$V1, activity = activity$V1)
mydata_1 <- cbind(mydata_1, data)
```

Here the dataframe "mydata" is the required data set. 

# 2 Extract the columns on mean and std
Question 2: Extracts only the measurements on the mean and standard deviation for each measurement.

```{r}
tar_column <- unique(grep("mean", names(mydata_1), value = TRUE),
                     grep("std", names(mydata_1), value = TRUE))
# keep the subject, activity and target columns
mydata_2 <- subset(mydata_1, select = c("subject", "activity", tar_column))
```

# 3 Rename activity names ----
Question 3: Uses descriptive activity names to name the activities in the data set.
Read activity label code book: 

```{r}
activity_label <- read.table("UCI HAR Dataset/activity_labels.txt")
names(activity_label) <- c("activity", "descriptive_activity_name")
activity_label
mydata_3 <- merge(mydata_2, activity_label, by = "activity")
# delete the original acitivity column
mydata_3 <-
  subset(mydata_3,
         select = names(mydata_3)[!names(mydata_3) %in% c("activity")])
```

Here "descriptive_activity_name" is the required “descriptive activity names”. 

# 4 Rename the varibles with descriptive names
Question 4: Appropriately labels the data set with descriptive variable names.
Check "features_info.txt" for details of the variable names, then replace the variable names to descriptive names: 

```{r}
mydata_4 <- mydata_3
names(mydata_4) <- gsub("^t", "time", names(mydata_4))
names(mydata_4) <- gsub("^f", "frequency", names(mydata_4))
names(mydata_4) <- gsub("Acc", "Accelerometer", names(mydata_4))
names(mydata_4) <- gsub("Mag", "Magnitude", names(mydata_4))
names(mydata_4) <- gsub("Gyro", "Gyroscope", names(mydata_4))
names(mydata_4)
```

# 5 Create a new data set with average values of each activity and each subject
Question 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

```{r}
mydata_5 <-
  aggregate(.~ subject + descriptive_activity_name,
            data = mydata_4, FUN = "mean")
# export the data set as *.txt file
write.table(mydata_5, "tidy_data.txt", row.names = FALSE)
```
