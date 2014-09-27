## Coursera - Data Science: Getting and Cleaning Data - Course Project

# Part 1 and Part 2
# (1) Merges the training and the test sets to create one data set.
# (2) Extracts only the measurements on the mean and standard deviation for each measurement.
# Function for reading subject_train, X_train, y_train from folder 'train'
# ;and for labelling and extracting data for mean and standard deviation 

r_Data = function (fname, folder) {
  path = file.path(folder, paste0("y_", fname, ".txt"))
  y_data = read.table(path, header=FALSE, col.names = c("ActiviytID"))
  
  path = file.path(folder, paste0("subject_", fname, ".txt"))
  subject_data = read.table(path, header=FALSE, col.names=c("SubjectID"))
  
  data_columns = read.table("features.txt", header=FALSE, as.is=TRUE, col.names=c("MeasureID", "MeasureName"))
  
  path = file.path(folder, paste0("X_", fname, ".txt"))
  dataset = read.table(path, header=FALSE, col.names=data_columns$MeasureName)
  
  subset_data_columns = grep(".*mean\\(\\)|.*std\\(\\)", data_columns$MeasureName)
  
  dataset = dataset[, subset_data_columns]
  
  dataset$ActivityID = y_data$ActivityID
  dataset$SubjectID = subject_data$SubjectID
  
  dataset
}

# reading test dataset
read_test_data = function() {
  r_Data("test", "test")
}

# reading train dataset
read_train_data = function () {
  r_Data("train", "train")
}

# merging 2 datasets and giving column names
mergeDataset = function () {
  dataset = rbind(read_test_data(), read_train_data())
  cnames = colnames(dataset)
  cnames = gsub("\\.+mean\\.+", cnames, replacement = "Mean")
  cnames = gsub("\\.+std\\.+", cnames, replacement = "Std")
  colnames(dataset) = cnames
  dataset
}

# Part 3 and Part 4
# (3) Uses descriptive activity names to name the activities in the data set
# (4) Appropriately labels the data set with descriptive variable names 
# Reading activity labels and creating column labels 
activityLabels = function (dataset) {
  activity_labels = read.table("activity_labels.txt", header = FALSE, as.is=TRUE, col.names = c("ActivityID", "ActivityName"))
  activity_labels$ActivityName = as.factor(activity_labels$ActivityName)
  data_labels = merge(dataset, activity_labels)
  data_labels
}

# merging activity labels to merged dataset
merge_label_data = function () {
  activityLabels(mergeDataset())
}

# Part 5
# (5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
# Creating 2nd independent tidy data set with average of variables for activities and subjects 
tidyData = function(merge_label_data) {
  library(reshape2)
  
  vars = c("ActivityID", "ActivityName", "SubjectID")
  measure_vars = setdiff(colnames(merge_label_data), vars)
  melted_data <- melt(merge_label_data, id=vars, measure.vars=measure_vars)
  
  # recast 
  dcast(melted_data, ActivityName + SubjectID ~ variable, mean)
}

#Getting clean tidy dataset
tidy_datafile =function(fname){
  tidy_data = tidyData(merge_label_data())
  write.table(tidy_data, fname)
}

tidy_datafile("tidy.txt")

 
