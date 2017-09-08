## M. S. Khakwani
## Assignment for cleaning data

# Download dataset from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


#set working directory to the location where the UCI HAR Dataset was unzipped
setwd('/Users/Khakwani/Desktop/coursera/UCI HAR Dataset/');

# Read the data from files into data tables

# read in the training data
dtFeatures     = read.table('./features.txt',header=FALSE); 
dtActivity     = read.table('./activity_labels.txt',header=FALSE); 
dtSubjectTr      = read.table('./train/subject_train.txt',header=FALSE);
dtXTrain       = read.table('./train/X_train.txt',header=FALSE); 
dtYTrain       = read.table('./train/y_train.txt',header=FALSE);

# set column names
colnames(dtActivity)    = c('activityId','dtActivity');
colnames(dtXTrain)        = features[,2]; 
colnames(dtYTrain)        = "activityId";
colnames(dtSubjectTr)     = "subjectId";

# 1. Merge training sets
dtTrain = cbind(dtYTrain,dtSubjectTr,dtXTrain);

# Read test data into data tables (dt)
dtSubjectTest = read.table('./test/subject_test.txt',header=FALSE);
dtXTest       = read.table('./test/X_test.txt',header=FALSE); 
dtYTest       = read.table('./test/y_test.txt',header=FALSE); 

# rename columns
colnames(dtSubjectTest) = "subjectId";
colnames(dtXTest)       = features[,2]; 
colnames(dtYTest)       = "activityId";


# Create the final test set by merging the xTest, yTest and subjectTest data
dtTest = cbind(dtYTest,dtSubjectTest,dtXTest);


# test and training data together
dtTotalData = rbind(dtTrain,dtTest);

# store column names
dtColNames  = colnames(dtTotalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# set to TRUE values for columns
extrcols = (grepl("activity..",dtColNames) | grepl("subject..",dtColNames) | grepl("-mean..",dtColNames) & !grepl("-meanFreq..",dtColNames) & !grepl("mean..-",dtColNames) | grepl("-std..",dtColNames) & !grepl("-std()..-",dtColNames));

# Subset dtTotalData table based on the logicalVector to keep only desired columns
dtDataExtract = dtTotalData[extrcols==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the dtTotalData set with the acitivityType table to include descriptive activity names
dtTotalData = merge(dtTotalData,dtActivity,by='activityId',all.x=TRUE);

# Updating the dtColNames vector to include the new column names after merge
dtColNames  = colnames(dtTotalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(dtColNames)) 
{
  dtColNames[i] = gsub("-std$","StdDev",dtColNames[i])
  dtColNames[i] = gsub("-mean","Mean",dtColNames[i])
  dtColNames[i] = gsub("^(t)","time",dtColNames[i])
  dtColNames[i] = gsub("^(f)","freq",dtColNames[i])
  dtColNames[i] = gsub("[Gg]yro","Gyro",dtColNames[i])
  dtColNames[i] = gsub("Acc","Accelerator",dtColNames[i])
};

# Reassigning the new descriptive column names to the dtTotalData set
colnames(dtTotalData) = dtColNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, dtFinData without the dtActivity column
dtFinData  = dtTotalData[,names(dtTotalData) != 'dtActivity'];

# Summarizing the dtFinData table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(dtFinData[,names(dtFinData) != c('activityId','subjectId')],by=list(activityId=dtFinData$activityId,subjectId = dtFinData$subjectId),mean);

# Merging the tidyData with dtActivity to include descriptive acitvity names
tidyData    = merge(tidyData,dtActivity,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');

