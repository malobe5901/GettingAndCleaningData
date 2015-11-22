##########################################################################################
##
## Coursera Getting and Cleaning Data - "Course Project"
## Marco P. Lopez
##
## Here are the data for the project:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
##
## You should create one R script called "run_analysis.R" that does the following:
##
## 1. Merge the training and the test sets to create one data set.
## 2. Extract only the measurements on the mean and standard deviation for each 
##    measurement. 
## 3. Use descriptive activity names to name the activities in the data set
## 4. Appropriately label the data set with descriptive activity names. 
## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.
##
##########################################################################################

## Cleans Up WorkSpace
rm(list=ls())

## Check for Needed Library and install package if needed
if (!require("reshape2")) install.packages("reshape2")
library(reshape2)

## Sets up working directory 
setwd("C:\\Data\\Coursera\\GettingCleaningData\\WorkingDirectory\\Week3\\Data")

## Assign information to variables needed to get data 
FileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
FileName <- "Q3_DataSet.zip"

## Download and unzip project's data:
if (!file.exists(FileName)){
  download.file(FileURL, FileName, mode="wb", method="wininet", cacheOK=FALSE)
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(FileName) 
}

##
## 1. Merge the training and the test sets to create one data set.
## 

## Reads data under "UCI HAR Dataset" Folder
Activity_Labels <- read.table('.\\UCI HAR Dataset\\activity_labels.txt',header=FALSE); 
Activity_Labels[,2] <- as.character(Activity_Labels[,2])
Features <- read.table('.\\UCI HAR Dataset\\features.txt',header=FALSE); 
Features[,2] <- as.character(Features[,2])

##
## 2.Extracts only Mean and Std from Features to filer only needed data when reading files
##

FeaturesWanted <- grep(".*mean.*|.*std.*", Features[,2])
FeaturesWanted.Names <- Features[FeaturesWanted,2]

## Reads data under "UCI HAR Dataset\train" Folder
Subject_Train <- read.table('.\\UCI HAR Dataset\\train\\subject_train.txt',header=FALSE); 
X_Train <- read.table('.\\UCI HAR Dataset\\train\\x_train.txt',header=FALSE)[FeaturesWanted]
Y_Train <- read.table('.\\UCI HAR Dataset\\train\\y_train.txt',header=FALSE); 

## Reads data under "UCI HAR Dataset\test" Folder
Subject_Test <- read.table('.\\UCI HAR Dataset\\test\\subject_test.txt',header=FALSE); 
X_Test <- read.table('.\\UCI HAR Dataset\\test\\x_test.txt',header=FALSE)[FeaturesWanted]; 
Y_Test <- read.table('.\\UCI HAR Dataset\\test\\y_test.txt',header=FALSE);

## Retrieves dimension of Data. 
## Make sure X_Train and X_Test has only 79 Columns out of 561 
#dim(Activity_Labels)	
#dim(Features)
#dim(Subject_Train) 
#dim(X_Train)  	
#dim(Y_Train) 	
#dim(Subject_Test)
#dim(X_Test) 	
#dim(Y_Test) 
#length(FeaturesWanted)
#length(FeaturesWanted.Names)

## Labels Column Names   
colnames(Activity_Labels) <- c('Activity_ID','Activity_Label');
colnames(Subject_Train) <- "Subject_ID";
colnames(X_Train) <- FeaturesWanted.Names; 
colnames(Y_Train) <- "Activity_ID";
colnames(Subject_Test) <- "Subject_ID";
colnames(X_Test) <-  FeaturesWanted.Names; 
colnames(Y_Test) <- "Activity_ID";

## Merges Y_Train, Subject_Train and X_Train files. Creates the final training dataset. 
Training_Data <- cbind(Y_Train,Subject_Train,X_Train);

## Merges X_Test, Y_Test and Subject_Test files. Creates the final test dataset.  
Test_Data = cbind(Y_Test,Subject_Test,X_Test);

## Combines final training and final test datasets. Creates the Final Dataset.
Final_Data = rbind(Training_Data,Test_Data);
dim(Final_Data)
colnames(Final_Data)

##
## 2. Continuation.. cleans up variable names.
##
FeaturesWanted.Names <- colnames(Final_Data)
FeaturesWanted.Names = gsub('-mean', 'Mean', FeaturesWanted.Names)
FeaturesWanted.Names = gsub('-std', 'Std', FeaturesWanted.Names)
FeaturesWanted.Names <- gsub('[-()]', '', FeaturesWanted.Names)
FeaturesWanted.Names <- gsub("^(t)","Time",FeaturesWanted.Names)
FeaturesWanted.Names <- gsub("^(f)","Freq",FeaturesWanted.Names)

## Reassing the new labels to the Final_Data set 
colnames(Final_Data) <- (FeaturesWanted.Names)

## Merges the Final Data set with Activity_labels
Final_Data <- merge(Final_Data,Activity_Labels,by='Activity_ID',all.x=TRUE);

# Create a new table, finalDataNoActivity_Labels without the Activity_Labels column
Final_Data.No_Activity_Labels  = Final_Data[,names(Final_Data) != 'Activity_Labels'];

# Gets the mean of each variable for each activity and each subject
TidyDataSet = aggregate(Final_Data.No_Activity_Labels[,names(Final_Data.No_Activity_Labels) != c('Activity_ID','Subject_ID')],by=list
                        (Activity_ID=Final_Data.No_Activity_Labels$Activity_ID,Subject_ID = Final_Data.No_Activity_Labels$Subject_ID),mean);

# Merging the TidyDataSet with Activity_Labels to include descriptive acitvity names
TidyDataSet= merge(TidyDataSet,Activity_Labels,by='Activity_ID',all.x=TRUE);

# Export the TidyDataSet set 
write.table(TidyDataSet, './TidyDataSet.txt',row.names=TRUE,sep='\t');