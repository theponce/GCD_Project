GCD_Project
===========

--------------------
        #Project
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
--------------------

# setwd("~/Documents/Coursera/Getting_and_Cleaning_Data/Project")
# files used -  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# function to rename columns of data sets to remain consistent through table binding
        RNS <- function(name,dir){
                x <- read.table(dir,sep="",strip.white=TRUE)            #build table from file
                names <- sprintf(paste(name,"%02d"), 1:ncol(x))        #using specified name, create list of column names incremented from 1 to end of table
                colnames(x) <- names
                data.frame(x)
        }

# Supporting Information Directory 
# pull in features text file and create data.frame with is to use the second column to rename the test and training data
        features_dir <- "./UCI HAR Dataset/features.txt" # Training set.
        activities_dir <- "./UCI HAR Dataset/activity_labels.txt" # Training set.

#load supporting tables
        features <- RNS("features",features_dir)
        act_lbls <- RNS("actvs",activities_dir)


# Test File  - directories
        tst_x_dir <- "./UCI HAR Dataset/test/X_test.txt" # Training set.
        tst_y_dir <- "./UCI HAR Dataset/test/y_test.txt" # Training labels.
        tst_subject_dir <- "./UCI HAR Dataset/test/subject_test.txt" # Subject who performed the activity
        tst_x_total_acc_dir <- "./UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt"
        tst_y_total_acc_dir <- "./UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt"
        tst_z_total_acc_dir <- "./UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt"
        tst_x_body_acc_dir <- "./UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt"
        tst_x_body_gyro_dir <- "./UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt"
        tst_dir <- c("tst_x_dir","tst_y_dir","tst_subject_dir")

# Training Files  - directories
        trn_subject_dir <- "./UCI HAR Dataset/train/subject_train.txt"
        trn_x_dir <- "./UCI HAR Dataset/train/X_train.txt"
        trn_y_dir <- "./UCI HAR Dataset/train/y_train.txt"
        trn_x_total_acc_dir <- "./UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt"
        trn_y_total_acc_dir <- "./UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt"
        trn_z_total_acc_dir <- "./UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt"
        trn_x_body_acc_dir <- "./UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt"
        trn_x_body_gyro_dir <- "./UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt"
        trn_dir <- c("trn_x_dir","trn_y_dir","trn_subject_dir")

# load data sets and name them 
        tst_x <- RNS("tst_x",tst_x_dir)
        tst_y <- RNS("act",tst_y_dir)
        tst_subject <- RNS("sbjct_id",tst_subject_dir)
        trn_x <- RNS("trn_x",trn_x_dir)
        trn_y <- RNS("act",trn_y_dir)
        trn_subject <- RNS("sbjct_id",trn_subject_dir)


#rename colnames as features before combinging as one merged set. 
        colnames(tst_x) <- features[,2]
        colnames(trn_x) <- features[,2]

#create activities column and rows to bind to labels table
        tst_y <- merge(tst_y, act_lbls, by.x = "act.01", by.y= "actvs.01", sort=FALSE)
        trn_y <- merge(trn_y, act_lbls, by.x = "act.01", by.y= "actvs.01", sort=FALSE)


# create full test data table using cbinds and the dataframes 
        tst_all <- cbind(tst_subject,tst_y,tst_x) # Combine all Test data  - Set and Labels - 2947 Rows
        trn_all <- cbind(trn_subject,trn_y,trn_x) # Combine all Test data  - Set and Labels - 2947 Rows

# combine both test and training data sets
        t_all <- rbind(tst_all,trn_all)

# identify index where column is either mean or standard deviation , added 1 & 2 to include subject and activity in final index
        index_1 <- grep("std()",colnames(t_all),value=FALSE)
        index_2 <- grep("mean()",colnames(t_all),value=FALSE)
        index_all <- sort(c(1,3,index_1,index_2))

# subset specified measurements - #2 measurements on the mean and standard deviation for each measurement
        t_part <- t_all[index_all]

# use plyr-library for summerizing and creating an indipendant tidy data set 
require(plyr)
t_part_smry <- ddply( t_part, .(sbjct_id.01, actvs.02), numcolwise(mean))

# write the indipendant tidy data set to a new text document
write.table(t_part_smry, file = "indipendant_tidy_data.txt", sep = ",", row.name=FALSE )


