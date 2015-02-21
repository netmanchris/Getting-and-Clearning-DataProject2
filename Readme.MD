#Getting and Cleaning Data - JHU - Coursera ReadMe


******


This repository contains the work of Christopher Young to complete the week 3 project 2 assignement for the Cousera Getting and Cleaning Data course taught by John Hopkins University in February of 2015.

For more information on the Cousera Data Science Specialization in general and the Getting and Clearning Data course in particular, please visit the following link

https://www.coursera.org/specialization/jhudatascience/1?utm_medium=catalog

Contents
     Readme.md - This File
     Codebook.md - 
     Ouput.txt
     UCI HAR Dataset (original dataset downloaded from
     https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip )
     run_analysis.R  

##Goal
 You should create one R script called run_analysis.R that does the following. 
 
1.  Merges the training and the test sets to create one data set.
2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity   and each subject.

##Codebook

The codebook.MD file details the data, and describes the features of this dataset. 

##Data Processing

Data Processing was performed with the following steps and can be reproduced loading the run_analysis.R file in this folder and running the function
clean_raw_data()

The clean_raw_data performs the following transformations on the original UCI HAR dataset


1.  loads R ibraries needed needed to perform the transformation
2. loads the raw data needed for creating tidy dataset.
3. Adds meaningful column names for Test & Train datasets
4. Joins Test and Train files into a single data.frame using cbind function
5. Row Bind Test and Train Datasets using rbind
6. Assign the appropriate Column Names to the FullDataset with descriptive variable names. Test Names Sourced from features.txt file from UCI HAR zip file.
7. Use grep to identify features containing the words "mean" and "std" from the FullDataSet Column Names, subset out them out and combine them into a new data.frame.
8. Use colNames to assign Activity and ActivityLabel names to the appropriate Columns of the actLables dataframe.
9. use merge to combine the CombineParsedData and actLables dataframes using the "Activity" column as the key. Include all rows.
10. Use the dplyr summary, select, group_by, and summarise_each functions to create independent tidy dataset with average of each variable for each activity and subject
11. Write Tidy dataset to a file named output.txt

Please refer to the run_analysis.R file in this folder for complete code. Comments above can be used to match the exact steps used to reproduce this Tidy Data Set. 

#Loading the Tidy Data Set

The tidy data set can be viewed using the following commands in R
output <- read.table("output.txt", header = TRUE)
view(Output)

thanks to @David_Hood's Project FAQ located here https://class.coursera.org/getdata-011/forum/thread?thread_id=69 for this piece of code. 