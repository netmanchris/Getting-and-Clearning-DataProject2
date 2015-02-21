 clean_raw_data <- function() {
  #load libraries needed
  library(dplyr)
  
  #Load the raw data needed for creating tidy dataset.
  testX     <- read.table("./Dataset/test/X_test.txt")
  trainX    <- read.table("./Dataset/train/X_train.txt")
  testSub   <- read.table("./Dataset/test/Subject_test.txt")
  trainSub  <- read.table("./Dataset/train//Subject_train.txt")
  testY     <- read.table("./Dataset/test/Y_test.txt")
  trainY    <- read.table("./Dataset/train/Y_train.txt")
  features  <- read.table("./Dataset/features.txt", stringsAsFactors = FALSE)
  actLabels <- read.table("./Dataset/activity_labels.txt", stringsAsFactors = FALSE)
  
  #Add Meaningful Column Names for Test & Train Datasets
  colnames(testY)    <- "Activity"
  colnames(trainY)   <- "Activity"
  colnames(testSub)  <- "Subject"
  colnames(trainSub) <- "Subject"
  
  #Join Test and Train files into a single data.frame using cbind 
  testdf  <- cbind(testY , testSub , testX )
  traindf <- cbind(trainY, trainSub, trainX)
  
  #Row Bind Test and Train Datasets using rbind
  FullDataSet <- rbind(testdf, traindf)
  
  # Assign the appropriate Column Names to the FullDataset with descriptive variable names. Test Names Sourced from features.txt file
  colnames(FullDataSet) <- c("Activity","Subject",features$V2)
  
  
  
  # Use grep to identify features containing the words "mean" and "std" from the FullDataSet Column Names, subset out them out 
  #Combines the Mean and Std subsets into a new data.frame
  CombineParsedData <- cbind(FullDataSet[,1:2], (FullDataSet[grepl("mean", colnames(FullDataSet))]), (FullDataSet[grepl("std", colnames(FullDataSet))]))
  
  
  # Use colNames to assign Activity and ActivityLabel names to the appropriate Columns of the actLables dataframe.
  colnames(actLabels) <- c("Activity", "ActivityLabel")
  
  #use merge to combine the CombineParsedData and actLables dataframes using the "Activity" column as the key. Include all rows. 
  
  mergedData <- merge(CombineParsedData, actLabels, by = "Activity", all = TRUE)
  
  # Create independent tidy dataset with average of each variable for each activity and subject
  summary_mergedData <-
    mergedData %>%
    select(-(Activity)) %>%
    group_by(ActivityLabel, Subject) %>%
    summarise_each(funs(mean))
  
  # Write output as a text file 
  write.table(summary_mergedData,file = "output.txt",row.names = FALSE)
}
