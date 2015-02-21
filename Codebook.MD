#Coursera Tidy Data Project Codebook

Author: Christopher Young
Data: Feb 20, 2015


This codebook will go over the steps performed to create the tidy dataset file OUTPUT.TXT in this repository from the Human Activity Recognition Using Smart Phones study ( http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones ).   The dataset to recreate this output can be found in the Dataset folder under or can be downloaded at directly from the website  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 


==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Universit‡ degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

#Data

##Data Source

*UCI HAR Dataset

This is the original raw data set that can be downloaded from the link mentioned above. 

This dataset contains the following files
- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

##Data Output: 

*output.txt

This is the tidy dataset that has been created using the process outlined in the Readme.MD file by running the run_analysis.R file located in the same directory as the Readme.MD? and the Codebook.MD files. 

#Variables

###ActivityLabel (string)

Each of subject performed six different activities identified with the following lables
		Laying
		Sitting
		Standing
		Walking
		Walking_Downstairs
		Walking_Upstairs

###Subject(integers)

The experiment involved thirty individuals with the age-range of 19-48 years. Each individual 	is identified by the unique integer. 
		1:30
#Features

Contains the mean and standard deviation measurements from the original UCI HAR Dataset. 

From the file *feature_info.txt from the original UCI HAR Dataset

>##Feature Selection 
>=================
>
>The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals >tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a >constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass >Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration >signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc->XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
>
>Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk >signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional >signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, >tBodyGyroMag, tBodyGyroJerkMag). 
>
>Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, >fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to >indicate frequency domain signals). 
>
>These signals were used to estimate variables of the feature vector for each pattern:  
>’-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.


Using the notation above the tidy dataset contains the following 81 Features: 


- tBodyAcc.mean...X

- tBodyAcc.mean...Y

- tBodyAcc.mean...Z

- tGravityAcc.mean...X

- tGravityAcc.mean...Y

- tGravityAcc.mean...Z

- tBodyAccJerk.mean...X

- tBodyAccJerk.mean...Y

- tBodyAccJerk.mean...Z

- tBodyGyro.mean...X

- tBodyGyro.mean...Y

- tBodyGyro.mean...Z

- tBodyGyroJerk.mean...X

- tBodyGyroJerk.mean...Y

- tBodyGyroJerk.mean...Z

- tBodyAccMag.mean..

- tGravityAccMag.mean..

- tBodyAccJerkMag.mean..

- tBodyGyroMag.mean..

- tBodyGyroJerkMag.mean..

- fBodyAcc.mean...X

- fBodyAcc.mean...Y

- fBodyAcc.mean...Z

- fBodyAcc.meanFreq…X

- fBodyAcc.meanFreq...Y

- fBodyAcc.meanFreq...Z

- fBodyAccJerk.mean...X

- fBodyAccJerk.mean...Y

- fBodyAccJerk.mean...Z

- fBodyAccJerk.meanFreq...X

- fBodyAccJerk.meanFreq...Y

- fBodyAccJerk.meanFreq...Z

- fBodyGyro.mean...X

- fBodyGyro.mean...Y

- fBodyGyro.mean...Z

- fBodyGyro.meanFreq...X

- fBodyGyro.meanFreq...Y

- fBodyGyro.meanFreq...Z

- fBodyAccMag.mean..

- fBodyAccMag.meanFreq..

- fBodyBodyAccJerkMag.mean..

- fBodyBodyAccJerkMag.meanFreq..

- fBodyBodyGyroMag.mean..

- fBodyBodyGyroMag.meanFreq..

- fBodyBodyGyroJerkMag.mean..

- fBodyBodyGyroJerkMag.meanFreq..

- tBodyAcc.std...X

- tBodyAcc.std...Y

- tBodyAcc.std...Z

- tGravityAcc.std...X

tGravityAcc.std...Y

tGravityAcc.std...Z

tBodyAccJerk.std...X

tBodyAccJerk.std...Y

tBodyAccJerk.std...Z

tBodyGyro.std...X

tBodyGyro.std...Y

tBodyGyro.std...Z

tBodyGyroJerk.std...X

tBodyGyroJerk.std...Y

tBodyGyroJerk.std...Z

tBodyAccMag.std..

tGravityAccMag.std..

tBodyAccJerkMag.std..

tBodyGyroMag.std..

tBodyGyroJerkMag.std..

fBodyAcc.std...X

fBodyAcc.std...Y

fBodyAcc.std...Z

fBodyAccJerk.std...X

fBodyAccJerk.std...Y

fBodyAccJerk.std...Z

fBodyGyro.std...X

fBodyGyro.std...Y

fBodyGyro.std...Z

fBodyAccMag.std..

fBodyBodyAccJerkMag.std..

fBodyBodyGyroMag.std..

fBodyBodyGyroJerkMag.std..
