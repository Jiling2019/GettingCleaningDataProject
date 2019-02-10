This code book describes the variables, the data, and any transformations or work that were performed to reach the tidy data set,"tidydata.txt". 

Files used in cleaning data:
============================
After unzipping the Human Activity Recognition Using Smartphones Data Set dowloaded from the url https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip, the following files were selected to prepare a tidy data:
(The files descriptions were modified from the original README.txt from the data source)
- 'features.txt': Name list of all 561 features.
- 'activity_labels.txt': Links the 6 class labels with their activity name.
- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. 7352 observation and 1 variables (volunteer id from 1 to 30).
- 'train/X_train.txt': Training set with 7352 observation and 561 variables(features).
- 'train/y_train.txt': Training labels.7352 observation and 1 variables (class from 1 to 6).
- 'test/subject_test.txt': Each row identifies the subject who performed the activity for each window sample. 2947 observation and 1 variables (volunteer id from 1 to 30).
- 'test/X_test.txt': Test set with 2947 observation and 561 variables(features)
- 'test/y_test.txt': Test labels. 2947 observation and 1 variables (class from 1 to 6).

Transformations or work that were performed for cleaning data
=============================================================
1. Read test files,'subject_test.txt','test/X_test.txt' and 'test/y_test.txt' into a data frame.
2. Read train files,'subject_train.txt','train/X_train.txt' and 'train/y_train.txt' into a data frame.
3. Merge the test and train data sets vertically.
4. Name column names of the merged data set using 'features.txt'.Call the subject variable "subject" and the variable from y_test.txt and y_train.txt "activity".
5. Extract only the measurements on the mean and standard deviation for each measurement.
6. Use file 'activity_labels.txt' to name the "activity" variable value.
7. Tidying the extracted data set by gather all features variables and then seperate it into variables "feature", "measure","direction".
8. Group all variables and then calculate the average of each measurement for each activity and each subject.

Description for the 6 variables in the tidy dataset
=======================================================
(The following descriptions were extract and modified from the file 'features_info.txt' and the original 'README.txt' from the data source.)
The orignal dataset obtained from the experiments has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. All variables in the new tidy data set includes valuse from both the train and test data. 

[1] "activity" :Six activities performed by each subject (person) in the experiments.(WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)

[2] "subject" : Each row identifies the subject who performed the activity. Its range is from 1 to 30. The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years.

[3] "feature": stating the signal feature. 17 different features. 

[4] "measure": stating statistics measure is mean:Mean value or std:Standard deviation

[5] "direction": stating the signal direction: X,Y,Z or not stating.  

[6] "mean" : The mean value of each measurement for each activity and each subject. 

More information of the measurements(from 'features_info.txt') :
The features selected for the original database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
The set of variables in original data sets that were estimated from these signals are: 
mean(): Mean value
std(): Standard deviation

