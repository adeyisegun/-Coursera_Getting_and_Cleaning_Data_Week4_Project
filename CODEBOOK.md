Getting and Cleaning Data - Course Project
==========================================

# Modifications

## the original data was modifies by

* Merging the training and the test sets to create one data set.
* Extracting only the measurements on the mean and standard deviation for each measurement.
* Using descriptive activity names to name the activities in the data set
* Appropriately labeling the data set with descriptive variable names.
* Creating a second, independent tidy data set with the average of each variable for each activity and each subject.


# Descriptions

## Identififiers
The first two columns - Subject and Activity - are Identifiers.
* Subject: the ID of the Subject
* Activity: the Name of the Activity performed by the subject when measurements were taken


'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean


## Measurements
As mentioned above,the variables remaining are just the calculated means and standard deviations of these sets of data:

* tBodyAccMeanX
* tBodyAccMeanY
* tBodyAccMeanZ
* tBodyAccStdX
* tBodyAccStdY
* tBodyAccStdZ
* tGravityAccMeanX
* tGravityAccMeanY
* tGravityAccMeanZ
* tGravityAccStdX
* tGravityAccStdY
* tGravityAccStdZ
* tBodyAccJerkMeanX
* tBodyAccJerkMeanY
* tBodyAccJerkMeanZ
* tBodyAccJerkStdX
* tBodyAccJerkStdY
* tBodyAccJerkStdZ
* tBodyGyroMeanX
* tBodyGyroMeanY
* tBodyGyroMeanZ
* tBodyGyroStdX
* tBodyGyroStdY
* tBodyGyroStdZ
* tBodyGyroJerkMeanX
* tBodyGyroJerkMeanY
* tBodyGyroJerkMeanZ
* tBodyGyroJerkStdX
* tBodyGyroJerkStdY
* tBodyGyroJerkStdZ
* tBodyAccMagMean
* tBodyAccMagStd
* tGravityAccMagMean
* tGravityAccMagStd
* tBodyAccJerkMagMean
* tBodyAccJerkMagStd
* tBodyGyroMagMean
* tBodyGyroMagStd
* tBodyGyroJerkMagMean
* tBodyGyroJerkMagStd
* fBodyAccMeanX
* fBodyAccMeanY
* fBodyAccMeanZ
* fBodyAccStdX
* fBodyAccStdY
* fBodyAccStdZ
* fBodyAccMeanFreqX
* fBodyAccMeanFreqY
* fBodyAccMeanFreqZ
* fBodyAccJerkMeanX
* fBodyAccJerkMeanY
* fBodyAccJerkMeanZ
* fBodyAccJerkStdX
* fBodyAccJerkStdY
* fBodyAccJerkStdZ
* fBodyAccJerkMeanFreqX
* fBodyAccJerkMeanFreqY
* fBodyAccJerkMeanFreqZ
* fBodyGyroMeanX
* fBodyGyroMeanY
* fBodyGyroMeanZ
* fBodyGyroStdX
* fBodyGyroStdY
* fBodyGyroStdZ
* fBodyGyroMeanFreqX
* fBodyGyroMeanFreqY
* fBodyGyroMeanFreqZ
* fBodyAccMagMean
* fBodyAccMagStd
* fBodyAccMagMeanFreq
* fBodyBodyAccJerkMagMean
* fBodyBodyAccJerkMagStd
* fBodyBodyAccJerkMagMeanFreq
* fBodyBodyGyroMagMean
* fBodyBodyGyroMagStd
* fBodyBodyGyroMagMeanFreq
* fBodyBodyGyroJerkMagMean
* fBodyBodyGyroJerkMagStd
* fBodyBodyGyroJerkMagMeanFreq