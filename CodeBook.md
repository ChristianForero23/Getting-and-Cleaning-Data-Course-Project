#This is the code book for the project

##How to get to the tinyData.txt:
1. Download data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.
2. Unzip it into working directory of R Studio.
2. Execute the R script.

## About the source data
Human Activity Recognition Using Smartphones Data Set. A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Here are the data for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## About R script
File with R code "run_analysis.R" performs the 5 following steps (in accordance assigned task of course work):   
1. Merges the training and the test sets to create one data set.
1.1 Reading files
2. Extracting  the measurements on the mean and standard deviation for each measurement.
3. Using descriptive activity names to name the activities in the data set
4. Properly labeling the dataset with descriptive variable names.
5. creating a second independent time data set with the average of each variable for each activity and each topic. 

The code assumes all the data is present in the same folder, un-compressed and without names altered.

##The data

The dataset includes the following files:

'README.txt'

'features_info.txt': Shows information about the variables used on the feature vector.

'features.txt': List of all features.

'activity_labels.txt': Links the class labels with their activity name.

'train/X_train.txt': Training set.

'train/y_train.txt': Training labels.

'test/X_test.txt': Test set.

'test/y_test.txt': Test labels.
