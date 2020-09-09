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

## About variables:   
* `xtrain`, `ytrain`, `xtest`, `ytest`, `subject_train` and `subject_test` contain the data from the downloaded files.
* `features` contains the correct names for the `xdata` dataset, which are applied to the column names stored in
