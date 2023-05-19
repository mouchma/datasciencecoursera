#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
#The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of 
#yes/no questions related to the project. You will be required to submit: 
    ##1) a tidy data set as described below, 
    ##2) a link to a Github repository with your script for performing the analysis, and 
    ##3) a code book that describes the variables, the data, and any transformations or work that you performed 
    ##   to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. 
    ##   This repo explains how all of the scripts work and how they are connected.

#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
#Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
#The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
#A full description is available at the site where the data was obtained:
#  
#  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
#
#Here are the data for the project:
#  
#  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  
#
#You should create one R script called run_analysis.R that does the following. 
    ## a) Merges the training and the test sets to create one data set.
    ## b) Extracts only the measurements on the mean and standard deviation for each measurement. 
    ## c) Uses descriptive activity names to name the activities in the data set

    ## d) Appropriately labels the data set with descriptive variable names. 

#From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.

#set working directory
setwd("C:/Users/maril/OneDrive/Documents/R Data Science/Course3/project/UCI HAR Dataset")


## a) Merge the training and the test sets to create one data set.

#Note:  from the discussion forums, what we really need to do is first *append* X_train to X_test then *append*  y_train to y-test
#       **then**  merge the training and test sets to create one data set

#Let's start by importing the files and see what they look like

#it seems like features contain measurement descriptions
features <- read.table("features.txt")
head(features)
setnames(features,1,"obs")
setnames(features,2,"measure")
head(features)

#it seems like activities contain descriptions for the y tables -- lookup table for y_test and y_train
activities <- read.table("activity_labels.txt")
head(activities)
setnames(activities,1,"activity")
setnames(activities,2,'activity_description')
head(activities)

#it seems like the subject tables contain the subject ids that have a 1:1 mapping to the x_test and x_train data on observation number 
# my opinion is that this is a weird way to set up the table to join by observation number instead of having a key variable but, "oh well"
subject_test <- read.table("test/subject_test.txt")
#head(subject_test)
setnames(subject_test,1,"subject_id")
#head(subject_test)

#OK, I think I get it.  
##the features table is a lookup table for the measurements in the x table 
##the activities table is a lookup table for the activities in the y table 
#the x data contains the measurements 
#the x data gets var names from the features table

x_test <- read.table("test/X_test.txt", col.names = features$measure)


#cannot use inner_join because x_test does not have an observation number to join on - cbind instead
#and just "trust" that the observation order lines up
x_test <- cbind(subject_test,x_test)
head(x_test)

y_test <- read.table("test/y_test.txt", col.names = "activity")
y_test <- inner_join(activities,y_test,by=c("activity"="activity"))
#head(y_test)

#take a leap of fatih here and cbind the x_test and y_test data based on observation number (not on the value of a variable)
test_data=cbind(y_test,x_test)

#now, follow the same process for the training data
subject_train <- read.table("train/subject_train.txt", col.names = "subject_id")
x_train <- read.table("train/X_train.txt", col.names = features$measure)
x_train <- cbind(subject_train,x_train)

y_train <- read.table("train/y_train.txt", col.names = "activity")
subject_train <- read.table("train/subject_train.txt", col.names = "subject_id")
y_train <- inner_join(activities,y_train,by=c("activity"="activity"))

train_data<-cbind(y_train,x_train)


## Now ready to merge the training and the test sets to create one data set.
test_train<-rbind(test_data,train_data)

## b) Extracts only the measurements on the mean and standard deviation for each measurement. 
my_tidy_data <- select(test_train, subject_id, activity_description, contains("mean"), contains("std"))



#c) Uses descriptive activity names to name the activities in the data set
# DONE - in code above
# completed this step in the code above with these two commands
#y_test <- inner_join(activities,y_test,by=c("activity"="activity"))
#y_train <- inner_join(activities,y_train,by=c("activity"="activity"))

## d) Appropriately labels the data set with descriptive variable names. 

#Terms from the features_info file
#acceleration
#body
#gravity
#body linear acceleration
#angular velocity
#magnitude
#'f' to indicate frequency domain signals
#(prefix 't' to denote time)



names(my_tidy_data)<-gsub("BodyBody", " body ", names(my_tidy_data))
names(my_tidy_data)<-gsub("Acc", " acceleration ", names(my_tidy_data))
names(my_tidy_data)<-gsub("Gyro", " Gyroscope ", names(my_tidy_data))
names(my_tidy_data)<-gsub("Jerk", " Jerk signals ", names(my_tidy_data))
names(my_tidy_data)<-gsub("Mag", " magnitude ", names(my_tidy_data))
names(my_tidy_data)<-gsub("^t", " Time_", names(my_tidy_data))
names(my_tidy_data)<-gsub("^f", " Frequency_", names(my_tidy_data))
names(my_tidy_data)<-gsub("meanFreq", " mean frequency ", names(my_tidy_data), ignore.case = TRUE)
names(my_tidy_data)<-gsub(".mean", "_mean", names(my_tidy_data), ignore.case = TRUE)
names(my_tidy_data)<-gsub(".std", "_standard deviation", names(my_tidy_data), ignore.case = TRUE)
names(my_tidy_data)<-gsub("...X", "_X", names(my_tidy_data))
names(my_tidy_data)<-gsub("...Y", "_Y", names(my_tidy_data))
names(my_tidy_data)<-gsub("...Z", "_Z", names(my_tidy_data))
write.table(my_tidy_data, "full_tidy_dataset.txt", row.name=TRUE)

#From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.
tidy_data <- my_tidy_data %>%
  group_by(subject_id, activity_description) %>%
  summarize_all(funs(mean))
write.table(tidy_data, "tidy_data.txt", row.name=TRUE)

#generate the columnames to update the codebook
names(tidy_data)

