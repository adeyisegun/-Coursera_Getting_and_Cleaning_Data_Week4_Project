path <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 

# create a temp folder to load data 
from_dir <- tempfile()
t_dir <- tempfile()

download.file(path, t_dir, mode = "wb")

#unzip data into temp folder
unzip(t_dir, exdir = from_dir)

# read train data
x_train <- read.table(paste0(from_dir,"/UCI HAR Dataset/train/X_train.txt"))
y_train <- read.table(paste0(from_dir,"/UCI HAR Dataset/train/y_train.txt"))
s_train <- read.table(paste0(from_dir,"/UCI HAR Dataset/train/subject_train.txt"))

#read test data
x_test <- read.table(paste0(from_dir,"/UCI HAR Dataset/test/X_test.txt"))
y_test <- read.table(paste0(from_dir,"/UCI HAR Dataset/test/y_test.txt"))
s_test <- read.table(paste0(from_dir,"/UCI HAR Dataset/test/subject_test.txt"))

# read List of all features (variables)
features <- read.table(paste0(from_dir,"/UCI HAR Dataset/features.txt"))
library(dplyr)
features <- (t(select(features, "V2")))

# read activity label
activity_labels <- read.table(paste0(from_dir,"/UCI HAR Dataset/activity_labels.txt"))
activity_labels [,2] <- as.character(activity_labels[,2])

#Merging the train and test data - important outcome of the project
x_data <- rbind(x_test, x_train)
y_data <- rbind(y_test, y_train)
s_data <- rbind(s_test, s_train)

# rename the coloumns
names(x_data) <- features[1,]
y_data <- rename(y_data, "Activity" = "V1")
s_data <- rename(s_data, "Subject" = "V1")

#Create the main data table merging both table tables - this is the outcome of 1
data <- cbind(s_data,y_data,x_data)
data$Activity <- factor(data$Activity, levels = activity_labels[,1], labels = activity_labels[,2])

# Extracting only the measurements on the mean and standard deviation for each measurement
col_ext <- grep("-(mean|std).*",colnames(data))
ext_data <- select(data, Subject, Activity, all_of(col_ext))

colnames(ext_data) <- gsub("-mean","Mean", colnames(ext_data))
colnames(ext_data) <- gsub("-std","Std", colnames(ext_data))
colnames(ext_data) <- gsub("[-()]","", colnames(ext_data))

# From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.
tidy_data <- ext_data %>% group_by(Subject, Activity) %>% summarise_all(list(mean))

write.table(tidy_data, "tidy_dataset.txt", row.names = FALSE, quote = FALSE)