################ Working directory #################################

WD <- "C://Users//mediaworld//Desktop//Coursera//DataScience//RProgramming//Esercizi//"

if (!require("dplyr")) {
  install.packages("dplyr")
}

if (!require("reshape2")) {
  install.packages("reshape2")
}

require("dplyr")
require("reshape2")

############ Load Training Data ###################################

path_X_train <- "UCI HAR Dataset//train//X_train.txt"
c_path_X_train <- paste0(WD,path_X_train)
X_train <- read.table(c_path_X_train)



# ID activity 1 a 6
path_y_train <- "UCI HAR Dataset//train//y_train.txt"
c_path_y_train <- paste0(WD,path_y_train)
y_train <- read.table(c_path_y_train)


# ID Subject 1 a 30
path_subject_train <- "UCI HAR Dataset//train//subject_train.txt"
c_path_subject_train <- paste0(WD,path_subject_train)
subject_train <- read.table(c_path_subject_train)


# activity labels
path_activity_labels <- "UCI HAR Dataset//activity_labels.txt"
c_path_activity_labels <- paste0(WD,path_activity_labels)
activity_labels <- read.table(c_path_activity_labels)


# variable labels
path_features <- "UCI HAR Dataset//features.txt"
c_path_features <- paste0(WD,path_features)
features <- read.table(c_path_features)




############# Load Test Data ######################################

path_X_test <- "UCI HAR Dataset//test//X_test.txt"
c_path_X_test <- paste0(WD,path_X_test)
X_test <- read.table(c_path_X_test)



# ID activity 1 a 6
path_y_test <- "UCI HAR Dataset//test//y_test.txt"
c_path_y_test <- paste0(WD,path_y_test)
y_test <- read.table(c_path_y_test)



# ID Subject 1 a 30
path_subject_test <- "UCI HAR Dataset//test//subject_test.txt"
c_path_subject_test <- paste0(WD,path_subject_test)
subject_test <- read.table(c_path_subject_test)





# 1.Merges the training and the test sets to create one data set
X <- rbind(X_train,X_test)




# 2.Extracts only the measurements on the mean and standard deviation for 
# each measurement.
mean_std_col <- grep ("[m][e][a][n][(][)]|std",features[,2])
X <- select(X,mean_std_col)



# 3.Uses descriptive activity names to name the activities in the data set
y_train[,1] <- factor(y_train[,1],levels = activity_labels[,1],
                      labels =activity_labels[,2])
y_test[,1] <- factor(y_test[,1],levels = activity_labels[,1],
                     labels =activity_labels[,2])

y <- rbind(y_train,y_test)

X <- mutate(X,activity=y[,1])




# 4.Appropriately labels the data set with descriptive variable names.
variableName<- features[mean_std_col,2]
names(X) <- c(as.character(variableName),"activity")



# 5.From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each 
# subject.

subject <- rbind(subject_train,subject_test)
X2 <- mutate(X,subject=subject[,1])
dataMelt <- melt(X2,id=c("activity","subject"),measure.vars = variableName)

# Apply mean function to dataset using dcast function
tidy_data   = dcast(dataMelt, subject + activity ~ variable, mean)

write.table(tidy_data, file = paste0(WD,"tidy_data.txt"),row.name = FALSE)