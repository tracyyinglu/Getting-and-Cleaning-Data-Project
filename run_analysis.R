library(tidyverse)
rm(list = ls())

##import feature names and use as column names in the dataset
features <- read.delim("features.txt",header = FALSE,sep = "")
feature_names <- as.character(features[,2])
activity_label <- read.delim("activity_labels.txt",header = FALSE, sep = "")


##import train dataset and labels; name columns with feature names ; combine labels with dataset
trainset <- read.delim("train/X_train.txt",header = FALSE,sep = "")
trainlabel <- read.delim("train/y_train.txt",header = FALSE,sep = "")
trainsubject <- read.delim("train/subject_train.txt",header = FALSE,sep = "")
names(trainset) <- feature_names
names(trainlabel) <- "label"
names(trainsubject) <- "subject"
trainset2 <- cbind(trainsubject,trainlabel,trainset)

##import test dataset and labels; name columns with feature names; combine labels with dataset
testset <- read.delim("test/X_test.txt",header = FALSE,sep = "")
testlabel <- read.delim("test/y_test.txt",header = FALSE,sep = "")
testsubject <- read.delim("test/subject_test.txt",header = FALSE,sep = "")
names(testset) <- feature_names
names(testlabel) <- "label"
names(testsubject) <- "subject"
testset2 <- cbind(testsubject,testlabel,testset)

##combine train and test datasets, and extracts only the measurements on the mean and standard deviation 
alldata <- rbind(trainset2,testset2)
write.table(alldata,"alldata.csv",row.names = FALSE, sep=",")

##Extracts only the measurements on the mean and standard deviation for each measurement
col_names <- grep(".*(mean|std|label|subject).*", names(alldata),value = TRUE)
select_data <- subset(alldata, select = col_names)
select_data$label <- as.character(select_data$label)

## Uses descriptive activity names to name the activities in the data set
for (j in 1:6) {
      select_data$label <- sub(j,activity_label[j,2],select_data$label)  
} 

##Appropriately labels the data set with descriptive variable names
names(select_data) <- sub("^t","time",names(select_data))
names(select_data) <- sub("^f","frequency",names(select_data))
names(select_data) <- sub("Acc","Acceleration",names(select_data))
names(select_data) <- sub("Mag","Magnitute",names(select_data))


## creates a second, independent tidy data set with the average of each variable for each activity and each subject.
summary_mean <- select_data %>% group_by(subject,label) %>% summarise_all(mean)

for (i in 3:81) {
      names(summary_mean)[i] <- paste("average",names(summary_mean)[i],sep = "_")
}

write.table(summary_mean,"mean_summary.txt",sep = "",row.names = FALSE)