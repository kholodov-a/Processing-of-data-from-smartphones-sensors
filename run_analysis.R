## Checking if the source dataset exists. If it doesn't exist (or is not unpacked)
## then download, extract it and set the working directory to it

if (!dir.exists("./UCI HAR Dataset")) { 
    flSrc <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    flDest <- paste0(getwd(),"/UCI HAR Dataset.zip")
    if (!file.exists(flDest)) {download.file(flSrc, flDest)}
    unzip(flDest,exdir="./")  # unzip file 
    }
setwd("./UCI HAR Dataset")

library(dplyr)

## Reading Activities labels
actLabels <- read.table("activity_labels.txt", header = FALSE)

## Reading X_test and X_train datasets and binding them together
X_testDS <- read.table("./test/X_test.txt", header = FALSE)
X_trainDS <- read.table("./train/X_train.txt", header = FALSE)
xDS <- rbind(X_testDS, X_trainDS)

## Reading Y_test and Y_train datasets, mutating the activity number in the first 
## columns of both datasets to the descriptive activity names from the activity 
## labels dataset. Binding both datesets together in the same order as 
## X-datasets ("_train" after "_test")
Y_testDS <- read.table("./test/y_test.txt", header = FALSE) %>%
    mutate(V1 =actLabels[V1,2])
Y_trainDS <- read.table("./train/y_train.txt", header = FALSE) %>%
    mutate(V1 = actLabels[V1,2])
yDS <- rbind(Y_testDS, Y_trainDS)

## Reading and merging subject labels (IMPORTANT: again "_test" should go 
## before "_train")
sbj_testDS <- read.table("./test/subject_test.txt", header = FALSE)
sbj_trainDS <- read.table("./train/subject_train.txt", header = FALSE)
sDS <- rbind(sbj_testDS, sbj_trainDS)

## Reading the names of variables from "feature.txt" and applying them to the dataset
msr <- read.table("features.txt", header = FALSE)
names(xDS) <- msr[,2]

## Identifying variables with measurements of mean and standard deviation and 
## selecting them for the new dataset
flds <- msr[grep("mean\\(|std\\(", msr$V2),2]
xDS <- xDS %>% select(all_of(flds))

## Merging subjects, Y labels and selected variables from the previous step AND
# saving it to the working directory as "fullDS.txt"
rowDS <- cbind(sDS, yDS, xDS)
names(rowDS)[1:2] <- c("Subject", "Activity")
write.table(rowDS, file = "fullDS.txt", row.name=FALSE)

## Creating and saving to the working directory a second, independent tidy datast 
## with the average of each variable for each activity and each subject
tidyDS <- mutate(rowDS, Subject = factor(Subject), Activity = factor(Activity))
tidyDS <- tidyDS %>% group_by(Subject, Activity) %>% 
    summarise(across(everything(), mean), .groups = 'keep') %>%
    as.data.frame()
write.table(tidyDS, file = "tidyDS.txt", row.name=FALSE)

