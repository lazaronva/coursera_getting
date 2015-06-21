# Initial explain.
# We use a GitHub basis created by Bruce Shin disponible at: 
# https://github.com/ssshow16/datasciencecoursera/blob/master/run_analysis.R
# to realise this analisys and to make the exercise.
# 
# 1. Merges the training and the test sets to create one data set.
## load train set, activities, subject
trainSet <- read.table(file="/home/lazaro/Documents/CURSOS/CURSO_GETTING_AND_CLEANING_DATA/Project/UCI_HAR_Dataset/train/X_train.txt",header=FALSE,allowEscapes=T)
trainActivities <- read.table(file="/home/lazaro/Documents/CURSOS/CURSO_GETTING_AND_CLEANING_DATA/Project/UCI_HAR_Dataset/train/y_train.txt")
trainSubject <- read.table(file="/home/lazaro/Documents/CURSOS/CURSO_GETTING_AND_CLEANING_DATA/Project/UCI_HAR_Dataset/train/subject_train.txt")
#
## merge loaded data
trainSet <- cbind(trainSet,activities=trainActivities,subject=trainSubject)
head(trainSet)
tail(trainSet)
## load test set, activities, subject
testSet <- read.table(file="/home/lazaro/Documents/CURSOS/CURSO_GETTING_AND_CLEANING_DATA/Project/UCI_HAR_Dataset/test/X_test.txt",header=FALSE,allowEscapes=T)
testActivities <- read.table(file="/home/lazaro/Documents/CURSOS/CURSO_GETTING_AND_CLEANING_DATA/Project/UCI_HAR_Dataset/test/y_test.txt")
testSubject <- read.table(file="/home/lazaro/Documents/CURSOS/CURSO_GETTING_AND_CLEANING_DATA/Project/UCI_HAR_Dataset/test/subject_test.txt")
## merge loaded data
testSet <- cbind(testSet,activities=testActivities ,subject=testSubject)
head(testSet)
tail(testSet)
## merge training and test sets
totalSet <- rbind(testSet, trainSet)
head(totalSet)
tail(totalSet)
# Conclusion_1: We create 563 columns with 10299 rows.
# this data.frame it's a combination of train and test data, activities and subjects
# not nominated that the row's number by totalSet = trainSet + testSet rows.
## load features names
features <- read.table(file="/home/lazaro/Documents/CURSOS/CURSO_GETTING_AND_CLEANING_DATA/Project/UCI_HAR_Dataset/features.txt",header=FALSE)
head(features)
tail(features)
feature.nm <- sapply(features$V2,function(x){
    x <- gsub("\\(","",x)
    x <- gsub("\\)","",x)
    x <- gsub(",","",x)
    x <- gsub("-","",x)
    x <- gsub("mean","Mean",x)
    x <- gsub("std","Std",x)
})
head(feature.nm)
tail(feature.nm)
# Conclusion_2: feature.nm data is a vector of 561 characters.
# Each character in feature.nm is a name of a measures by feature vectors computed.
# The list of measures can be read in Table 4.5 at Ortiz's work (Ortiz 2014, 59) disponible in:
# http://tdx.cat/bitstream/handle/10803/284725/TJLRO1de1.pdf?sequence=1
# In this case all computed data are in totalSet and need to put a column name.
## load features names
activities <- read.table(file="/home/lazaro/Documents/CURSOS/CURSO_GETTING_AND_CLEANING_DATA/Project/UCI_HAR_Dataset/activity_labels.txt",header=FALSE)
names(activities) <- c("class","name")
# head(activities)
# tail(activities)
## add "subject" name into features names
names(totalSet) <- c(feature.nm,"activities","subject")
head(totalSet)
tail(totalSet)
# Conclusion_3: All columns of data were nominated here.
#
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
totalSetmean <- sapply(totalSet, mean)
head(totalSetmean)
totalSetsd <- sapply(totalSet, sd)
head(totalSetsd)
tail(totalSetsd)
totalSetmeansd <- data.frame(totalSetmean, totalSetsd, check.rows = TRUE, check.names = TRUE)
head(totalSetmeansd)
tail(totalSetmeansd)
test <- c(feature.nm, "activities", "subject")
test
totalSetmeansd$feature <- test
tail(totalSetmeansd)
totalSetmeansd <- totalSetmeansd[, c("feature", "totalSetmean", "totalSetsd")]
tail(totalSetmeansd)
names(totalSetmeansd) <- c("feature","means","sdev")
tail(totalSetmeansd)
## change class to activitie names
totalSet$activities <- sapply(totalSet$activities, function(x) activities[activities$class==x,]$name)
head(totalSet$activities)
# Conclusion 4: We extract all means and sd of feature variables to understod about the values.
# We applied functions to another failes to verify the results.
# verifying results
#
# 3. Uses descriptive activity names to name the activities in the data set
## search column for mean and standard deviation
mean.col.nm <- names(totalSet)[grep("Mean",names(totalSet))]
std.col.nm <-  names(totalSet)[grep("Std",names(totalSet))]
mean.col.nm
std.col.nm
## make subset for mean and standard diviation
mean.std.ds <- totalSet[,c(mean.col.nm,std.col.nm,"activities","subject")]
mean.std.ds
tail(mean.std.ds)
#
## make tidy data.set with the average of each variable for each activity and each subject.  
tidyDataset <- aggregate(mean.std.ds[,!(names(mean.std.ds) %in% c("activities","subject"))],
                    list(mean.std.ds$activities,mean.std.ds$subject),mean)
#
names(tidyDataset)[1] <- "activities"
names(tidyDataset)[2] <- "subject"
head(tidyDataset)
tail(tidyDataset)
## Save tidy data
write.table(file="tidy.data.set.txt", tidyDataset, row.names=F,quote=F,se="\t")
read.table("tidy.data.set.txt")
