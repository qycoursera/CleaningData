download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "dataset.zip", method = "libcurl")
unzip("dataset.zip")
setwd("UCI HAR Dataset/")
activity_lables = read.csv("activity_labels.txt", sep = " ", header = F)
features = read.table("features.txt", header=F)
subject_train = read.csv("train/subject_train.txt", header=F)
y_train=read.csv("train/y_train.txt", header = F)
y_train$V1 = as.factor(y_train$V1)
levels(y_train$V1) <- activity_lables$V2
training = read.table("train/X_train.txt", header = FALSE)
mean_std_index = grepl("mean[(][)]|std[(][)]", features$V2)
names(training)<- features$V2
subtraining = training[,mean_std_index]
subtraining$subject = subject_train$V1
subtraining$activity = y_train$V1

subject_test = read.csv("test/subject_test.txt", header=F)
y_test=read.csv("test/y_test.txt", header = F)
y_test$V1 = as.factor(y_test$V1)
levels(y_test$V1) <- activity_lables$V2
testing = read.table("test/X_test.txt", header = FALSE)
names(testing)<- features$V2
subtesting = testing[,mean_std_index]
subtesting$subject = subject_test$V1
subtesting$activity = y_test$V1
res <- rbind(subtesting, subtraining)
splited = split(res, list(res$subject, res$activity))
compute_mean <-function(df){
  len = length(df)
  subject = df[1,len-1]
  activity = as.character(df[1, len])
  features_mean = as.list(colMeans(df[, c(-len, -(len-1))], na.rm = T))
  features_mean$activity=activity
  features_mean$subject = subject
  features_mean
}
dd <-lapply(splited, compute_mean)
df <- do.call(rbind.data.frame, dd)
write.table(df, file="../results.txt", row.names = F, )
