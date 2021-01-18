# Configure environment
rm(list=ls())
setwd("C:/Users/Timo/Desktop/Prog/Coursera/Getting and Cleaning Data/Woche 4/Datasets/UCI HAR Dataset")

# Importing the Data
features=read.table('./features.txt',header=FALSE);
activityLabels=read.table('./activity_labels.txt',header=FALSE);
subjectTrain=read.table('./train/subject_train.txt',header=FALSE);
subjectTest=read.table('./test/subject_test.txt',header=FALSE);
XTrain=read.table('./train/X_train.txt',header=FALSE);
yTrain=read.table('./train/y_train.txt',header=FALSE);
XTest=read.table('./test/X_test.txt',header=FALSE);
yTest=read.table('./test/y_test.txt',header=FALSE);

# Assigning column names
colnames(activityLabel)=c('activityId','activityType');
colnames(subjectTrain)="subjectId";
colnames(subjectTest)="subjectId";
colnames(XTrain)=features[,2]; 
colnames(yTrain)="activityId";
colnames(XTest)=features[,2]; 
colnames(yTest)="activityId";

# Part 1
# Merging the training and the test sets to create one data set
trainingset=cbind(subjectTrain,XTrain,yTrain)
testset=cbind(subjectTest,XTest,yTest)
data=rbind(trainingset,testset)

# Part 2
# Creating MeasurementsVector that extracts only measuerements on the mean and standard deviation for each measurement
colNames=colnames(data);
MeasurementsVector=(grepl("activity..",colNames)|grepl("subject..",colNames)|grepl("-mean..",colNames)&!grepl("-meanFreq..",colNames)&!grepl("mean..-",colNames)|grepl("-std..",colNames)&!grepl("-std()..-",colNames));
data=data[MeasurementsVector==TRUE];

# Part 3
# Merging the data set with the activityLabels table to name the activities in the data set
data=merge(data,activityLabels,by='activityId',all.x=TRUE);
colNames=colnames(data);

# Part 4
# Cleaning variable names to label the data set with descriptive activity names
for (i in 1:length(colNames))
{
  colNames[i]=gsub("\\()","",colNames[i])
  colNames[i]=gsub("^(t)","time",colNames[i])
  colNames[i]=gsub("^(f)","frequency",colNames[i])
  colNames[i]=gsub("Acc","Accelerometer",colNames[i])
  colNames[i]=gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i]=gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i]=gsub("[Gg]yro","Gyroscope",colNames[i])
  colNames[i]=gsub("Jerk","Jolt",colNames[i])
  colNames[i]=gsub("Mag","Magnitude",colNames[i])
  colNames[i]=gsub("-mean","Mean",colNames[i])
  colNames[i]=gsub("-std$","StandardDeviation",colNames[i])
};
colnames(data)=colNames;

# Part 5
# Creating a second, independent tidy data set with the average of each variable for each activity and each subject.
newdata=data[,names(data)!='activityLabels'];
tidydata=aggregate(newdata[,names(newdata)!=c('activityId','subjectId')],by=list(activityId=newdata$activityId,subjectId=newdata$subjectId),mean);
tidydata=merge(tidydata,activityLabels,by='activityId',all.x=TRUE);
write.table(tidydata, './tidyDataset.txt',row.names=TRUE,sep='\t');
