#Merges the training and the test sets to create one data set.
  #Read test files
    # Read txt files with names of the form "*test.txt"
    testfiles = list.files(path="./UCI HAR Dataset/test", pattern= '*test.txt')
    # Read txt files into a data list and transform it into a data frame 
    testdir ="./UCI HAR Dataset/test"
    test = lapply(paste(testdir,testfiles,sep="/"), read.table)
    test <-data.frame(test)
  #Read train files
    # Read txt files with names of the form "*train.txt"
    trainfiles = list.files(path="./UCI HAR Dataset/train", pattern= '*train.txt')
    # Read txt files into a data list and transform it into a data frame 
    traindir ="./UCI HAR Dataset/train"
    train = lapply(paste(traindir,trainfiles,sep="/"), read.table)
    train <-data.frame(train)
  #Merges test and train dataset
    testtrain <-rbind(test,train)
#Appropriately labels the data set with descriptive variable names.
    #name columns names
    features <-read.table("./UCI HAR Dataset/features.txt",stringsAsFactors = F)
    names(testtrain) <-c("subject",features[[2]],"activity")
#Extracts only the measurements on the mean and standard deviation for each measurement.
    meanstdcol <-grepl("mean\\(\\)|std\\(\\)",names(testtrain))
    meanstdcol[563] <-TRUE
    meanstdcol[1] <-TRUE
    meanstd <-testtrain[,meanstdcol]
    meanstd <-select(meanstd,"subject","activity",everything())
#Uses descriptive activity names to name the activities in the data set
    activitynames <-read.table("./UCI HAR Dataset/activity_labels.txt")
    meanstd$activity <- factor(meanstd$activity, labels=activitynames$V2)

#From the data set in step 4, creates a second, independent tidy data set with the average 
#of each variable for each activity and each subject.
    tidydata <-meanstd %>% 
      gather(key = "features", value = "value",-subject,-activity)%>%
      separate(features,c("feature","measure","direction"))%>%
      group_by(subject,activity,feature,measure,direction)%>%
      summarize(mean = mean(value))
      
#Transfer tidydata to a file
    write.table(tidydata, "tidydata.txt", row.names=FALSE)
