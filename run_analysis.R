# check if UCI HAR Dataset directory exists or not !
if (file.exists("UCI HAR Dataset")){
        #### Read the necessary files.
        
        # Read the training set file
        training <- read.table("UCI HAR Dataset/train/X_train.txt")
        # Read the training subject file
        train_sub_values <- read.table("UCI HAR Dataset/train/subject_train.txt")
        # Read train activity values 
        train_activity_values <- read.table("UCI HAR Dataset/train/y_train.txt")
                                       
        # Read the test set file
        test <- read.table("UCI HAR Dataset/test/X_test.txt")
        # Read the test subject file
        test_sub_values <- read.table("UCI HAR Dataset/test/subject_test.txt")
        # Read test activity values 
        test_activity_values <- read.table("UCI HAR Dataset/test/y_test.txt")
        
        # Read the feature names file
        feature_names <- read.table("UCI HAR Dataset/features.txt")
        
        # Read activity values file
        activity <- read.table("UCI HAR Dataset/activity_labels.txt",col.names=c("activity_id","activity"))
        
        # Add subject and activity values to the training and test sets
        
        training$subject = as.vector(train_sub_values[[1]])
        training$activity = as.vector(train_activity_values[[1]])
        test$subject = as.vector(test_sub_values[[1]])
        test$activity = as.vector(test_activity_values[[1]])
        
        
        ####Get the label names as a vector
        feature_labels = list()
        feature_labels <- as.vector(feature_names[[2]])
        # Add subject and activity column names
        feature_labels <-c(feature_labels,"subject_id","activity_id")
        
        ####Add column names to train_set_values and test_set_values
        colnames(training) <- feature_labels
        colnames(test) <- feature_labels
        
        #### Merge the training and test set
        myset <- rbind(training,test)
        
        
        #### Extracts only the measurements 
        #### on the mean and standard deviation for each measurement along with
        #### subject and activity and overwrites myset
        library(dplyr)
        myset <- select(myset,subject_id,activity_id,contains("-mean()"),contains("-std()"),-contains("-meanFreq()")) 
                        
        #### Modify the activity values with meaningful names
        myset_final <- merge(activity,myset,by="activity_id",all.y=T)
        myset_final <- select(myset_final,-activity_id)
        #### Modify the column variables with more meaningful names
        columnnames <- names(myset_final)
        #Remove ()
        columnnames <- gsub('\\()','',columnnames)
        # Replace std as stdDeviation
        columnnames <- gsub('std','stdDeviation',columnnames)
        #Modify t -> time , f -> frequency
        columnnames <- gsub('^t','Time',columnnames)
        columnnames <- gsub('^f','Frequency',columnnames)
        colnames(myset_final) <- columnnames
        
        ####  create an independent tidy data set with the average of each 
        ####  variable for each activity and each subject
        my_tidy_set <- ddply(myset_final, .(subject_id,activity), numcolwise(mean))

        # Write out the final tidy data set into a text file
        write.table(my_tidy_set,file="Tidy_data_set.txt",row.name=FALSE)
        
        
} else {
        print("UCI HAR Dataset is not available!Can't proceed.")        
}
