run_analysis<-function(){
  
  #find all .txt files in train and test directories
  train<-list.files("train",pattern=".txt",full.names=TRUE)
  test<-list.files("test",pattern=".txt",full.names=TRUE)  
  
  #read in "name" files
  features<-read.table("features.txt")
  activity<-read.table("activity_labels.txt")
  
  #read all the data files to train_data and test_data
  subject_data<-read.table(train[1])
  subject_data<-rbind(subject_data,read.table(test[1]))
  
  x_data<-read.table(train[2])
  x_data<-rbind(x_data,read.table(test[2]))
  
  y_data<-read.table(train[3])
  y_data<-rbind(y_data,read.table(test[3]))
  
  # giving the activity types descriptive names
  y_data<-activity[activity[y_data[,1],1],2]
  
  #setting column names
  colnames(x_data)<-features[,2]
  
  #greping the mean and std data
  meanval<-grep("mean()",features[,2],value=TRUE)
  stdval<-grep("std()",features[,2],value=TRUE)

  #adding std and mean names together
  names<-c(meanval,stdval)
  
  #subsetting mean and std data from x_data to x_datause
  x_datause<-x_data[,names]
  
  # combining all data to 1 tidy data frame
  data_tot<-x_datause
  data_tot[,"Subject_ID"]<-subject_data
  data_tot[,"Activity_class"]<-y_data

  #determining average mean and std values for each subject per activity class

  library(dplyr)

  data_plot<- group_by(data_tot,Subject_ID,Activity_class) %>% summarise_each(funs(mean))

  #writing data to file

  write.table(data_plot,"tidy_data.txt",row.name=FALSE)


}