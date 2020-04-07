library(reshape2)
library(psych)
library(dplyr)

#### Feature Extraction process

#Specify your folder name and create a list of files_names present in the folder
folder<-"D:/Jacobs/Spring 2020/Data Mining/BBDC/Subject01/"
file_list <- list.files(path=folder,pattern="*.csv")
#Load your provided training labels
train_labels<-read.csv(file.choose(), header = T)


#The loops uploads each file from the mentioned folder.
#Features are extracted from each file and it's reduced to just 1 obs.
#The corresponding label is added to the file and finally written to a csv file.
#The process is repeated for all files present in the file_list.
#In the final dataset in each folder, we have 440 obs with their corresponding labels.

for (i in 1:length(file_list)){
  df<-read.csv(paste(folder, file_list[i], sep=''))
  colnames(df)<-c("emg1","emg2","emg3","emg4","airborne", "acc_u_x" , "acc_u_y" , "acc_u_z" , "gonio_x",
                      "acc_l_x","acc_l_y" , "acc_l_z", "gonio_y","gyro_u_x", "gyro_u_y","gyro_u_z",
                      "gyro_l_x","gyro_l_y","gyro_l_z")
  
  #Imputes the missing values with their median
  if(sum(is.na(df)!=0)){
    preProcValues <- preProcess(df, method = c("medianImpute","center","scale"))
    df <- predict(preProcValues, df)
  }
  
  # Seperate dataframe into two parts. We create new DFs for emg at 1000Hz 
  #and other sensors at 100 Hz
  df1<-df[,c(1:5)]
  df2<-df[,c(6:19)]
  # Keeping only unique obs. in df2
  df2<-df2 %>% dplyr::filter(row_number() %% 11 == 1)
  
  #Extract features from both these dataframes and reduce them to a single line
  #Reduce df1 first
  
  df1_fe<- describe(df1)
  df1_fe<-df1_fe[,-c(1,2)]
  df1_fe_trans<- t(df1_fe)
  my.result1 <- melt(df1_fe_trans, id = c("emg1","emg2","emg3","emg4","airborne",))  
  
  my.result1$attribute<- paste(my.result1$Var1,my.result1$Var2) #Concatinate two columns in a single column
  my.result1<- my.result1[, c(4,3)] #Take only relevant columns and remove others

  my.result1<- t(my.result1) # Transpose to see the result as desired
  
  #Reduce df2 now
  
  df2_fe<- describe(df2)
  df2_fe<-df2_fe[,-c(1,2)]
  df2_fe_trans<- t(df2_fe)
  my.result2 <- melt(df2_fe_trans, id = c("acc_u_x" , "acc_u_y" , "acc_u_z" , "gonio_x",
                                         "acc_l_x","acc_l_y" , "acc_l_z", "gonio_y","gyro_u_x", "gyro_u_y","gyro_u_z",
                                         "gyro_l_x","gyro_l_y","gyro_l_z",))  
  
  my.result2$attribute<- paste(my.result2$Var1,my.result2$Var2) #Concatinate two columns in a single column
  my.result2<- my.result2[, c(4,3)] #Take only relevant columns and remove others

  my.result2<- t(my.result2) # Transpose to see the result as desired
  
  # Merge both the dataframes into a single one.
  de <- merge(my.result1, my.result2, by=0, all=TRUE)
  
  #Add corresponding labels
  for (j in 1:dim(train_labels)[1]) {
    if(grepl(file_list[i],train_labels[c(j),c(2)])){
      de$label<-train_labels[c(j),c(3)]
      
    }
  }
  
  # Write your dataframe to a csv file
  if(i == 1){
    write.table(de,file=paste(folder, "result_1.csv", sep=''),row.names = F,col.names = F,append=T,sep=',')
  }
  if(i>1){
    de<-de[-c(1),]
    write.table(de,file=paste(folder, "result_1.csv", sep=''),row.names = F,col.names = F,append=T,sep=',')
  }
}
#################################################################################

# Combine all result.csv from all 15 folders into a single training dataset

#Specify your folder name and make a list of fie names
folder<-"D:/Jacobs/Spring 2020/Data Mining/BBDC/Final/"
file_list1 <- list.files(path=folder,pattern="*.csv")

#Merge all the resultant files from the 15 folders to 
#create the complete training dataset
for (i in 1:length(file_list1)){
  df_final<-read.csv(paste(folder, file_list1[i], sep=''))
  # Write your dataframe to a csv file
  if(i == 1){
    write.table(df_final,file=paste(folder, "test_data.csv", sep=''),row.names = F,col.names = T,append=T,sep=',')
  }
  if(i>1){
    
    write.table(df_final,file=paste(folder, "test_data.csv", sep=''),row.names = F,col.names = F,append=T,sep=',')
  }
}

################################################################################

## *** We have to follow the same process for creating the Test data from the four
## *** Subjects namely, 1,10,14,15, except for adding the labels

