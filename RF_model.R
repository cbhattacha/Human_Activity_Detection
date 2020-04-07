# Import required libraries
library(e1071)
library(caret)
library(e1071)
set.seed(1)

#Choose your training dataset that you have prepared in the last file
df<-read.csv(file.choose(),header=T)
# Remove the first column as it's not required
df<-df[,-c(1)]
#Check data type of all your predictors
str(df)

#Rename the column_name of the last colummn
df<-df %>% 
  rename(
    label=curve.left.step
  )

#Check for NA values
sum(is.na(df))

# Remove the NA rows as they are very less in number.
df<-na.omit(df)
sum(is.na(df)) # Cross check, now result should b 0

#Format of the levels in column label is changed, s.t. it doesn't produce error.
levels(df$label) <- make.names(levels(factor(df$label)))

#Split into Training and Validation set.
index <- createDataPartition(df$label, p=0.75, list=FALSE)
trainSet <- df[ index,]
valSet <- df[-index,]

#Defining the training controls for the model
fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

#Defining the predictors and outcome
x<-trainSet[,c(1:209)]
y<-trainSet[,c(210)]
x_val<-valSet[,c(1:209)]

# This is when you load the test dataset created from the previous file.
testSet<-read.csv(file.choose(),header=T)
x_test<-testSet[,-c(1)]

# Model with Random Forest
model_rf<-train(x,y,method='rf',trControl=fitControl,tuneLength=3)

#Prediction with validation set. For our model it's 97%
valSet$pred_rf<-predict(object = model_rf,x_val)
confusionMatrix(valSet$label,valSet$pred_rf)

#Prediction with test set
testSet$pred_rf<-predict(object = model_rf,x_test)

#Upload the provided challenge.csv
challenge_results<-read.csv(file.choose(),header=T)
challenge_results$Label<-testSet$pred_rf
# Write results to  csv file
write.csv(challenge_results,file="challenge_results.csv")


# The accuracy on test DS can be checked as we have the true labels
#Upload the file with true labels. challenge_labels.csv
chlab<-read.csv(file.choose(),header = T)
levels(chlab$Label) <- make.names(levels(factor(chlab$Label)))

#check accuracy for the unseen subjects. For our model it's 83.6 %
testSet$true_label<-chlab[,c(3)]
confusionMatrix(testSet$true_label,testSet$pred_rf)




