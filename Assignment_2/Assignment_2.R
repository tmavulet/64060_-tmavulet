UniversalBank <- read.csv("C:/Users/mavul/Downloads/UniversalBank.csv")

summary(UniversalBank)

#Remove the Null variables
UniversalBank$ID <- NULL
UniversalBank$ZIP.Code <- NULL
summary(UniversalBank)

#Call the libraries
library(caret)
library(class)
library(ggplot2)
library(lattice)
library(FNN)

summary(UniversalBank)
UniversalBank$Personal.Loan=as.factor(UniversalBank$Personal.Loan)
summary(UniversalBank)
                                  
#Question 1 ----------
#Normalize the data

Bank_Norm <- UniversalBank
Norm_model<-preProcess(UniversalBank[,-7],method = c("center","scale"))
Bank_Norm[,-7]=predict(Norm_model, UniversalBank[,-7])
summary(Bank_Norm)
                                                         
#Train the data
                                                         
Train_Index=createDataPartition(UniversalBank$Personal.Loan,p=0.6, list = FALSE)
Train.df=Bank_Norm[Train_Index,]
Validation.df=Bank_Norm[-Train_Index,]
                                                         
#Predicting the data frame
                                                         
To_Predict=data.frame(Age=40, Experience=10, Income=84, Family=2, CCAvg=2,
                      Education=1, Mortgage=0, Securities.Account=0, CD.Account=0,
                      Online=1,CreditCard=1)

                                                        
print(To_Predict)
 
To_Predict_norm<-predict(Norm_model,To_Predict) 
print(To_Predict_norm) 

Prediction <- knn(train = Train.df[,1:7], test = To_Predict_norm[,1:7], 
                  cl=Train.df$Personal.Loan, k=1)
print(Prediction)

# --- This shows that the 5 nearest neighbors will be classified as 0 and the customer will also be 0.

#Question 2 ----------
set.seed(123)
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 2)
searchGrid=expand.grid(k=1:10)
                                                         
                                                         
knn.model=train(Personal.Loan~., data = Train.df, method = 'knn', tuneGrid = searchGrid, trControl = fitControl)

knn.model

# --- We can see that the best choice which balances the model from over fitting is k=3

#Question 3 ----------
predictions <- predict(knn.model,Validation.df)
confusionMatrix(predictions,Validation.df$Personal.Loan)

# --- This is the confusion matrix.
                                                         
#Question 4 ----------                                                        
To_Predict=data.frame(Age=40, Experience=10, Income=84, Family=2, CCAvg=2,
                      Education=1, Mortgage=0, Securities.Account=0, CD.Account=0,
                      Online=1,CreditCard=1)
                                                         
To_Predict_norm=predict(Norm_model,To_Predict)
                                                         
predict(knn.model,To_Predict_norm)

# --- The customer is classified as 1.
#Question 5 ------------
set.seed(123)
Test_Index_1 = createDataPartition(UniversalBank$Age, p= 0.2 , list=FALSE)  
Test_Data_1  = UniversalBank [Test_Index_1,]
Rem_DATA = UniversalBank[-Test_Index_1,] 
head(Rem_DATA)

Train_Index_1 = createDataPartition(Rem_DATA$Age, p= 0.5 , list=FALSE)
Train_Data_1 = Rem_DATA[Train_Index_1,] 
Validation_Data_1 = Rem_DATA[-Train_Index_1,] 
head(Validation_Data_1)

train.norm.df_1 <- Train_Data_1
valid.norm.df_1 <- Validation_Data_1
test.norm.df_1 <- Test_Data_1
rem_data.norm.df_1 <- Rem_DATA

norm.values_1 <- preProcess(Train_Data_1[-7], method=c("center", "scale"))
train.norm.df_1[-7] <- predict(norm.values_1, Train_Data_1[-7])  
valid.norm.df_1[-7] <- predict(norm.values_1, Validation_Data_1[-7])
test.norm.df_1[-7] <- predict(norm.values_1, test.norm.df_1[-7]) 

rem_data.norm.df_1[-7] <- predict(norm.values_1,Rem_DATA[-7]) 
head(test.norm.df_1)

#--- Therefore, the model is best fit on the training data and is most accurate on the training data and the least on the testing data.