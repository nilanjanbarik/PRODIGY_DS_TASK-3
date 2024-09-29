#Prodigy_DS_Task-3

# Decision Tree Classifier for Prediction

# We load the data and clean it by removing the missing values.

data3<-read.csv("E:\\EDUCATION\\Prodigy Infotech Internship\\bank.csv",header = TRUE,sep = ";")
head(data3)
data3<-na.omit(data3)


# Now we divide the data into two part in 2:1 ratio for training set and test set.

set.seed(542)
index<-sample(nrow(data3),3014)
training_data<-data3[index,]
head(training_data)
test_data<-data3[-index,]
head(test_data)
dim(training_data)
dim(test_data)

# Now we build a classification tree and for that we use "rpart" library fucntion.
# We attach the data for ease.

library(rpart)

attach(data3)

cart_model<-rpart(y~.,training_data,method = "class")
cart_model
plot(cart_model)
text(cart_model,use.n = TRUE)

# The classifier tree is presented .

# Now to predict whether a customer would purchase a product or not we go for prediction of test set and confusion matrix.

# We predict the above model for both training and test data.


cart_pred_train<-predict(cart_model,training_data,type = "class")
cart_pred_test<-predict(cart_model,test_data,type = "class")

# We now create confusion matrices for both training and test data.
# For that we need "caret" package.

library(ggplot2)
library(lattice)
library(caret)

# Confusion Matrix for training data.

train_con_mat<-confusionMatrix(cart_pred_train,as.factor(training_data$y))
train_con_mat

# Confusion Matrix for test data.

test_con_mat<-confusionMatrix(cart_pred_test,as.factor(test_data$y))
test_con_mat
