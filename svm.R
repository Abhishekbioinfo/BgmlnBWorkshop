
#install.packages("mlbench")
#install.packages('e1071')

library(e1071)        # SVM methodology
library(caret)        # Plotting

data(BreastCancer, package="mlbench")     #Importing data
mydata <- BreastCancer[complete.cases(BreastCancer), ]  # create copy
View(mydata)
# remove id column
mydata <- mydata[,-1]

# Change malignant values to 1's else 0's
# Encoding step where character data is converting into numeric

mydata$Class <- ifelse(mydata$Class == "malignant", 1, 0)
mydata$Class <- factor(mydata$Class, levels = c(0, 1))
table(mydata$Class)

# convert to numeric datatype
for(i in 1:9) {
  mydata[, i] <- as.numeric(as.character(mydata[, i]))
}
#summary  of data
summary(mydata)
str(mydata)
sapply(mydata[,1:9], sd)

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~Cl.thickness + Class, data = mydata)
## In this example when we are checking with with respect to change in Cell Thickness how
## many are belongs to Class 0 and 1

# Prep Training and Test data also taking care of class imbalanced

'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.
set.seed(100)
trainDataIndex <- createDataPartition(mydata$Class, p=0.7, list = F) #Segregating
#70% of training data
trainData <- mydata[trainDataIndex, ]
testData <- mydata[-trainDataIndex, ]

# Class distribution of train data
table(trainData$Class)

#### Taking Care off Class Imbalacing further using downSample and upsample function
# Down Sample (optional)
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$Class)

table(down_train$Class)


# Up Sample (optional)
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                     y = trainData$Class)

table(up_train$Class)

##Fitting SVM to the Training set

classifier = svm(formula = Class ~ Cl.thickness + Marg.adhesion + Bare.nuclei + Bl.cromatin,
                 data = up_train,
                 type = 'C-classification',
                 kernel = 'linear')

summary(classifier)

#Predicting the Test set results
pred = predict(classifier, testData)

# Recode factors
y_pred_num <- ifelse(pred == 1, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class

# Accuracy
mean(y_pred == y_act)  # Accuracy Percentage

#Confusion Matrix
table(testData$Class, pred)

classifier$SV

plot(classifier, trainData, Cl.thickness ~ Marg.adhesion, slice=list(Bare.nuclei=3, Bl.cromatin=4))
plot(classifier, testData, Cl.thickness ~ Marg.adhesion, slice=list(Bare.nuclei=3, Bl.cromatin=4))
