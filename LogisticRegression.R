#To export the data user has to install mlbench package using the command install.packages("mlbench") 


data(BreastCancer, package="mlbench")
mydata <- BreastCancer[complete.cases(BreastCancer), ]  # create copy
View(mydata)

#################### Data table reformatting ##################################
# remove id column
mydata <- mydata[,-1]

# Change malignant values to 1's else 0's
mydata$Class <- ifelse(mydata$Class == "malignant", 1, 0)
mydata$Class <- factor(mydata$Class, levels = c(0, 1))
table(mydata$Class)

# convert to numeric
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
library(caret)
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

# Build Logistic Model
logitmod <-  glm(Class ~ .,
                 family = "binomial", data=up_train)  #For all columns

summary(logitmod)

logitmod <- glm(Class ~ Cl.thickness + Marg.adhesion + Bare.nuclei + Bl.cromatin, 
                family = "binomial", data=up_train) #For selected columns

summary(logitmod)

confint(logitmod)

#Predict with test dataset

pred <- predict(logitmod, testData, type = "response")
pred

# Recode factors
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class

# Accuracy
mean(y_pred == y_act)  # Accuracy Percentage


##########################################Correlation #####################################
library("ggpubr")
ggscatter(up_train, x = "Cl.thickness", y = "Class", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson")
#ggscatter(trainData, x = "Gender", y = "SpHb", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson")
#ggscatter(trainData, x = "perfusion_index", y = "SpHb", add = "reg.line", conf.int = TRUE,  cor.coef = TRUE, cor.method = "pearson")
#ggscatter(trainData, x = "pulse_rate", y = "SpHb", add = "reg.line", conf.int = TRUE,  cor.coef = TRUE, cor.method = "pearson")
#ggscatter(trainData, x = "oxygen_saturation", y = "SpHb", add = "reg.line", conf.int = TRUE,  cor.coef = TRUE, cor.method = "pearson")

###################################ROCR#####################################

table(ActualValue=testData$Class, PredictedValue=pred>0.5)
library(ROCR)
ROCRPred = prediction(pred,testData$Class)
ROCRPref = performance(ROCRPred,"acc")
hist(pred)
plot(ROCRPref)
abline(h=0.95, v=0.1)
#plot(ROCRPref, colorsize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
###################################pROC######################################
library(pROC)
pROC_obj <- roc(testData$Class,pred,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")
#################################precred######################################
library(precrec)
precrec_obj <- evalmod(scores = pred, labels = testData$Class, mode="basic")
autoplot(precrec_obj) 
