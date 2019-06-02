# read data from csv file
dat = read.csv("C:\\Users\\�ukasz\\Desktop\\RTut\\FifaPlayersClassification\\data1.csv", header = TRUE, sep = '#')
# remove rows with empty features
dat <- na.omit(dat)
dat_rows <- nrow(dat)
# calculate 80 centil of Overall feature
centil_80 = quantile(dat$Overall, seq(0,1, 0.20))[5]
good_ids = as.list(dat[dat$Overall > centil_80,2])
bad_ids = as.list(dat[dat$Overall <= centil_80,2])

# create matrix with labels
labels_mx = matrix("bad", dat_rows, 1)
# players with Overall > 66 have label 'good' (3213), the rest have 'bad' (14994), split closest to 20:80
labels_mx[dat$Overall > centil_80,1] = "good"
labels = as.list(labels_mx)

# remove feature 'Overall'
dat <- dat[,-8]

# convertion of factor feature to numeric value
convert_to_numeric_value <- function(amount) {
  if (nchar(amount) > 3) {
    without_euro_sign = substr(amount,4,nchar(amount))
    if (substr(without_euro_sign, nchar(without_euro_sign), nchar(without_euro_sign)) == "K")
      numeric = as.numeric(substr(without_euro_sign,1,nchar(without_euro_sign)-1))*1000
    else if (substr(without_euro_sign, nchar(without_euro_sign), nchar(without_euro_sign)) == "M")
      numeric = as.numeric(substr(without_euro_sign,1,nchar(without_euro_sign)-1))*1000000
    else
      numeric = as.numeric(without_euro_sign)
  }
  else
    numeric = as.numeric(amount)
  return (numeric)
}
amount = wagea[1]

convert_weight_values <- function(amount) {
  numeric = as.numeric(substr(amount,1,nchar(amount)-3))
  return (numeric)
}

# convert wage and value column
wages = as.character(dat$Wage)
values = as.character(dat$Value)
wages_converted <- matrix((sapply(wages, convert_to_numeric_value)), nrow(dat), 1)
values_converted <- matrix((sapply(values, convert_to_numeric_value)), nrow(dat), 1)

dat$Wage <- wages_converted
dat$Value <- values_converted

#changed this to be numeric values instead of list which cannot be used in training model
# dataset$Wage <- do.call(rbind, wages_converted)
# dataset$Value <- do.call(rbind, values_converted)

#delete lbs from weight
weights = as.character(dat$Weight)
weights_converted <- matrix((sapply(weights, convert_weight_values)), nrow(dat), 1)
dat$Weight <- weights_converted

# concatenate feature matrix with labels
dataset = cbind(dat, labels_mx)
colnames(dataset)[89] <- "Label"

#corelation between numerical features
mx = cbind(dat$Age, dat$Potential, dat$Height, dat$Overall)
correlation_mx = cor(mx)

#copy of dataset
dataset1 <- dataset

#proposed columns to delete
dataset1[, c("ID", "Name", "Photo", "Flag", "Club.Logo", "Joined", "Nationality",
            "Club", "Real.Face", "Loaned.From", "Contract.Valid.Until", "Jersey.Number", "Special", "Release.Clause")] = NULL
dataset1[, 1] = NULL
#Value and Wage gives Error in x[, i] <- frame[[i]] : number of items to replace is not a multiple of replacement length
#and cannot train randomForest
#now it works with them
# dataset1[, c("Value", "Wage")] = NULL 

#Remove all factor type values that have more than 53 categories
dataset1[, c("LS", "ST", "RS", "LW", "LF", "CF", "RF", "RW", "LAM", "CAM", "RAM", "LM", "LCM", "CM", "RCM", "RM", "LWB", "LDM", "CDM", "RDM"
             , "RWB", "LB", "LCB", "CB", "RCB", "RB", "Height")] = NULL

# Naive Bayes Classification
library(e1071)

# fit the Naive Bayes model with the whole dataset
Naive_Bayes_Model=naiveBayes(Label ~., data=dataset1)
# predict on the same dataset
NB_Predictions=predict(Naive_Bayes_Model, dataset1)
# confusion matrix to check accuracy
conf_mx <- table(NB_Predictions,dataset1$Label)

#macro-averaging
# accuracy for class 'bad' = 0.9786
acc1 <- conf_mx[1,1]/(conf_mx[1,1] + conf_mx[1,2]) #bad
# accuracy for class 'good' = 0.7066
acc2 <- conf_mx[2,2]/(conf_mx[2,2] + conf_mx[2,1]) #good
# macro-averaged accuracy = 0.8426 for fitting on full dataset and before feature selection
accuracy <- (acc1 + acc2) / 2

#0.91937
prec1 <- conf_mx[1,1] / (conf_mx[1,1] + conf_mx[2,1])
#0.9063
prec2 <- conf_mx[2,2] / (conf_mx[2,2] + conf_mx[1,2])
#0.9128429
precision <- (prec1 + prec2) / 2

#micro-averaging
# number of good predictions
ok_predictions <- conf_mx[1,1] + conf_mx[2,2]
# micro_averaged accuracy = 0.917 for fitting on full dataset and before feature selection
accuracy_micro <- ok_predictions/nrow(dataset1)


# Decision Tree simple classification
library(rpart)

# grow tree 
fit <- rpart(Label ~.,
             method="class", data=dataset1)

fit_predict = predict(fit, dataset1)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree 
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

library(randomForest)
library(rpart.plot)

forest <- randomForest(Label ~., data=dataset1, na.action=na.roughfix)

pred_rf = predict(forest, dataset1)

# confusion matrix to check accuracy
conf_mx <- table(pred_rf,dataset1$Label)

#macro-averaging
# accuracy for class 'bad' = 0.9786
acc1 <- conf_mx[1,1]/(conf_mx[1,1] + conf_mx[1,2]) #bad
# accuracy for class 'good' = 0.7066
acc2 <- conf_mx[2,2]/(conf_mx[2,2] + conf_mx[2,1]) #good
# macro-averaged accuracy = 0.8426 for fitting on full dataset and before feature selection
accuracy <- (acc1 + acc2) / 2

#0.91937
prec1 <- conf_mx[1,1] / (conf_mx[1,1] + conf_mx[2,1])
#0.9063
prec2 <- conf_mx[2,2] / (conf_mx[2,2] + conf_mx[1,2])
#0.9128429
precision <- (prec1 + prec2) / 2

#micro-averaging
# number of good predictions
ok_predictions <- conf_mx[1,1] + conf_mx[2,2]
# micro_averaged accuracy = 0.917 for fitting on full dataset and before feature selection
accuracy_micro <- ok_predictions/nrow(dataset1)

plot(forest)
varImp(forest)


forest <- randomForest(Label ~., data=dataset1, na.action=na.roughfix, ntree = 100)
# this line gives Error in rpart.plot(forest) : Not an rpart object
rpart.plot(forest)

rf_predictions <- predict(forest, dataset1)
# confusion matrix to check accuracy
conf_mx <- table(pred_rf,dataset1$Label)


#ROC plot
library("plotROC")
library(caret)

#divide whole set to 2 parts 
inds <- createDataPartition(dataset1$Label, p = 0.75)
training_set <- dataset1[inds[[1]],]
test_set  <- dataset1[-inds[[1]],]


rf <- randomForest(Label ~., data=training_set, na.action=na.roughfix, ntree = 100)
pred_rf <- predict(rf, test_set)

roc_estimate <- calculate_roc(pred_rf, dataset1$Label)
single_rocplot <- ggroc(roc_estimate)
plot_journal_roc(single_rocplot)