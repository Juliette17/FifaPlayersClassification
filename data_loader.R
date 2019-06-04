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

convert_weight_values <- function(amount) {
  numeric = as.numeric(substr(amount,1,nchar(amount)-3))
  return (numeric)
}


# convert wage and value column
wages = as.character(dat$Wage)
values = as.character(dat$Value)
release_clause = as.character(dat$Release.Clause)
wages_converted <- matrix((sapply(wages, convert_to_numeric_value)), nrow(dat), 1)
values_converted <- matrix((sapply(values, convert_to_numeric_value)), nrow(dat), 1)
release_clauses_converted <- matrix((sapply(release_clause, convert_to_numeric_value)), nrow(dat), 1)

dat$Wage <- wages_converted
dat$Value <- values_converted
dat$Release.Clause <- release_clauses_converted

#changed this to be numeric values instead of list which cannot be used in training model
# dataset$Wage <- do.call(rbind, wages_converted)
# dataset$Value <- do.call(rbind, values_converted)

#delete lbs from weight
weights = as.character(dat$Weight)
weights_converted <- matrix((sapply(weights, convert_weight_values)), nrow(dat), 1)
dat$Weight <- weights_converted

#corelation between numerical features
# mx = cbind(dat$Overall, dat$Age, dat$Potential, dat$Value, dat$Wage, dat$Reactions, dat$BallControl, dat$Special)
mx = cbind(dat$Overall, dat$Age, dat$Potential, dat$Value, dat$Wage, dat$Reactions, dat$BallControl, dat$Special, dat[, 55:88])
correlation_mx = cor(mx)
mx_to_xls <- data.frame(correlation_mx)
library(xlsx)
write.xlsx(mx_to_xls, "D:\\features_correlation.xlsx")


dat_statistics = dat

#Remove all factor type values that have more than 53 categories
dat_statistics[, c("ID", "Name", "Photo", "LS", "Flag", "Club.Logo", "Joined", "Real.Face", "Loaned.From", "Contract.Valid.Until", "Jersey.Number",
        "ST", "RS", "LW", "LF", "CF", "RF", "RW", "LAM", "CAM", "RAM", "LM", "LCM", "CM", "RCM", "RM", "LWB", "LDM", "CDM", "RDM"
                 , "RWB", "LB", "LCB", "CB", "RCB", "RB", "Height", "Position")] = NULL

dat_numerical_statistics = dat_statistics
dat_numerical_statistics[, c("Nationality", "Club", "Work.Rate", "Body.Type", "Value", "Wage", "Release.Clause", "Weight", "Preferred.Foot", "Weak.Foot")] = NULL


dat[, 1] = NULL

summaries <- list()
for (i in 1:ncol(dat_numerical_statistics)) {
  print(names(dat_numerical_statistics)[i])
  summaries[[names(dat_numerical_statistics)[i]]] <- summary(dat_numerical_statistics[, i])
}

summary_mx = rbind(summaries$Age, summaries$Potential, summaries$Special, summaries$International.Reputation, summaries$Skill.Moves, summaries$Crossing
      , summaries$Finishing, summaries$HeadingAccuracy, summaries$ShortPassing, summaries$Volleys,
      summaries$Dribbling, summaries$Curve , summaries$FKAccuracy , summaries$LongPassing , summaries$BallControl 
      , summaries$Acceleration, summaries$SprintSpeed, summaries$Agility , summaries$Reactions ,
      summaries$Balance , summaries$ShotPower , summaries$Jumping , summaries$Stamina , summaries$Strength , summaries$LongShots 
      , summaries$Aggression , summaries$Interceptions, summaries$Positioning , summaries$Vision , summaries$Penalties,
      summaries$Composure, summaries$Marking, summaries$StandingTackle , summaries$SlidingTackle , summaries$GKDiving , 
      summaries$GKHandling , summaries$GKKicking, summaries$GKPositioning , summaries$GKReflexes)

write.xlsx(summary_mx, "D:\\summaries.xlsx")

summaries <- list()
for (i in 1:ncol(dat_statistics)) {
  print(names(dat_statistics)[i])
  summaries[[names(dat_statistics)[i]]] <- summary(dat_statistics[, i])
}

# remove feature 'Overall'
dat <- dat[,-8]

# concatenate feature matrix with labels
dataset = cbind(dat, labels_mx)
colnames(dataset)[89] <- "Label"

#copy of dataset
dataset_copy <- dataset

#proposed columns to delete
dataset_copy[, c("ID", "Name", "Photo", "Flag", "Club.Logo", "Joined", "Nationality",
             "Club", "Real.Face", "Loaned.From", "Contract.Valid.Until", "Jersey.Number", 
             "Special", "Release.Clause", "Value", "Wage", "Reactions", "Potential")] = NULL


#Remove all factor type values that have more than 53 categories
dataset_copy[, c("LS", "ST", "RS", "LW", "LF", "CF", "RF", "RW", "LAM", "CAM", "RAM", "LM", "LCM", "CM", "RCM", "RM", "LWB", "LDM", "CDM", "RDM"
             , "RWB", "LB", "LCB", "CB", "RCB", "RB", "Height")] = NULL

set.seed(17)

#divide whole set to 2 parts 
inds <- createDataPartition(dataset_copy$Label, p = 0.5)
dataset1 <- dataset_copy[inds[[1]],]
validation_set  <- dataset_copy[-inds[[1]],]

library(plotROC)
# Naive Bayes Classification
library(e1071)
library(caret)

# fit the Naive Bayes model with the whole dataset
Naive_Bayes_Model= train(Label ~ ., 
                         data = dataset1,
                         method = 'nb',
                         trControl = trainControl(method='cv',number=10),
                         tuneLength = 15)
# predict on the validation dataset
NB_Predictions=predict(Naive_Bayes_Model, validation_set)
# confusion matrix to check accuracy
conf_mx <- table(NB_Predictions,validation_set$Label)

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
accuracy_micro <- ok_predictions/nrow(validation_set)
output = data.frame(accuracy, accuracy_micro, precision)

roc_estimate <- calculate_roc(NB_Predictions, validation_set$Label)
single_rocplot <- ggroc(roc_estimate)
plot_journal_roc(single_rocplot)
auc_value <- calc_auc(single_rocplot)[1, "AUC"]

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

######################### badania drzew
library(rpart)
output <- NULL
for (param in 2:10){
  fit <- rpart(Label ~., data=dataset1,
               control=rpart.control(maxdepth = 3))
  
  pred_tree = predict(fit, dataset1, type="class")
  
  conf_mx <- table(pred_tree,dataset1$Label)
  acc1 <- conf_mx[1,1]/(conf_mx[1,1] + conf_mx[1,2]) #bad
  acc2 <- conf_mx[2,2]/(conf_mx[2,2] + conf_mx[2,1]) #good
  accuracy <- (acc1 + acc2) / 2
  prec1 <- conf_mx[1,1] / (conf_mx[1,1] + conf_mx[2,1])
  prec2 <- conf_mx[2,2] / (conf_mx[2,2] + conf_mx[1,2])
  precision <- (prec1 + prec2) / 2
  ok_predictions <- conf_mx[1,1] + conf_mx[2,2]
  accuracy_micro <- ok_predictions/nrow(dataset1)
  
  roc_estimate <- calculate_roc(pred_tree, dataset1$Label)
  single_rocplot <- ggroc(roc_estimate)
  #plot_journal_roc(single_rocplot)
  auc_value <- calc_auc(single_rocplot)[1, "AUC"]
  output = rbind(output, data.frame(accuracy, accuracy_micro, precision))
}

caret.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3)


# Use caret to train the rpart decision tree using 10-fold cross 
# validation repeated 3 times and use 15 values for tuning the
# cp parameter for rpart. This code returns the best model trained
# on all the data! Mighty!
rpart.cv <- train(Label ~ ., 
                  data = dataset1,
                  method = "rpart",
                  trControl = caret.control,
                  tuneLength = 15, maxdepth=5, minsplit = 25, minbucket = 5, cp = 0.01)

pred_tree = predict(fit, validation_set, type="class")

conf_mx <- table(pred_tree,validation_set$Label)
acc1 <- conf_mx[1,1]/(conf_mx[1,1] + conf_mx[1,2]) #bad
acc2 <- conf_mx[2,2]/(conf_mx[2,2] + conf_mx[2,1]) #good
accuracy <- (acc1 + acc2) / 2
prec1 <- conf_mx[1,1] / (conf_mx[1,1] + conf_mx[2,1])
prec2 <- conf_mx[2,2] / (conf_mx[2,2] + conf_mx[1,2])
precision <- (prec1 + prec2) / 2
ok_predictions <- conf_mx[1,1] + conf_mx[2,2]
accuracy_micro <- ok_predictions/nrow(validation_set)

output = data.frame(accuracy, accuracy_micro, precision)

#random forest tests

library(randomForest)
library(mlbench)
library(caret)

tuneRF(dataset = dataset1, mtryStart = , ntreeTry=50, stepFactor=2, improve=0.05,
       trace=TRUE, plot=TRUE, doBest=FALSE, ...)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(17)
mtry <- sqrt(ncol(dataset1)-1)
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Label~., data=dataset1, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree = 500)
print(rf_default)

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(dataset1)-1)))
modellist <- list()
set.seed(17)
for (ntree in c(50, 100, 200, 500, 1000, 1500, 2000, 2500)) {
  fit <- train(Label~., data=dataset1, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  print(ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)

modellist_maxnodes <- list()
for (max_nodes in c(5, 7, 9, 11, 13, 15, 17, 18, 21, 23, 25)) {
  for (minsplit in c(10, 20, 20, 30, 40, 50, 60)) {
  fit <- train(Label~., data=dataset1, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=500, maxnodes = max_nodes, minsplit = minsplit)
  print(ntree)
  key <- toString(max_nodes)
  modellist_maxnodes[[max_nodes]] <- fit
  }
}
rf_predict = predict(fit, dataset1)

conf_mx <- table(rf_predict,dataset1$Label)
acc1 <- conf_mx[1,1]/(conf_mx[1,1] + conf_mx[1,2]) #bad
acc2 <- conf_mx[2,2]/(conf_mx[2,2] + conf_mx[2,1]) #good
accuracy <- (acc1 + acc2) / 2
prec1 <- conf_mx[1,1] / (conf_mx[1,1] + conf_mx[2,1])
prec2 <- conf_mx[2,2] / (conf_mx[2,2] + conf_mx[1,2])
precision <- (prec1 + prec2) / 2
ok_predictions <- conf_mx[1,1] + conf_mx[2,2]
accuracy_micro <- ok_predictions/nrow(dataset1)

rf_raining_output = data.frame(accuracy, accuracy_micro, precision)

rf_predict = predict(fit, validation_set)

conf_mx <- table(rf_predict,validation_set$Label)
acc1 <- conf_mx[1,1]/(conf_mx[1,1] + conf_mx[1,2]) #bad
acc2 <- conf_mx[2,2]/(conf_mx[2,2] + conf_mx[2,1]) #good
accuracy <- (acc1 + acc2) / 2
prec1 <- conf_mx[1,1] / (conf_mx[1,1] + conf_mx[2,1])
prec2 <- conf_mx[2,2] / (conf_mx[2,2] + conf_mx[1,2])
precision <- (prec1 + prec2) / 2
ok_predictions <- conf_mx[1,1] + conf_mx[2,2]
accuracy_micro <- ok_predictions/nrow(validation_set)

rf_output = data.frame(accuracy, accuracy_micro, precision)


