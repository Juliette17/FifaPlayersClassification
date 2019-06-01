# read data from csv file
dat = read.csv("data1.csv", header = TRUE, sep = '#')
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

# concatenate feature matrix with labels
dataset = cbind(dat, labels_mx)
colnames(dataset)[89] <- "Label"

library(e1071)

# fit the Naive Bayes model with the whole dataset
Naive_Bayes_Model=naiveBayes(Label ~., data=dataset)
# predict on the same dataset
NB_Predictions=predict(Naive_Bayes_Model, dataset)
# confusion matrix to check accuracy
conf_mx <- table(NB_Predictions,dataset$Label)

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
accuracy_micro <- ok_predictions/nrow(dataset)

