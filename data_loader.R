# read data from csv file
dat = read.csv("data1.csv", header = TRUE, sep = '#')
dat_rows <- nrow(dat)
good_ids = as.list(dat[dat$Overall > 66,2])
bad_ids = as.list(dat[dat$Overall <= 66,2])

# create matrix with labels
labels_mx = matrix("bad", dat_rows, 1)
# players with Overall > 66 have label 'good' (8764), the rest have 'bad' (9443), split closest to 50:50
labels_mx[dat$Overall > 66,1] = "good"
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
# number of good predictions
ok_predictions <- conf_mx[1,1] + conf_mx[2,2]

accuracy = ok_predictions/nrow(dataset)

