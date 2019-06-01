# read data from csv file
dat = read.csv("D:\\Studia\\MGR\\SEM_2\\MOW\\projekt\\repo\\FifaPlayersClassification\\data1.csv", header = TRUE, sep = '#')
# remove rows with empty features
dat_without_empty <- na.omit(dat)
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

# Naive Bayes Classification
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


# Decision Tree simple classification
library(rpart)

# grow tree 
fit <- rpart(Label ~.,
             method="class", data=dataset)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "D:\\Studia\\MGR\\SEM_2\\MOW\\projekt\\tree.ps", 
     title = "Classification Tree")

# prune the tree 
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Kyphosis")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree.ps", 
     title = "Pruned Classification Tree for Kyphosis")

#corelation between numerical features
mx = cbind(dat$Age, dat$Potential, dat$Height, dat$Overall)
correlation_mx = cor(mx)


# convertion of factor feature to numeric value
convert_to_numeric_value <- function(amount) {
  if (length(amount) > 3) {
    without_euro_sign = tail(amount, -3)
    if (tail(without_euro_sign, 1) == "K")
      numeric = as.numeric(paste(head(without_euro_sign, -1), collapse = ""))*1000
    else if (tail(without_euro_sign, 1) == "M")
      numeric = as.numeric(paste(head(without_euro_sign, -1), collapse = ""))*1000000
    else
      numeric = as.numeric(without_euro_sign)
  }
  else
    numeric = as.numeric(paste(amount, collapse = ""))
    return (numeric)
}

# convert wage and value column
wages = strsplit(as.character(dat$Wage), "")
values = strsplit(as.character(dat$Value), "")
wages_converted <- matrix((sapply(wages, convert_to_numeric_value)), nrow(dat), 1)
values_converted <- matrix((sapply(values, convert_to_numeric_value)), nrow(dat), 1)
dat$Wage <- wages_converted
dat$Value <- values_converted


