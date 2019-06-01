dat = read.csv("data1.csv", header = TRUE, sep = '#')
dat_rows <- nrow(dat)
good_ids = as.list(dat[dat$Overall > 66,2])
bad_ids = as.list(dat[dat$Overall <= 66,2])
labels_mx = matrix("bad", dat_rows, 1)
# players with Overall > 66 have label 'good' (8764), the rest have 'bad' (9443), split closest to 50:50
labels_mx[dat$Overall > 66,1] = "good"
labels = as.list(labels_mx)
