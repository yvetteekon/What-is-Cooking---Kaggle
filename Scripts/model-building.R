# load libraries
packages <- c("xgboost", "Matrix", "data.table") # Matrix creates sparse matrix quickly
lapply(packages, require, character.only = T)

# load data
load("dtms.RData")
load("train.RData")
load("test.RData")

# create new dataframe
matrix <- as.matrix(dtms)
df1 <- as.data.frame(matrix)
dim(df1)
head(df1)

# check if all the words are appropriate
colnames(df1) <- make.names(colnames(df1))
head(colnames(df1), 4)

# check for the dominant dependent variable
df2 <- data.frame(sort(table(train$cuisine), decreasing = T))
head(df2, 3)

# add dependent variable "cuisine" to df1
df1$cuisine <- as.factor(c(train$cuisine, rep("italian", nrow(test))))

# split the data
train1 <- df1[1:nrow(train), ]
test1 <- df1[-(1:nrow(train)), ]

# create sparse matrix from the train data (keep predictors and remove response variable)
train2 <- xgb.DMatrix(Matrix(data.matrix(train1[, !colnames(train1) %in% c("cuisine")])),
                             label = as.numeric(train1$cuisine) - 1)

# create sparse matrix from test data
test2 <- xgb.DMatrix(Matrix(data.matrix(test1[, !colnames(test1) %in% c("cuisine")])),
                     label = as.numeric(test1$cuisine) - 1)

# create watchlist
watchlist <- list(train2, test2)

# build multiclass models
# model 1
xgbmodel1 <- xgboost(data = train2, max.depth = 25, eta = 0.3, nround = 200,
                     objective = "multi:softmax", num_class = 20, verbose = 1,
                     watchlist = watchlist)

# model 2
xgbmodel2 <- xgboost(data = train2, max.depth = 20, eta = 0.2, nrounds = 250,
                     objective = "multi:softmax", num_class = 20, watchlist = watchlist)

# model 3
xgbmodel3 <- xgboost(data = train2, max.depth = 25, gamma = 2,
                     min_child_weight = 2, eta = 0.1, nround = 250,
                     objective = "multi:softmax", num_class = 20, verbose = 2,
                      watchlist = watchlist)

# make predictions
# predict 1
xgbmodel.predict1 <- predict(xgbmodel1, newdata = data.matrix(test1[, !colnames(test1) %in% c('cuisine')]))
xgbmodel.predict1.text <- levels(train1$cuisine)[xgbmodel.predict1 + 1]

# predict 2
xgbmodel.predict2 <- predict(xgbmodel2, newdata = data.matrix(test1[, !colnames(test1) %in% c('cuisine')]))
xgbmodel.predict2.text <- levels(train1$cuisine)[xgbmodel.predict2 + 1]

# predict 3
xgbmodel.predict3 <- predict(xgbmodel3, newdata = data.matrix(test1[, !colnames(test1) %in% c('cuisine')]))
xgbmodel.predict3.text <- levels(train1$cuisine)[xgbmodel.predict3 + 1]

# data frame for predict 1
submit1 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict1.text))
colnames(submit1) <- c('id','cuisine')
submit1 <- data.table(submit1, key = 'id')

# data frame for predict 2
submit2 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict2.text))
colnames(submit2) <- c('id','cuisine')
submit2 <- data.table(submit2, key = 'id')

# data frame for predict 3
submit3 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict3.text))
colnames(submit3) <- c('id','cuisine')
submit3 <- data.table(submit3, key = 'id')

# calculate metrics
sum(diag(table(test1$cuisine, xgbmodel.predict1)))/nrow(test1)
sum(diag(table(test1$cuisine, xgbmodel.predict2)))/nrow(test1)
sum(diag(table(test1$cuisine, xgbmodel.predict3)))/nrow(test1)

# ensembling
submit3$cuisine2 <- submit2$cuisine
submit3$cuisine1 <- submit1$cuisine

# function to find the maximum value row wise
Mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}

x <- Mode(submit3[,c("cuisine","cuisine2","cuisine1")])
y <- apply(submit3,1,Mode)
final_submit <- data.frame(id= submit3$id, cuisine = y)

# view submission file
data.table(final_submit)

# final submission
write.csv(final_submit, 'ensemble.csv', row.names = FALSE)
