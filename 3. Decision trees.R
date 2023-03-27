# Get loan_data
setwd('C:/Users/jerem/Documents/GitHub/Credit-risk-in-R')
loan_data <- read.csv('loan_data_2.csv')
attach(loan_data)
colnames(loan_data)

#get packages
library(rpart)
library(rpart.plot)

# Set seed of 567
set.seed(567)

# Sample rows for training set
index_train <- sample(1:nrow(loan_data), 2/3*nrow(loan_data))

# Set training and testing sets
training_set <- loan_data[index_train, ]
test_set <- loan_data[-index_train, ]

# With only 22% of loans defaulted, we undersample the training data
# This means the decision tree will not be biased towards predicting no defaults
# (The control parameter is the minimum improvement needed at each node)
summary(loan_data$loan_status)
undersample <- rpart(loan_status ~ ., data = training_set,
                     method = 'class', control = rpart.control(cp=0.001))

# We visualise the decision tree
rpart.plot(undersample)

# This time we tell the model what the prior proportion of default was
tree_prior <- rpart(loan_status ~ ., data = training_set,
                    method = 'class', control = rpart.control(cp=0.001),
                    parms = list(prior = c(0.78, 0.22)))
rpart.plot(tree_prior)

# Including a loss matrix
# The idea is to increase the cost of classifying a default as non-default
parms = list(loss = matrix(c(0, 10, 1, 0), ncol=2))

tree_loss <- rpart(loan_status ~ ., data = training_set,
                   method = 'class', control = rpart.control(cp=0.001),
                   parms = parms)
rpart.plot(tree_loss)

# This last tree is clearly over-fitting
# We plot the cross-validated error as a function of the complexity parameter
plotcp(tree_loss)

# We look at the error in a table and find the min
printcp(tree_loss)
index_min <- which.min(tree_loss$cptable[,'xerror'])
cp_min <- tree_loss$cptable[index_min, 'CP']

# Finally we prune the tree using this cp
tree_loss_prune <- prune(tree_loss, cp = cp_min)
rpart.plot(tree_loss_prune)
# We see that this gives us a very reasonable decision tree with 64% non-defaults
# And which only considers three variables: the loan_percent_income, loan_grade and person_income

# This time we use weights to bias the tree
# Giving a higher weight to defaults in the training set
# We also include a minimum for the size of nodes and the number of points in a node to be split
weights <- ifelse(training_set$loan_status == 1, 3, 1)
tree_weight <- rpart(loan_status ~ ., data = training_set,
                     method = 'class', control = rpart.control(minsplit = 5, minbucket = 2, cp = 0.001),
                     weights = weights)
rpart.plot(tree_weight)

# Again we prune the tree with the appropriate cp
index_min <- which.min(tree_weight$cptable[,'xerror'])
cp_min <- tree_weight$cptable[index_min, 'CP']
tree_weight_prune <- prune(tree_weight, cp = cp_min)
rpart.plot(tree_weight_prune)

# We now consider all five trees we have created and consider their predictions on the test set
pred_prior <- predict(tree_prior, newdata = test_set, type = 'class')
pred_loss <- predict(tree_loss, newdata = test_set, type = 'class')
pred_loss_prune <- predict(tree_loss_prune, newdata = test_set, type = 'class')
pred_weight <- predict(tree_weight, newdata = test_set, type = 'class')
pred_weight_prune <- predict(tree_weight_prune, newdata = test_set, type = 'class')
