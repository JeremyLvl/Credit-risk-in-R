# Get loan_data
setwd('C:/Users/jerem/Documents/GitHub/Credit-risk-in-R')
loan_data <- read.csv('loan_data_2.csv')
attach(loan_data)
colnames(loan_data)

#get packages
library(rpart)
library('pROC')

# Set seed of 567
set.seed(567)

#We recover the pruned weighted tree from the previous work
index_train <- sample(1:nrow(loan_data), 2/3*nrow(loan_data))
training_set <- loan_data[index_train, ]
test_set <- loan_data[-index_train, ]


undersample <- rpart(loan_status ~ ., data = training_set,
                     method = 'class', control = rpart.control(cp=0.001))
weights <- ifelse(training_set$loan_status == 1, 3, 1)
tree_weight <- rpart(loan_status ~ ., data = training_set,
                     method = 'class', control = rpart.control(minsplit = 5, minbucket = 2, cp = 0.001),
                     weights = weights)

index_min <- which.min(tree_weight$cptable[,'xerror'])
cp_min <- tree_weight$cptable[index_min, 'CP']
tree_weight_prune <- prune(tree_weight, cp = cp_min)

# We get the predictions for the probability of default for the test data
pred_weight_prune <- predict(tree_weight_prune, newdata = test_set)[,2]

# We set a cutoff rate of 80%, ie. the proportion of applications that should be accepted
cutoff <- quantile(pred_weight_prune, 0.8)
cutoff
accepted <- ifelse(pred_weight_prune > cutoff, 1, 0)

# We get the actual default status of the accepted applications
accepted_status <- test_set$loan_status[accepted == 0]

# And we consider how many of the accepted applications ended in default
sum(accepted_status)/length(accepted_status)

# We make a function which does this for the different models we worked with
strategy <- function(pred_set) {
  cutoff = rep(NA, 21)
  bad_rate= rep(NA, 21)
  accept_rate = seq(1, 0, by=-0.05)
  for (i in 1:21) {
    cutoff[i] = quantile(pred_set, accept_rate[i])
    acc_i = ifelse(pred_set > cutoff[i], 1, 0)
    acc_status_i = test_set$loan_status[acc_i == 0]
    bad_rate[i] = sum(acc_status_i)/length(acc_status_i)
  }
  
  table = cbind(accept_rate, cutoff= round(cutoff, 2), bad_rate = round(bad_rate, 2))
  return(table)
}

# We use this function to see the bad rate for the un-pruned weighted predictions
pred_weight <- predict(tree_weight, newdata = test_set)[,2]
strat_weight <- strategy(pred_weight)

# And we plot the bad rate against the accept rate
plot(strat_weight[,c('accept_rate','bad_rate')], type='l')

# We get the ROC curve for the two models
ROC_weight <- roc(test_set$loan_status, pred_weight)
ROC_prune <- roc(test_set$loan_status, pred_weight_prune)

# And we plot these, we see that actually they have almost exactly the same curve
plot(ROC_weight)
lines(ROC_prune)

# We compute the area under the curve
# Noting that an area closer to 1 is best
auc(ROC_weight)


