# Get loan_data
setwd('C:/Users/jerem/Documents/GitHub/Credit-risk-in-R')
loan_data <- read.csv('loan_data_2.csv')
attach(loan_data)
loan_data

#get packages
library(tidyverse)
library(caret)

# Set seed of 567
set.seed(567)

# Sample rows for training set
index_train <- sample(1:nrow(loan_data), 2/3*nrow(loan_data))

# Set training and testing sets
training_set <- loan_data[index_train, ]
test_set <- loan_data[-index_train, ]

# Build a log regression model for default given the interest rate
model <- glm(formula = loan_status ~ int_cat, family = 'binomial',
             data = training_set)

# Check coefficients of model
coefs <- rownames_to_column(data.frame(model$coefficients)) %>% 
  mutate(rowname = str_remove(rowname, 'int_cat')) %>%
  arrange(length(rowname)) %>%
  arrange(c(1,5,6,7,2,3,4,8))
coefs$rowname <- factor(coefs$rowname, levels=coefs$rowname)
ggplot(coefs, aes(x=rowname,y=model.coefficients))+geom_bar(stat='identity')

#--

# Build a log regression model for default given the many variables
multi_model <- glm(formula = loan_status ~ person_age + person_income + loan_grade + loan_amnt,
             family = 'binomial', data = training_set)

# With this model we clearly see that the age of the person and the grade and amount of the
# loan are significant variables for the loan status
summary(multi_model)

# Finally we predict on the test data using this multi-variable model
pred = predict(multi_model, newdata = test_set, type = 'response')
# The predictions are distributed from 0 to 1 in an acceptable way
summary(pred)
# We round the predictions to 0 and 1, setting the threshold to the median prediction
# We look at the confusion matrix
cm <- table(ifelse(pred > median(pred),1,0), test_set$loan_status)
cm
print(confusionMatrix(cm,positive = '1')$byClass[c('Sensitivity','Specificity','Balanced Accuracy')])
# We see a decent specificity of 0.86, ie. predicting no default when there isn't one
# A high specificity of 0.60, ie. predicting a default when there is one
# A decent accuracy of 0.74, ie. the average of sensitivity and specificity


