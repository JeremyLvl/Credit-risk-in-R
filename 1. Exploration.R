# Get loan_data and view its structure
setwd('C:/Users/jerem/Documents/GitHub/Credit-risk-in-R')
loan_data <- read.csv('loan_data.csv')
attach(loan_data)
str(loan_data)

# Load the packages
library(gmodels)
library(ggplot2)
library(tibble)

#--

# Check proportion of all loans default, 1 indicates default
# We see on average 21.8% of loans default
CrossTable(loan_status)

# Check default rate by credit grade
# As expected, the default rate is negatively correlated to the grade
CrossTable(loan_grade, loan_status,
           prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE)

# Get a histogram of the amounts loaned
hist_2 <- hist(loan_amnt, xlab = "Loan amount",
               main = "Histogram of the loan amount")

#--

# Look at the age and income variables together
plot(person_age, person_income, xlab = "Age", ylab = "Income")

# Remove the clear outliers of age over 100
index_highage <- which(person_age > 100)
new_data <- loan_data[-index_highage, ]

#--

# Look at the interest rates
summary(loan_int_rate)
dim(loan_data)

# We note 3116 null interest rates from 32581 data entries
# We can replace these with the median interest rate
median_int <- median(loan_int_rate, na.rm=TRUE)
na_index <- which(is.na(loan_int_rate))
loan_data_replaced <- loan_data
loan_data_replaced$loan_int_rate[na_index] <- median_int

# Instead we categorise and keep an 'NA' category
loan_data$int_cat <- cut(loan_int_rate, breaks=c(5,7,9,11,13,15,17, Inf))
levels(loan_data$int_cat) <- c(levels(loan_data$int_cat), 'missing')
loan_data$int_cat[na_index] <- 'missing'
ggplot(as_tibble(loan_data$int_cat), mapping = aes(x=value)) +
  geom_bar() + labs(x='interest rate')

write.csv(loan_data, 'loan_data_2.csv')
