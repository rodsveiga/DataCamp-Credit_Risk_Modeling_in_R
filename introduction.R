########## Downloading the dataset from the web.
library(curl)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://assets.datacamp.com/course/credit-risk-modeling-in-r/loan_data_ch1.rds"
download.file(fileUrl, destfile = "./data/loan_data.rds", method = "curl")
loan_data <- readRDS("./data/loan_data.rds")

##########  Looking at the data set
head(loan_data, 10)
str(loan_data)
## Loading the gmodels package in order to use the CrossTable funcion.
library(gmodels)
CrossTable(loan_data$home_ownership)
## CrossTable gives more information than table. It shows the proportions as well. 
table(loan_data$home_ownership)
## Relating home_ownership with loan_status
CrossTable(x = loan_data$home_ownership, y = loan_data$loan_status, prop.r = TRUE,
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
## Check the CrossTable documentation for more information about the options.

## Usually, default are rare.
CrossTable(loan_data$loan_status)
## The grades appear to be consistent.
CrossTable(x = loan_data$grade, y = loan_data$loan_status, prop.r = TRUE,
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

########## Histograms and outliers
hist(loan_data$int_rate, main = "Histogram of Interest Rate", xlab = "Interest rate")
## We can also make use of the ggplot2 package.
library(ggplot2)
ggplot(data = loan_data, aes(x = int_rate)) +
  geom_histogram(binwidth = 1, na.rm = TRUE)
## Annual income
hist(loan_data$annual_inc, main = "Histogram of Annual Income", xlab = "Annual Income")
## Histogram breaks
hist_income <- hist(loan_data$annual_inc, main = "Histogram of Annual Income", 
                    xlab = "Annual Income")
hist_income$breaks
# To get a better result, we should change the break ratio. One can try this rule of thumb.
n_breaks <- sqrt(nrow(loan_data))
# This results in a much longer vector of breaks.
hist_income_n <- hist(loan_data$annual_inc, breaks = n_breaks, 
                      main = "Histogram of Annual Income", xlab = "Annual Income")
# But it still does not look nice. Looking to a scattering plot, we recognize an outlier.
plot(loan_data$annual_inc, ylab = "Annual Income")
# A boxplot seems to be the best way plot. Here we antecipate and relate it with loan_status.
boxplot(annual_inc ~ loan_status, data = loan_data, 
        xlab = "loan status", ylab = "Annual Income")

### How to deal with outliers: use a rule of thumb. 

## Question: what is a reasonable rule of the thumb?
## Example 1: get rid of all annual_inc > 3000000
index_outlier_expert <- which(loan_data$annual_inc > 3000000)
loan_data_expert <- loan_data[-index_outlier_expert , ]
## Example 2: get rid of all annual_inc > Q3 + 1.5*IQR
outlier_cutoff <- quantile(loan_data$annual_inc, 0.75) + 1.5*IQR(loan_data$annual_inc,  0.75)
index_outlier_ROT <- which(loan_data$annual_inc > outlier_cutoff)
loan_data_ROT <- loan_data[-index_outlier_ROT, ]

# Boxplot after rule of thumb example 1.
boxplot(annual_inc ~ loan_status, data = loan_data_expert, 
        xlab = "loan status", ylab = "Annual Income")
# Boxplot after rule of thumb example 2.
boxplot(annual_inc ~ loan_status, data = loan_data_ROT, 
        xlab = "loan status", ylab = "Annual Income")
# Note that the rule of thumb from example 2 gives a nicer plot.

# Histogram after rule of thumb example 1.
hist(loan_data_expert$annual_inc, breaks = sqrt(nrow(loan_data_expert)),
     main = "Histogram of Annual Income - with rule of thumb 1", xlab = "Annual income")
# Boxplot after rule of thumb example 2.
hist(loan_data_ROT$annual_inc, breaks = sqrt(nrow(loan_data_ROT)),
     main = "Histogram of Annual Income - with rule of thumb 2", xlab = "Annual income")

### Bivariate plot.
plot(x = loan_data$emp_length, y = loan_data$annual_inc,
     xlab = "Employment lenght", ylab = "Annual income")
# It seems to be more interesting to get rid of the outliers.
plot(x = loan_data_ROT$emp_length, y = loan_data_ROT$annual_inc,
     xlab = "Employment lenght", ylab = "Annual income")
# Or perhaps make a boxplot.
boxplot(annual_inc ~ emp_length, data = loan_data_ROT, 
        xlab = "Employment lenght", ylab = "Annual income")

## Exercise: investigate the loan_amnt variable
hist_1 <- hist(loan_data$loan_amnt, main = "Histogram of Loan Amount", xlab = "Loan amount")
hist_1$breaks
hist_2 <- hist(loan_data$loan_amnt, breaks = sqrt(nrow(loan_data)), xlab = "Loan amount", 
                                                  main = "Histogram of Loan Amount")
boxplot(loan_amnt ~ loan_status, data = loan_data, 
        ylab = "Loan amount", xlab = "loan status")
## Exercise: investigate the age variable
boxplot(age ~ loan_status, data = loan_data, 
        ylab = "Age", xlab = "loan status")
# Probably there is wrong information, because there is a unusual old fellow in the data set, 
# but fortunatelly his loan status is 0.
max(loan_data$age)
# Plotting a scatterplot.
plot(loan_data$age, ylab = "Age")
# Get rid of the highest age.
index_highage <- which( loan_data$age >= max(loan_data$age) )
new_data <- loan_data[-index_highage, ]
# Plotting with new_data
boxplot(age ~ loan_status, data = new_data, 
        ylab = "Age", xlab = "loan status")

# Lets just observe we can have a bivariate outlier. In this case there is a person with the
# huge annual wage if $6 million that appears to be 144 years old.
plot(loan_data$age, loan_data$annual_inc, xlab = "Age", ylab = "Annual income")

### Missing data and coarse classification

summary(loan_data)
# There are NA's in two variables of the data set.
summary(loan_data$int_rate)
summary(loan_data$emp_length)
# There are three ways of deal with NA: delete row/column, replace them or keep them.
# Delete observations (rows): if there are not too many NA's (compared with the total
# number of observations)
# Delete variables (columns): if there are too many NA's (compared with the total
# number of observations)

# Delete row
index_NA <- which(is.na(loan_data$emp_length))
loan_data_no_NA <- loan_data[-c(index_NA) ,]
# Delete column
loan_data_delete_int_rate <- loan_data
loan_data_delete_int_rate$int_rate <- NULL
# Replace NA: median imputation
index_NA <- which(is.na(loan_data$emp_length))
loan_data_replace <- loan_data
loan_data_replace$emp_length[index_NA] <- median(loan_data$emp_length, na.rm = TRUE)
# Keep NA: coarse.
# Coarse classification: put variable in "bins". For example, we could chose categories 
# such as "0-15", "15-30", "30-45", "45+", "missing.
# To achieve that we create a new column in the data set, full of NA.
loan_data$emp_cat <- rep(NA, length(loan_data$emp_length))
# Then we replace these NA's with categories in the correct rows.
loan_data$emp_cat[which(loan_data$emp_length <= 15)] <- "0-15"
loan_data$emp_cat[which(loan_data$emp_length > 15 & loan_data$emp_length <= 30)] <- "15-30"
loan_data$emp_cat[which(loan_data$emp_length > 30 & loan_data$emp_length <= 45)] <- "30-45"
loan_data$emp_cat[which(loan_data$emp_length > 45)] <- "45+"
loan_data$emp_cat[which(is.na(loan_data$emp_length))] <- "Missing"
# For further analysis, it is useful to treat the emp_cat variable as a factor.
loan_data$emp_cat <- as.factor(loan_data$emp_cat)
# The basic function summary already shows us how useful.
summary(loan_data$emp_cat)
# Plot does that too.
plot(loan_data$emp_cat)

##### Data splitting and confusion matrices

### Training and test set
# Trainig set: run the model (2/3 of the data in this course)
# Test set: evaluate the result (1/3 of the data in this course )

### Evaluate a model
# Evaluate a model means compare the prediction of the model to the test set data. In our
# case we should test the model prediction for the loan status: default or not default.

### Confusion matrices
# A confusion matrix is a matrix (contingency table) where the diagonal elements represent the correct
# predictions (TN: true negatives and TP: true positives) and the non-diagonal elements the
# failed predictions (FN: false negative and FP: false positive).
# Columns: model prediction. Rows: actual loan status.
## Some measures:
# Accuracy: the percentenge of correctly classified events
# Sensitivity: the percentage of bad custumers that are classified correctly.
# Specificity: the percentage of good custumers that are classified correctly.

set.seed(567)

# To creat the training set we choose randomly 2/3 of row index.
index_train <- sample(1:nrow(loan_data), (2/3)*nrow(loan_data))
# Documentation: sample takes a sample of the specified size from the elements of x using 
# either with or without replacement --> sample(x, size, replace = FALSE, prob = NULL)                              
         
# Creating the training set.
training_set <- loan_data[index_train, ]
# Creating the test set with the rows that are not in the training test.
test_set <- loan_data[-index_train, ]

# Creating a confusion matrix.
# Suppose we have run a model and stored the predicted outcomes in a vector called model_pred.
# It is a vector of 0 and 1 and we want to compare with test_set$loan_status.

# conf_matrix <- table(test_set$loan_status, model_pred)

# Compute classification accuracy
# (conf_matrix[1]+conf_matrix[4])/(conf_matrix[1]+conf_matrix[2]+ conf_matrix[3]+conf_matrix[4])

# Compute sensitivity
# conf_matrix[4]/(conf_matrix[4]+conf_matrix[2])

