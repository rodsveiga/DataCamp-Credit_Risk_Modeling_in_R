# Final data structure
str(training_set)
# the loan_status variable is the response variable and we have four factor variables and 
# three continuous variables as the exploratory variables.

##  What is logistic regression? A regression model with output between 0 and 1.

#### Fitting a logistic model in R

## Numeric varialbe as a predictor,
# Let us look to a example where we choose the variable age as a predictor.
log_model <- glm(loan_status ~ age, family = "binomial", data = training_set)
# Note that we have chosen a numeric variable as a predictor.
# Documentaion: glm is used to fit generalized linear models, specified by giving a symbolic 
# description of the linear predictor and a description of the error distribution.

## Categorical variable as predictor.
log_model_cat <- glm(loan_status ~ emp_cat, family = "binomial", data = training_set)
# When you include a categorical variable in a logistic regression model in R, you will 
# obtain a parameter estimate for all but one of its categories. This category for which 
# no parameter estimate is given is called the reference category. The parameter for each 
# of the other categories represents the odds ratio in favor of a loan default between the
# category of interest and the reference category.
summary(training_set$emp_cat)
# Note the reference category in "0-15" is this case.

# Acess to the coefficients of the linear regression.
log_model_cat$coefficients
# Interpretation: Compared to the reference category, the odds in favor of default change
# for "30-45" by a multiple of:
exp(log_model_cat$coefficients[[3]])  

#### Multiple variables in a logistic regression model

log_model_multi <- glm(loan_status ~ age + emp_cat + grade + loan_amnt + annual_inc, 
                       family = "binomial", data = training_set)
# Statistical significance of a certain parameter estimate: the significance of a 
# parameter is often refered to as a p-value, however in a model output you will see it 
# denoted as Pr(>|z|). In glm, mild significance is denoted by a "." to very strong 
# significance denoted by "***
summary(log_model_multi)

#### Logistic regression model: predicting the probability of default
log_model_small <- glm(loan_status ~ age + home_ownership, family = "binomial", 
                                                           data = training_set)
# Note thah the "MORTAGE" variable of home_ownership is the reference category.

# Suppose we have a test_case data frame where we store some test set cases.

# The predict function gives us the linear predictor.
# predict(log_model_small, newdata = test_case)
# Look at the predict documentation.

# The actual predict probability of default for this custumer is given 
# if we set the following argument in the predict function: type = "response"
# predictions_all_small <- predict(log_model_small, newdata = test_case, type = "response")

# We can look at the range of predictions over the test case.
# range(predicions_all_small)

## Logistic regression model using all available predictior in the data set.
log_model_full <- glm(loan_status ~. , family = "binomial", data = training_set)
# Once again the make predictions using the prediction funcion.
predictions_all_full <- predict(log_model_full, newdata = test_set, type = "response")
range(predictions_all_full)

#### Evaluating the logistic regressoon model result

# Usually the probabilities predicted by the model are not going to be like the variable
# test_set$loan_status (0 and 1). Actually, they have a value between 0 and 1.

### Cutoff or treshold value

# The cutoof value choice affects the sensitivity parameter calculated from the confusion
# matrix.

# Transforming the prediction vector to a vector of binary values indicating the status
# of the loan. Applying the ifelse() function in the context of a cut-off, you would have 
# something like ifelse(predictions > 0.3, 1, 0).

# In the first argument, you are testing whether a certain value in the predictions-vector
# is bigger than 0.3. If this is TRUE, R returns "1" (specified in the second argument), 
# if FALSE, R returns "0" (specified in the third argument), representing "default" and 
# "no default", respectively.

# For example, make a binary predictions-vector using a cut-off of 15%.
pred_cutoff_15 <- ifelse(predictions_all_full  > 0.15, 1, 0)
# Construct a confusion matrix
table(test_set$loan_status, pred_cutoff_15)

# Other way to call our logistic regression model.
log_model_full <- glm(loan_status ~. , family = binomial(link = logit), data = training_set)
# Alternative logistic regression models.
log_model_full <- glm(loan_status ~. , family = binomial(link = probit), data = training_set)
log_model_full <- glm(loan_status ~. , family = binomial(link = cloglog), data = training_set)

#### Comparing link functions for a given cut-off

# Fit the logit, probit and cloglog-link logistic regression models
log_model_logit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                       family = binomial(link = logit), data = training_set)
log_model_probit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                        family = binomial(link = probit), data = training_set)

log_model_cloglog <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                         family = binomial(link = cloglog), data = training_set)

# Make predictions for all models using the test set
predictions_logit <- predict(log_model_logit, newdata = test_set, type = "response")
predictions_probit <- predict(log_model_probit, newdata = test_set, type = "response")
predictions_cloglog <- predict(log_model_cloglog, newdata = test_set, type = "response")

# Use a cut-off of 14% to make binary predictions-vectors
cutoff <- 0.14
class_pred_logit <- ifelse(predictions_logit > cutoff, 1, 0)
class_pred_probit <- ifelse(predictions_probit > cutoff, 1, 0)
class_pred_cloglog <- ifelse(predictions_cloglog > cutoff, 1, 0)

# Make a confusion matrix for the three models
tab_class_logit <- table(true_val,class_pred_logit)
tab_class_probit <- table(true_val,class_pred_probit)
tab_class_cloglog <- table(true_val,class_pred_cloglog)

# Compute the classification accuracy for all three models
acc_logit <- sum(diag(tab_class_logit)) / nrow(test_set)
acc_probit <- sum(diag(tab_class_probit)) / nrow(test_set)
acc_cloglog <- sum(diag(tab_class_cloglog)) / nrow(test_set)
