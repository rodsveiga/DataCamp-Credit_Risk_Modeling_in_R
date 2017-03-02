#### Computing the gain for a tree

# In the lesson, you looked at how the Gini-measure is used to create the perfect split
# for a tree. Now, you will compute the gain for the tree called small_tree loaded in your
# workspace.

# The data set contains 500 cases, 89 of these cases are defaults. This led to a Gini 
# of 0.292632 in the root node. As a small reminder, remember that Gini of a certain 
# node = 2 * proportion of defaults in this node * proportion of non-defaults in this node. 

gini_root <- 2 * (89 / 500) * (411 / 500)

# You will use these Gini measures to help you calculate the gain of the leaf nodes 
# with respect to the root node. Look at the following code to get an idea of how you 
# can use the gini measures you created to calculate the gain of a node.

# Gain = gini_root - (prop(cases left leaf) * gini_left) - (prop(cases right leaf * gini_right))

# Compute the gini in the left hand and the right hand node, and the gain of the two 
#leaf nodes with respect to the root node. The object containing the tree is small_tree.

# The Gini-measure of the root node is given below
gini_root <- 2 * (89 / 500) * (411 / 500)
# Compute the Gini measure for the left leaf node
gini_ll <- 2 * (401/446) * (45/446)
# Compute the Gini measure for the right leaf node
gini_rl <- 2 * (10/54) * (44/54)
# Compute the gain
gain <- gini_root - 446 / 500 * gini_ll - 54 / 500 * gini_rl
# Compare the gain-column in small_tree$splits with our computed gain, multiplied by 500, and assure they are the same
small_tree$splits
improve <- gain * 500

#### Building decision trees using the rpart package
# - hard building nice decision tree for credit risk data
# - main reason: unbalanced data
library(rpart)
fit_default <- rpart(loan_status ~., method = "class", data = training_set, 
                     control = rpart.control(cp = 0.001) )
# Take a look at the rpart documentation. Here we relax the complexity parameter to 0.001

# Plot the decision tree
plot(fit_default, uniform = TRUE)
# Add labels to the decision tree
text(fit_default)

### Three techniques to overcome unbalance
## Undersampling or oversampling
   # Accuracy issue will disappear
   # Only training set
## Changing the prior probabilities
## Including a loss matrix
# --> Validade model to see what is best


### Changing the prior probabilities

# You can also change the prior probabilities to obtain a decision tree. This is an
# indirect way of adjusting the importance of misclassifications for each class. You 
# can specify another argument inside rpart() to include prior probabities. The argument 
# you are looking for has the following form
# parms = list(prior=c(non_default_proportion, default_proportion))

# Change the code below such that a tree is constructed with adjusted prior probabilities.
tree_prior <- rpart(loan_status ~ ., method = "class", data = training_set, 
                    parms=list(prior=c(non_default_proportion = 0.7, default_proportion = 0.3)),
                    control = rpart.control(cp = 0.001))

# Plot the decision tree
plot(tree_prior, uniform = TRUE)

# Add labels to the decision tree
text(tree_prior)

### Including a loss matrix

# Thirdly, you can include a loss matrix, changing the relative importance of misclassifying
# a default as non-default versus a non-default as a default. You want to stress that
# misclassifying a default as a non-default should be penalized more heavily. Including a 
# loss matrix can again be done in the argument parms in the loss matrix.

# parms = list(loss = matrix(c(0, cost_def_as_nondef, cost_nondef_as_def, 0), ncol=2))

# Doing this, you are constructing a 2x2-matrix with zeroes on the diagonal and changed 
# loss penalties off-diagonal. The default loss matrix is all ones off-diagonal.

# Change the code below such that a decision tree is constructed using a loss matrix penalizing 10 times more heavily for misclassified defaults.
tree_loss_matrix <- rpart(loan_status ~ ., method = "class", data =  training_set, 
                          parms = list(loss = 
                                         matrix(c(0, cost_def_as_nondef = 10, 
                                                  cost_nondef_as_def = 1, 0), ncol=2)), 
                          control = rpart.control(cp = 0.001))


# Plot the decision tree
plot(tree_loss_matrix, uniform = TRUE)

# Add labels to the decision tree
text(tree_loss_matrix)

#### Pruning the decision tree

## Problems with large decision trees
# - Too complex: not clear anymore.
# - Overfitting when applying to test set.
# - Solution: use the functions printcp(), plotcp() for pruning purposes.
printcp(tree_prior)
# We can plot the variable CP versus xerror, in order to find what value of CP minimizes the
# cross validated error. 
plotcp(tree_prior)
# Create an index for of the row with the minimum xerror.
index <- which.min(tree_prior$cptable[ , "xerror"])
# Create tree_min by selecting the index of tree_prior$cptable within the column "CP".
tree_min <- tree_prior$cptable[index, "CP"]

## Plot the pruned tree
ptree_prior <- prune(tree_prior, cp = tree_min)
plot(ptree_prior, uniform = TRUE)
text(ptree_prior)
# Plotting the pruned tree we get a smaller decision tree. Note that this does not give the
# information about the true number of defaults against non defaults in each leaf. We can get this if
# taht if we include the argument use.n in the text function.
text(ptree_prior, use.n = TRUE)

## More intuitive plot: prp() in the rpart.plot-package
library(rpart.plot)
prp(ptree_prior)
# Number of default and non default cases.
prp(ptree_prior, extra = 1)

## Pruning the tree with the loss matrix

# Now, we will prune the tree that was built using a loss matrix in order to penalize 
# misclassified defaults more than misclassified non-defaults.

# Setting a seed and run the code to construct the tree with the loss matrix again.
set.seed(345)
tree_loss_matrix  <- rpart(loan_status ~ ., method = "class", data = training_set,
                           parms = list(loss=matrix(c(0, 10, 1, 0), ncol = 2)),
                           control = rpart.control(cp = 0.001))
# Plot the cross-validated error rate as a function of the complexity parameter and the size
# of the tree.
plotcp(tree_loss_matrix)
# Create an index for of the row with the minimum xerror.
index <- which.min(tree_loss_matrix$cptable[ , "xerror"])
# Create tree_min by selecting the index of tree_prior$cptable within the column "CP".
tree_min <- tree_loss_matrix$cptable[index, "CP"]
# Looking at the cp-plot, you will notice that pruning the tree using the minimum 
# cross-validated error will lead to a tree that is as big as the unpruned tree, as the 
# cross-validated error reaches its minimum for cp = 0.001. Because you would like to make 
# the tree somewhat smaller, try pruning the tree using cp = 0.0012788. For this complexity
#parameter, the cross-validated error approaches the minimum observed error.
ptree_loss_matrix <- prune(tree_loss_matrix, cp = tree_min)
prp(ptree_loss_matrix)
# Prune the tree using cp = 0.0012788
ptree_loss_matrix <- prune(tree_loss_matrix, cp = 0.0012788)
prp(ptree_loss_matrix, extra = 1)
# Note in the plotcp that as the cp value increases, the size of tree decreases.

#### Other tree options and confusion matrices

### Other interesting rpart()-arguments
## In rpart:
# - weights: include case weights.
## In the control argument of rpart (rpart.control):
# - minsplit: minimum number of observation for split attempt,
# - minbucket: minimum number of observations in leaf node.

## Making predictions using the decision tree
pred_tree_loss_matrix_class <- predict(ptree_loss_matrix, newdata = test_set, type = "class")
# When we set type="class" we get a vector straight away, without having to chose a cutoff
# value anymore as opost what we did with logistic regression.
# I we want a non-binary result and we prefer to choose the cutoff value ourselves:
pred_tree_loss_matrix <- predict(ptree_loss_matrix, newdata = test_set)
head(pred_tree_loss_matrix, 100)
## Constructing a confusion matrix
table(test_set$loan_status, pred_tree_loss_matrix_class)
# Note that many defaults were predict. Probably it would be better to work with probablities
# and change the cutoff.

## Exercises changing some arguments.

# Now we will use some final arguments that were have discussed. Some specifications in the
# rpart.control()-function will be changed, and some weigths will be included using the 
# weights argument in rpart(). The vector case_weights is constructed below. 
case_weights <- training_set$loan_status
default_rows <- which(training_set$loan_status == 1)
nondefault_rows <-  which(training_set$loan_status != 1)
case_weights[default_rows] <- case_weights[default_rows] + 3
case_weights[nondefault_rows] <- case_weights[nondefault_rows] + 1
# This vector contains weights of 1 for the non-defaults in the training set, and weights
# of 4 for defaults in the training sets. By specifying higher weights for default, the 
# model will assign higher importance to classifying defaults correctly.

# We will also change the minimum number of splits that are allowed in a node to 5, and the
# minimum number of observations allowed in leaf nodes to 2 by using the arguments minsplit
# and minbucket in rpart.control respectively.
set.seed(345)
tree_weights <- rpart(loan_status ~ ., method = "class",
                      data = training_set,
                      control = rpart.control(minsplit = 5, minbucket = 2, cp = 0.001),
                      weights = case_weights)
# Plotting the cross-validated error rate for a changing cp.
plotcp(tree_weights)
# Creating an index for of the row with the minimum xerror.
index <- which.min(tree_weights$cptable[ , "xerror"])
# Creating tree_min.
tree_min <- tree_weights$cp[index, "CP"]
# Pruning the tree using tree_min.
ptree_weights <- prune(tree_weights, tree_min)
# Plotting the pruned tree using the rpart.plot()-package.
prp(ptree_weights)

#### Confusion matrices and accuracy of our final trees

# Make predictions for each of the pruned trees using the test set.
pred_prior <- predict(ptree_prior, newdata = test_set,  type = "class")
pred_loss_matrix <- predict(ptree_loss_matrix, newdata = test_set,  type = "class")
pred_weights <- predict(ptree_weights, newdata = test_set,  type = "class")
# construct confusion matrices using the predictions.
confmat_prior <- table(test_set$loan_status, pred_prior)
confmat_loss_matrix <- table(test_set$loan_status, pred_loss_matrix)
confmat_weights <- table(test_set$loan_status, pred_weights)
# Compute the accuracies
acc_prior <- sum(diag(confmat_prior)) / nrow(test_set)
acc_loss_matrix <- sum(diag(confmat_loss_matrix)) / nrow(test_set)
acc_weights <- sum(diag(confmat_weights)) / nrow(test_set)

# Note that each tree has a different accuracy and each tree has a different number 
# of splits.
