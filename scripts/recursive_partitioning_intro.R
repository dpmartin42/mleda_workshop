############################
# Author: Daniel P. Martin
# Date: May 26 2015
# Introduction to recursive
# partitioning in R
############################

# setwd to workshop folder using setwd()

set.seed(1297) # set seed for reproducibility

# Use install.packages("package name") if need be

library(ISLR) # for College dataset
library(ggplot2) # for plotting
library(grid) # for plotting
library(rpart) # for CART
library(partykit) # for plotting a CART model
library(randomForest) # for random forests

####################################
# Regression example using 
# College data
#
# Goal: Use college characteristics
# to predict the graduation rate
####################################

rm(list = ls()) # clear the working directory

####################
# Read in data

?College
data(College)
str(College)

summary(College)

# Fix outliers:
# a college reported Grad.Rate > 100 
# a college reported percentage of faculty with PhDs > 100

College[College$Grad.Rate > 100, ]$Grad.Rate <- 100
College[College$PhD > 100, ]$PhD <- 100

summary(College)

# What does the Unversity of Virginia look like?

College["University of Virginia", ]

#####################
# Data plotting

# Plot from the slides as a reminder of what the data look like

College$Grad_Cut <- cut(College$Grad.Rate, breaks = 4, labels = c("Poor", "Moderate", "Good", "Great"))
palette <- c("#ca0020", "#f4a582", "#92c5de", "#0571b0")

ggplot(aes(x = Outstate, y = Top10perc), data = College) + 
  geom_point(aes(color = Grad_Cut), size = 4) +
  scale_colour_manual(values = palette) + 
  labs(x = "\nOut of State Tuition", y = " Percent of Students in\nTop 10% of High School\n") +
  theme_bw() + guides(color = guide_legend(title = "Graduation Rate: "))

# Remove Grad_Cut to make the next steps easier

College <- College[, names(College) != "Grad_Cut"]

######################
# Data pre-processing

# split into training and testing sets (70/30)

train_rows <- sample(x = 1:nrow(College), size = round(nrow(College) * .70), replace = FALSE)

train <- College[train_rows, ]
test <- College[-train_rows, ]

head(train)
str(train)

######################
# CART model building

?rpart

cart_train <- rpart(Grad.Rate ~ ., data = train)

plotcp(cart_train)
cart_train$cptable

cart_prune <- prune(cart_train, cp = 0.02093533)
plot(as.party(cart_prune))

######################
# CART model evaluation

# Calc mean-squared error (MSE) for pruned vs. not pruned

error_unpruned <- mean((predict(cart_train, newdata = test) - test$Grad.Rate)^2)

error_pruned <- mean((predict(cart_prune, newdata = test) - test$Grad.Rate)^2)

c(error_unpruned, error_pruned)

# convert MSE to proportion variation explained (1 - MSE/var(y))

1 - c(error_unpruned, error_pruned)/var(test$Grad.Rate)

cart_train$variable.importance

###############################
# Random forest model building

?randomForest

rf_train <- randomForest(Grad.Rate ~ ., data = train)

# Unpacking the "black box" with importance and partial dependence

varImpPlot(rf_train)

partialPlot(x = rf_train, pred.data = train, x.var = "Outstate")
partialPlot(x = rf_train, pred.data = train, x.var = "perc.alumni")

# library(edarf)

# pd <- partial_dependence(fit = rf_train, data = train, var = c("Outstate", "perc.alumni"), interaction = TRUE)
# plot_pd(pd)

###############################
# Forest model evaluation

error_rf <- mean((predict(rf_train, newdata = test) - test$Grad.Rate)^2)
error_rf

# convert MSE to proportion variation explained (1 - MSE/var(y))

1 - error_rf/var(test$Grad.Rate)

##############################
# Linear regression comparison

lm_train <- lm(Grad.Rate ~ ., data = train)
summary(lm_train)

error_lm <- mean((predict(lm_train, newdata = test) - test$Grad.Rate)^2)
1 - error_lm/var(test$Grad.Rate)

####################################################################################################################

####################################
# Classification example using 
# grad school data from UCLA stats site
#
# Goal: Use undergrad GRE, GPA
# and school rank to predict if a 
# student is admitted to grad school
####################################

rm(list = ls()) # clear the working directory

####################
# Read in data

grad_school <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

str(grad_school) # admit and rank needs to become a factor

grad_school$admit <- factor(grad_school$admit)
grad_school$rank <- factor(grad_school$rank)

str(grad_school)

#####################
# Data plotting

ggplot(aes(x = gre, y = gpa), data = grad_school) + 
  geom_point(aes(color = admit), size = 4) +
  scale_colour_manual(values = c("#D55E00", "#009E73")) + 
  labs(x = "\nGRE Score", y = "Undergraduate GPA\n") +
  theme_bw() + guides(color = guide_legend(title = "Admit to Grad School?"))

######################
# Data pre-processing

# split into training and testing sets (70/30)

train_rows <- sample(x = 1:nrow(grad_school), size = round(nrow(grad_school) * .70), replace = FALSE)

train <- grad_school[train_rows, ]
test <- grad_school[-train_rows, ]

head(train)
str(train)

######################
# CART model building

cart_train <- rpart(admit ~ ., data = train)

plotcp(cart_train)
cart_train$cptable

cart_prune_min <- prune(cart_train, cp = 0.03157895)
plot(as.party(cart_prune_min))

######################
# CART model evaluation

# Calc classification error for pruned vs. not pruned

error_unpruned <- sum(predict(cart_train, newdata = test, type = "class") == test$admit)/nrow(test)
error_pruned <- sum(predict(cart_prune_min, newdata = test, type = "class") == test$admit)/nrow(test)

c(error_unpruned, error_pruned)

###############################
# Random forest model building

rf_train <- randomForest(admit ~ ., data = train)

# Unpacking the "black box" with importance and partial dependence

varImpPlot(rf_train)

partialPlot(x = rf_train, pred.data = train, x.var = "gpa")
partialPlot(x = rf_train, pred.data = train, x.var = "gre")

###############################
# Forest model evaluation

error_rf <- sum(predict(rf_train, newdata = test, type = "class") == test$admit)/nrow(test)
error_rf

##############################
# Logistic regression comparison

glm_train <- glm(admit ~ ., family = "binomial", data = train)
summary(glm_train)

glm_pred <- predict(glm_train, newdata = test, type = "response")
glm_class <- ifelse(glm_pred > 0.5,
                    1,
                    0)

error_glm <- sum(glm_class == test$admit)/nrow(test)
error_glm

####################################################################################################################

####################################
# Classification example with 3 categories
# using iris data with missingness
#
# Goal: Use flower characteristics
# to classify the flower into its correct
# species
####################################

rm(list = ls())

######################
# Data pre-processing

data(iris)
head(iris)
str(iris)

iris$Sepal.Width[sample(x = 1:nrow(iris), size = 10, replace = FALSE)] <- NA

head(iris, n = 15)

# split into training and testing sets (70/30)

train_rows <- sample(x = 1:nrow(iris), size = round(nrow(iris) * .70), replace = FALSE)

train <- iris[train_rows, ]
test <- iris[-train_rows, ]

head(train)
str(train)

######################
# CART model building

cart_train <- rpart(Species ~ ., data = iris)

plotcp(cart_train)
plot(as.party(cart_train))

summary(cart_train) # see surrogate splits that were used

######################
# CART model evaluation

# Calc classification error for pruned vs. not pruned

error_cart <- sum(predict(cart_train, newdata = test, type = "class") == test$Species)/nrow(test)
error_cart

###############################
# Random forest model building

rf_train <- randomForest(Species ~ ., data = train) # uh-oh, random forest doesn't work on missing data

train_imp <- rfImpute(Species ~ ., data = train)

rf_train <- randomForest(Species ~ ., data = train_imp)

# Unpacking the "black box" with importance and partial dependence

varImpPlot(rf_train)

partialPlot(x = rf_train, pred.data = train, x.var = "Petal.Width")
partialPlot(x = rf_train, pred.data = train, x.var = "Petal.Length")

###############################
# Forest model evaluation

test_imp <- rfImpute(Species ~ ., data = test)

error_rf <- sum(predict(rf_train, newdata = test_imp, type = "class") == test_imp$Species)/nrow(test)
error_rf






