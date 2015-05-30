############################
# Author: Daniel P. Martin
# Date: May 26 2015
# Multilevel exploratory data 
# analysis with recursive 
# partitioning
############################

# setwd to workshop folder using setwd()

set.seed(1277) # set seed for reproducibility

# Use install.packages("package name") if need be

library(lme4)
library(lmerTest)
library(randomForest)

############################
# Install mleda off GitHub

# install.packages("devtools")
library(devtools)

install_github("dpmartin42/mleda")
library(mleda)

####################################
# Regression example using 
# High School and Beyond Survey data
####################################

rm(list = ls()) # clear the working directory

# Data are from a sub-sample of the 1982 High School and Beyond Survey (Raudenbush, Bryk, 2002),
# and include information on 7,185 students nested within 160 schools: 90 public and 70 Catholic.
# Sample sizes vary from 14 to 67 students per school.
# Goal is to identify important variables that predict student math achievement scores

# Variables are:
# School: an ordered factor identifying the school that the student attends
# Minority (L1): a factor with levels No Yes indicating if the student is a member of a minority racial group.
# Sex (L1): a factor with levels Male and Female.
# SES (L1): a numeric vector of socio-economic status.
# MathAch (outcome): a numeric vector of mathematics achievement scores.
# MEANSES (L2): a numeric vector of the mean SES for the school.
# Size (L2): a numeric vector giving the number of students in the school
# Sector (L2): a factor with levels Public and Catholic
# PRACAD (L2): a numeric vector giving the percentage of students on the academic track
# DISCLIM (L2): a numeric vector measuring the disciplinary climate
# HIMINTY (L2): a factor with 1 if more than 40% minority enrolment, 0 if less than 40%

####################
# Read in data

?HSB_data
data(HSB_data)

# Create subset to make computation go a little faster

HSB_data <- HSB_data[HSB_data$School %in% sample(x = unique(HSB_data$School), size = 80, replace = FALSE), ]

head(HSB_data)

str(HSB_data)
summary(HSB_data)

############################
# Data plotting using mleda

plot_ml(the_data = HSB_data,
        var_name = c("SES", "Minority", "Sector", "Size"),
        var_level = c(1, 1, 2, 2),
        cluster = "School",
        outcome = "MathAch")

##############
# Step 1: ICC

null_mod <- lmer(MathAch ~ 1 + (1 | School), data = HSB_data)
summary(null_mod)

9.261/(9.261 + 40.424) # ICC = 18.6%

###############################
# Step 2: Estimate proportion
# variation accounted for
# and compare models using
# split-half validation

data_fold <- create_fold(HSB_data, "School", 2)

# Random forest model yields: 0.15

validate_ml(the_data = data_fold,
            formula = "MathAch ~ Minority + Sex + SES + Size + Sector +
            PRACAD + DISCLIM + HIMINTY + MEANSES",
            stat_method = "rf",
            cluster = "School")

# Multilevel model yields: 0.17

validate_ml(the_data = data_fold,
            formula = "MathAch ~ Minority + Sex + SES + Size + Sector +
            PRACAD + DISCLIM + HIMINTY + MEANSES + (1 | School)",
            stat_method = "hlm",
            cluster = "School")

########################
# Step 3: Get 
# graphical information
# (i.e., variable importance
# and partial dependence)
########################

# Plot variable importances

naive_mod <- lmer(MathAch ~ Minority + Sex + SES + Size + Sector + 
                    PRACAD + DISCLIM + HIMINTY + MEANSES + (1 | School),
                  data = data_fold[data_fold$fold == 1, ])

rf_mod <- randomForest(MathAch ~ Minority + Sex + SES + Size + Sector +
                         PRACAD + DISCLIM + HIMINTY + MEANSES,
                       data = data_fold[data_fold$fold == 1, ])

mod_list <- list(naive = naive_mod, cart_forest = rf_mod)

# Plot variable importance 

importance_ml(mod_list)

# Plot for SES and MEANSES using predicted value plots for a CART forest

plot_ml(the_data = HSB_data, the_mod = rf_mod, var_name = c("SES", "MEANSES"),
        var_level = c(1, 2), cluster = "School", outcome = "MathAch", interact = TRUE)

# Plot for SES and PRACAD and Minority (no interaction) using predicted value plots for a CART forest

plot_ml(the_data = HSB_data, the_mod = rf_mod, var_name = c("SES", "PRACAD", "Minority"),
        var_level = c(1, 2, 1), cluster = "School", outcome = "MathAch")

# Plot for SES and PRACAD and Minority (no interaction) using raw data

plot_ml(the_data = HSB_data, var_name = c("SES", "PRACAD", "Minority"),
        var_level = c(1, 2, 1), cluster = "School", outcome = "MathAch")

# tl;dr: Main-effects only model looks to be sufficient, and SES is the strongest predictor

