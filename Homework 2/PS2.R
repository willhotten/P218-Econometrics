#' This script helps to answer questions 2 and 3 of P218 - Econometrics HW2.
#' 
#' Last update: 27/10/2022
#' By: @willhotten

################################################################################
#################################### Setup #####################################
################################################################################

# Clean environment change working directory
rm(list=ls())
#setwd("C:\\Users\\gsimoesgaspar\\OneDrive - London Business School\\Teaching")
setwd("~/Library/CloudStorage/OneDrive-LondonBusinessSchool/Documents/PhD/Courses/Year 1/P218 Econometrics I/P218-Econometrics/Homework 2")

# Define a vector of packages that the script will use
pkgs_required <- c("AER", "estimatr", "tidyverse", "vtable", "fastDummies", "stargazer")
pkgs_install  <- pkgs_required[!(pkgs_required %in% installed.packages()[,"Package"])]

# If any package is not already install then install it in machine
if (length(pkgs_install) > 0) {
  install.packages(pkgs_install, dependencies=TRUE)
}

# Import necessary packages
invisible(lapply(pkgs_required, library, character.only=TRUE))
# Import necessary packages
invisible(lapply(pkgs_required, library, character.only=TRUE))

################################################################################
##################################### Q2 #######################################
################################################################################

# Importing dataset
df <- read.delim("ps1.dat", sep="")

# Generating new variables for log wage, education and experience
df$logw0 <- log(df$w0)
df$educ <- df$ed0
df$exper <- df$a0 - (df$educ + 6)

# Creating dummy variables for each of the education variables and each of the experience variables
df_dummy <- dummy_cols(df, select_columns = c("educ", "exper"))

# Dropping columns not relevant for regression
df_dummy2 <- subset(df_dummy, select = -c(w0, ed0, a0, educ, exper))

# Running regression on dummy variable set
reg_dummy2 <- lm(logw0 ~ ., data=df_dummy2)

# To produce table of results for LaTeX
stargazer(reg_dummy2, title="Results", align=TRUE)

################################################################################
##################################### Q3 #######################################
################################################################################

# Specifying true parameter values based on true model: y_i = 1 + 0.5z_i + e_i
alpha = 1
beta = 0.5 

# Setting seed, sample size (n) and no. iterations (M)
set.seed(1)
n = 100
M = 1000

# Setting up stores of parameter estimates
intercept <- rep(0,M)
slope <- rep(0,M)
sigmasq_hat <- rep(0,M)

# Monte Carlo simulation
for (i in 1:M){
  
  # Generate data from specification provided in question
  u_i = runif(n, -1, 1)
  z_i = rchisq(n, 3)
  y_i = alpha + beta*z_i + u_i

  # Generate data frame of generated data
  data_i = data.frame(y = y_i, z = z_i)
  
  # Run OLS regressions of y on x
  ols_i <- lm(y ~ z, data = data_i)
  
  # Save estimates from OLS regression
  intercept[i] <- ols_i$coefficients[1]
  slope[i] <- ols_i$coefficients[2]
  sigmasq_hat[i] <- (sum(resid(ols_i)^2))/(n-2)
  
}

# Store summary statistics of estimates across total M iterations
estimates <- data.frame(intercept, slope, sigmasq_hat)

# Produce table of summary statistics to input into LaTeX
stargazer(estimates, title = "Monte Carlo simulation results, N = 100, M = 1,000", align =TRUE)