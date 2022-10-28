#' This script helps to answer questions 2 and 3 of P218 - Econometrics HW2.
#' 
#' Last update: 27/10/2022
#' By: @willhotten

################################################################################
#################################### Setup #####################################
################################################################################

# Clean environment
rm(list=ls())

# Define a vector of packages that the script will use
pkgs_required <- c("AER", "estimatr", "tidyverse", "vtable", "fastDummies", "stargazer", "ggplot2")
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

# Import dataset
df <- read.delim("Homework 2/ps1.dat", sep="")

# Generate new variables for log wage, education and experience
df$logw0 <- log(df$w0)
df$educ <- df$ed0
df$exper <- df$a0 - (df$educ + 6)

# Create dummy variables for each of the education variables and each of the experience variables
df_dummy <- dummy_cols(df, select_columns = c("educ", "exper"))

# Drop columns not relevant for regression
df_dummy2 <- subset(df_dummy, select = -c(w0, ed0, a0, educ, exper))

# Run regression on dummy variable set
reg_dummy2 <- lm(logw0 ~ ., data=df_dummy2)

# Produce table of results for LaTeX
stargazer(reg_dummy2, title="Results", align=TRUE)

################################################################################
##################################### Q3 #######################################
################################################################################

# Specify true parameter values based on true model: y_i = 1 + 0.5z_i + e_i
alpha = 1
beta = 0.5 

# Set seed, sample size (n) and no. iterations (M)
set.seed(1)
n = 100
M = 1000

# Setup stores of parameter estimates
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
stargazer(estimates, title = paste("Monte Carlo simulation results, n = ", n, ", M = ", M, sep=""), align =TRUE, out=paste("Homework 2/MC_results_n", n, "_M", M,".tex", sep = ""))

# Produce and save histograms of regression coefficients

# Intercept histogram
hist1 <- ggplot(estimates, aes(x=intercept)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
ggsave(paste("Homework 2/hist_intercept_n",n,"_M",M,".png", sep=""))

# Slope histogram
hist2 <- ggplot(estimates, aes(x=slope)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
ggsave(paste("Homework 2/hist_slope_n",n,"_M",M,".png", sep=""))

# Compare empirial cdf of slope estimate to normal cdf

# Asymptotic variance for normal cdf
avar = (1/3)/(6*n)

# Plotting empirical cdf and normal cdf with mean = 1 and sd = asymptotic variance defined above 
ecdf <- ggplot(estimates, aes(x=slope)) +
  stat_ecdf(geom = "step") +
  stat_function(fun=pnorm,color="blue", args = list(mean = beta, sd = sqrt(avar))) +
  labs(title="ECDF and theoretical CDF")
ggsave(paste("Homework 2/ecdf_slope_n",n,"_M",M,".png", sep=""))



