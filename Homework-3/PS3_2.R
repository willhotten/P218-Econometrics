#' This script helps to answer question 2 of P218 - Econometrics HW3.
#' 
#' Last update: 20/11/2022
#' By: @willhotten

################################################################################
#################################### Setup #####################################
################################################################################

# Clean environment
rm(list=ls())

# Define a vector of packages that the script will use
pkgs_required <- c("AER", "estimatr", "tidyverse", "vtable", "stargazer", "ggplot2", "readxl", "dynlm", "tsbox")
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

##################################### Q2A ######################################

# Import dataset
Nerlove1963 <- read_excel("Homework-3/Nerlove1963.xlsx")

# Generate log variables
Nerlove1963$logTC = log(Nerlove1963$Cost)
Nerlove1963$logQ = log(Nerlove1963$output)
Nerlove1963$logQsq = Nerlove1963$logQ^2
Nerlove1963$logPL = log(Nerlove1963$Plabor)
Nerlove1963$logPK = log(Nerlove1963$Pcapital)
Nerlove1963$logPF = log(Nerlove1963$Pfuel)

# Run regression of logQ, logPL, lokPK, logPF on logTC
reg1 <- lm(logTC ~ logQ + logPL + logPK + logPF, data=Nerlove1963)

# Run regression of logQ, logQsq, logPL, lokPK, logPF on logTC
reg2 <- lm(logTC ~ logQ + logQsq + logPL + logPK + logPF, data=Nerlove1963)

# Produce table of results for LaTeX
stargazer(reg1, single.row = TRUE,
          title = "Results", 
          align =TRUE, out="Homework-3/Results/reg1_results.tex")

stargazer(reg2, single.row = TRUE,
          title = "Results", 
          align =TRUE, out="Homework-3/Results/reg2_results.tex")

# Hypothesis test that the coefficient on logQsq is zero
summary(reg2)

##################################### Q2B ######################################

# Suitable range of values for beta7
quantile(Nerlove1963$logQ, probs = seq(0, 1, by = .05))
# Suggests range of 4 - 8.5 would be appropriate

##################################### Q2C ######################################

# Pick M values of beta7 within the range (4, 8.5)
M = 1000
beta7 <- seq(4, 8.5, length.out = M)

# Initiate blank RSS vector
RSS <- rep(0,M)

# Creating new variables to allow us to impose the restriction: beta3 + beta4 + beta5 = 1
Nerlove1963$logTC_sub_logPF = Nerlove1963$logTC - Nerlove1963$logPF
Nerlove1963$logPL_sub_logPF = Nerlove1963$logPL - Nerlove1963$logPF
Nerlove1963$logPK_sub_logPF = Nerlove1963$logPK - Nerlove1963$logPF

# Start the loop
for (i in 1:M){
  # Calculate corresponding z for each beta
  Nerlove1963$z = Nerlove1963$logQ*(1+exp(beta7[i] - Nerlove1963$logQ))^-1
  
  # Perform OLS
  reg3 <- lm(logTC_sub_logPF ~ logQ + logPL_sub_logPF + logPK_sub_logPF + z, data=Nerlove1963)
  
  # Store RSS
  RSS[i] <- sum(resid(reg3)^2)
}

# Plotting RSS to visualise
plot(RSS)
min(RSS)

# Find the value for beta7 that minimises RSS
beta7min <- beta7[which.min(RSS)]

# Save the regression which minimises RSS
# Generate corresponding z variable
Nerlove1963$z = Nerlove1963$logQ*(1+exp(beta7min - Nerlove1963$logQ))^-1

# Perform OLS
reg_GLS <- lm(logTC_sub_logPF ~ logQ + logPL_sub_logPF + logPK_sub_logPF + z, data=Nerlove1963)

# Store RSS
RSS_GLS <- sum(resid(reg_GLS)^2)

# Generate output for LaTeX
stargazer(reg_GLS, single.row = TRUE,
          title = "Results", 
          align =TRUE, out="Homework-3/Results/reg_GLS_results.tex")

##################################### Q2D ######################################

# Calculating standard errors using formula for asymptotic distribution of NLS
# First estimating sigma
n <- 145
k <- 6
sigmasqhat <- RSS_GLS/(n-k)

# Next estimating the inside of the brackets - need to estimate the Hessian
H <- matrix(0, 6, 6)

# Not sure how to do this - need to correct
H[1,1] <- 999
H[2,2] <- mean(Nerlove1963$logQ^2)
H[3,3] <- mean(Nerlove1963$logPL_sub_logPF^2)
H[4,4] <- mean(Nerlove1963$logPK_sub_logPF^2)
H[5,5] <- mean(Nerlove1963$z)^2
H[6,6] <- 999

# Using this to compute Var(betahat_GLS)
V_beta_GLS <- (1/n)*sigmasqhat*solve(H)

# Taking square root of the diagonals to obtain SE's
se_beta_GLS <- rep(0, 6)
for (i in 1:6){
  se_beta_GLS[i] <- sqrt(V_beta_GLS[i,i])
}

################################################################################
##################################### Q6 #######################################
################################################################################

##################################### Q6A ######################################

# Import data
Hall1978 <- read_excel("Homework-3/PS4data.xls")

# Create consumption variable
Hall1978$CPC <- (Hall1978$`real consumption of nondurables` + Hall1978$`real consumption of services`)/Hall1978$population

# Turn into ts object
Hall1978ts = ts(Hall1978$CPC, start = c(1948, 1), frequency = 4)

# Run regression
reg_hall_u <- dynlm(CPC ~ L(CPC, 1) + L(CPC, 2) + L(CPC, 3) + L(CPC, 4), Hall1978ts)
RSS_u <- sum(resid(reg_hall_u)^2)

# Run restricted regression
reg_hall_r <- dynlm(CPC ~ L(CPC, 1), Hall1978ts)
RSS_r <- sum(resid(reg_hall_r)^2)
F <- ((RSS_r - RSS_u)/3)/((RSS_u)/(220-5))

# Critical value
F_crit <- qf(0.95, 3, (220-5))
