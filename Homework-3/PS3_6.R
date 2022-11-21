#' This script helps to answer question 6 of P218 - Econometrics HW3.
#' 
#' Last update: 21/11/2022
#' By: @willhotten

################################################################################
#################################### Setup #####################################
################################################################################

# Clean environment
rm(list=ls())

# Define a vector of packages that the script will use
pkgs_required <- c("AER", "estimatr", "tidyverse", "vtable", "stargazer", "texreg", "ggplot2", "readxl", "dynlm", "tsbox", "Hmisc", "autoReg")
pkgs_install  <- pkgs_required[!(pkgs_required %in% installed.packages()[,"Package"])]

# If any package is not already install then install it in machine
if (length(pkgs_install) > 0) {a
  install.packages(pkgs_install, dependencies=TRUE)
}

# Import necessary packages
invisible(lapply(pkgs_required, library, character.only=TRUE))
# Import necessary packages
invisible(lapply(pkgs_required, library, character.only=TRUE))

################################################################################
##################################### Q6 #######################################
################################################################################

##################################### Q6A ######################################

# Import data
Hall1978 <- read_excel("Homework-3/PS4data.xls")
Hall1978 <- as.data.frame(Hall1978)

# Create consumption variable
Hall1978$CPC <- (Hall1978$`real consumption of nondurables` + Hall1978$`real consumption of services`)/Hall1978$population

# Turn into ts object
CPC <- ts(Hall1978$CPC, start = c(1948, 1), frequency = 4)

# Run regression
reg_hall_u <- dynlm(CPC ~ L(CPC, 1) + L(CPC, 2) + L(CPC, 3) + L(CPC, 4), CPC)

# Unrestricted regression outputs
stargazer(reg_hall_u, single.row = TRUE,
          title = "Unrestricted regression results", 
          align =TRUE, out="Homework-3/Results/Hallunrestricted_results.tex")

# Store RSS from unrestricted
RSS_u <- sum(resid(reg_hall_u)^2)

# Run restricted regression
reg_hall_r <- dynlm(CPC ~ L(CPC, 1), CPC)

# Unrestricted regression outputs
stargazer(reg_hall_r, single.row = TRUE,
          title = "Restricted regression results", 
          align =TRUE, out="Homework-3/Results/Hallrestricted_results.tex")

# Store RSS from restricted
RSS_r <- sum(resid(reg_hall_r)^2)
F <- ((RSS_r - RSS_u)/3)/((RSS_u)/(220-5))

# Critical value
F_crit <- qf(0.95, 3, (220-5))

##################################### Q6C ######################################

# Generate log variables
# Log of consumption per capita
Hall1978$log_CPC <- log(Hall1978$CPC)
# Income per capita
Hall1978$YPC <- Hall1978$`real disposable income`/Hall1978$population
# Log of income per capita
Hall1978$log_YPC <- log(Hall1978$YPC)

# Creating lagged vars
Hall1978$log_CPC_l1 <- Lag(Hall1978$log_CPC, 1)
Hall1978$log_CPC_l2 <- Lag(Hall1978$log_CPC, 2)
Hall1978$log_CPC_l3 <- Lag(Hall1978$log_CPC, 3)
Hall1978$log_CPC_l4 <- Lag(Hall1978$log_CPC, 4)
Hall1978$log_CPC_l5 <- Lag(Hall1978$log_CPC, 5)
Hall1978$log_CPC_l6 <- Lag(Hall1978$log_CPC, 6)
Hall1978$log_YPC_l1 <- Lag(Hall1978$log_YPC, 1)
Hall1978$log_YPC_l2 <- Lag(Hall1978$log_YPC, 2)

# Creating lagged diff vars for instruments, and dependent variables (1 and 2 stage)
Hall1978$DV2 <- Hall1978$log_CPC - Hall1978$log_CPC_l1
Hall1978$w1 <- Hall1978$log_CPC_l2 - Hall1978$log_CPC_l3
Hall1978$w2 <- Hall1978$log_CPC_l3 - Hall1978$log_CPC_l4
Hall1978$w3 <- Hall1978$log_CPC_l4 - Hall1978$log_CPC_l5
Hall1978$w4 <- Hall1978$log_CPC_l5 - Hall1978$log_CPC_l6
Hall1978$Y <- Hall1978$log_YPC - Hall1978$log_YPC_l1

# First stage regression of Y on instruments
reg_1s <- lm(Y ~ w1 + w2 + w3 + w4, Hall1978)

# First stage outputs table
stargazer(reg_1s, single.row = TRUE,
          title = "1st stage results", 
          align =TRUE, out="Homework-3/Results/1stage_results.tex")

# Second stage dataframe and regression
stage2 <- data.frame()[1:214, ]
stage2$fittedvalues <- reg_1s$fitted.values
stage2$Y <- Hall1978$DV2[-c(1,2,3,4,5,6)]

# Run 2nd stage regression
# Without robust standard errors for use with stargazer
reg_2s <- lm(Y ~ fittedvalues, stage2)
# With White standard errors (HC0)
reg_2s_rob <- lm_robust(Y ~ fittedvalues, stage2, se_type="HC0")

# Second stage outputs table
stargazer(reg_2s, 
          single.row = TRUE,
          se = starprep(reg_2s, se_type = "HC0"),
          title = "2nd stage results",
          align =TRUE,
          out="Homework-3/Results/2stage_results.tex")

##################################### Q6D ######################################

# Alternative way to do 2SLS is using ivreg command
# IV regression - combined
reg_iv <- ivreg(DV2 ~ Y |
              w1 + w2 + w3 + w4,
              data = Hall1978)

# HC0 (White) robust standard errors
coeftest(reg_iv, vcov=vcovHC(reg_iv, type="HC0"))

# Tests for endogeneity and weak instruments
summ_reg_iv <- summary(reg_iv, diagnostics=TRUE)
summ_reg_iv


