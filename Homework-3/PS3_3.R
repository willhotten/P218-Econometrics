#' This script helps to answer question 3 of P218 - Econometrics HW3.
#' 
#' Last update: 22/11/2022
#' By: @willhotten

################################################################################
#################################### Setup #####################################
################################################################################

# Clean environment
rm(list=ls())

# Define a vector of packages that the script will use
pkgs_required <- c("AER", "estimatr", "tidyverse", "vtable", "stargazer", "texreg", "ggplot2", "readxl")
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
##################################### Q3 #######################################
################################################################################

##################################### Q3D ######################################

# Import data
wage <- read_excel("Homework-3/wage.xlsx")

# Create log(wage) var
wage$lwage <- log(wage$wage)

# Define parameter values
# Number male / female
n <- count(wage, male)
n1 <- n$n[1]
n2 <- n$n[2]

# Demean wage and logwage
wage <- wage %>%
  group_by(male) %>%
  mutate(across(c("wage", "lwage"), ~ .x - mean(.x), .names = "dm_{col}"))

# Demeaned squared and cubed
wage$dm_lwage_2 <- wage$dm_lwage^2
wage$dm_lwage_4 <- wage$dm_lwage^4
sum_dm_lwage_2 <- aggregate(wage$dm_lwage_2, by=list(male=wage$male), FUN=sum)

# Summed observations for kappa formula
A <- sum(wage$dm_lwage_4)
B <- sum(wage$dm_lwage_2)^2

# s-squared
s1 <- solve(n1-1)*sum_dm_lwage_2[1,2]
s2 <- solve(n2-1)*sum_dm_lwage_2[2,2]

# Calculate kappa hat and T
# kappahat
kappahat <- (n1 + n2)*A/B
# T
T <- ((n1*n2)/(n1 + n2))^(0.5)*(log(s1) - log(s2))

# Hypothesis testing using distributions calculated in question
# Calculate t / sqrt(kappa - 1)
Zstat <- T/sqrt(kappahat-1)
# Compare to critical value
Z <- qnorm(0.95, mean = 0, sd = 1, lower.tail = TRUE)
diff <- Zstat - Z
# diff <0 so Z > Zstat so we fail to reject at 5% significance level

# Hypothesis testing using standard normal theory test
Fstat <- s1/s2
F <- qf(0.95, n1-1, n2-1, lower.tail=TRUE)
diff2 <- Fstat - F
# diff2 >0 so we reject at 5% significance level - reject using standard test 