# Calling the required packages
library(brms)
library(dplyr)
library(bayesplot)
library(posterior)
options(scipen = 9999)

# Importing the dataset.
db <- read.csv("/Users/christinegrech/Desktop/Course/Year 4/Dissertation/Data/Final Data/data_final_to_use.csv", header = TRUE)

# Log transforming and Z-standardising the response variable (consumption).
db$log_consumption <- log(db$Consumption_KWH)
mu_log  <- mean(db$log_consumption, na.rm = TRUE)
sd_log  <- sd(db$log_consumption, na.rm = TRUE)

db$log_consumption_std <- (db$log_consumption - mu_log) / sd_log

# Checks
summary(db$log_consumption_std)
mean(db$log_consumption_std)
sd(db$log_consumption_std)

db <- db %>%
  mutate(
    Company_Size_z = (Company_Size - mean(Company_Size, na.rm = TRUE)) / sd(Company_Size, na.rm = TRUE), # Z-standardising company size
    Company_id = factor(Company_id),
  )

# Exporting the datasets with the standardised variables.
write.csv(db, "/Users/christinegrech/Desktop/Course/Year 4/Dissertation/Data/Final Data/data_final_standardised.csv" )

# Histogram of log transformed and standardised consumption
hist(
  db$log_consumption_std,
  breaks = 50,
  prob = TRUE,   
  main = "Histogram of Standardised log(Consumption)",
  xlab = "Standardised log(kWh)",
  col = "#1B98E0BF",
  border = "white"
)

curve(
  dnorm(x, mean = mean(db$log_consumption_std, na.rm = TRUE),
        sd = sd(db$log_consumption_std, na.rm = TRUE)),
  col = "red",
  lwd = 2,
  add = TRUE
)

qqnorm(db$log_consumption_std,
       main = "Q-Q Plot of Standardised log(Electricity Consumption)")

