library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(writexl)
library(readxl)
library(brms)
library(bayesplot)

#read dataset
survivor_data <- read_excel("Desktop/Bayesian/project/m
                            erged_survivor_data_final.xlsx")
View(merged_survivor_data_final)

#colnames(survivor_data)

#dealing with censored data
#see success data 
table(survivor_data$success, useNA = "ifany")

#Step 1: Clean and recode the success variable
survivor_data <- survivor_data %>%
  mutate(
    success_clean = case_when(
      success == "Yes" ~ 1L,
      success == "No" ~ 0L,
      TRUE ~ NA_integer_  # Set all others to NA
    )
  )

survivor_data$n_finalists <- factor(survivor_data$n_finalists)
# Verify recoding
table(survivor_data$success_clean, useNA = "ifany")

table(survivor_data$n_finalists)

# Step 1: Fit the model
fit <- brm(
  formula = success_clean ~ gender + ethnicity + lgbt + collar +
    
    #challenge performance covariates
    n_individual_challenges + 
    n_tribal_challenges +
    n_reward_challenges +
    
    # Tribal dynamics
    n_tribe +
    n_finalists +  # Convert to factor for categorical treatment
    
    # Random effects
    (1 | version_season),
  
  data = survivor_data,
  family = bernoulli(),
  prior = c(
    prior("normal(0, 1)", class = "b"),
    prior("normal(0, 1)", class = "Intercept"),
    prior("cauchy(0, 1)", class = "sd")
  ),
  chains = 4, iter = 4000, control = list(adapt_delta = 0.95), seed = 123
)


# Step 2: Posterior Predictive Checks 
#(black -observed data, lightblue-replications)
pp_check(fit, ndraws = 100)

#interpretation: 
#Systematic bias:
#the dark line consistently deviates from the replicates → misspecification

plot(conditional_effects(fit))

# Compares observed vs predicted 0/1 counts
pp_check(fit, type = "bars", ndraws = 100)  

# Checks predicted vs actual mean success rate
pp_check(fit, type = "stat", stat = "mean")  

#when using latent variable it must not contain dots/underscores
survivor_data <- survivor_data |>
  rename(
    fidols = f_idols,
    uidols = u_idols
  )
view(survivor_data)

# Remove column by name
#survivor_data$success_clean <- NULL

#Need to have idol columns !!!!
#Model #2:
fit_latent <- brm(
  bf(
    success ~ gender + ethnicity + lgbt + collar + 
      n_finalists + z_proficiency + (1 | version_season),
    f_idols ~ z_proficiency,
    u_idols | trials(f_idols) ~ z_proficiency,
    nl = FALSE
  ) +
    lf(z_proficiency ~ (1 | version_season)),
  
  data = survivor_data,
  
  family = list(
    bernoulli(link = "logit"),   # for success
    poisson(),                   # for f_idols
    binomial()                   # for u_idols | f_idols
  ),
  
  prior = c(
    # Main outcome
    prior(normal(0, 1), class = "b", resp = "success"),
    prior(normal(0, 1), class = "Intercept", resp = "success"),
    
    # Idols found
    prior(normal(0, 0.5), class = "b", resp = "fidols"),
    prior(normal(0, 0.5), class = "Intercept", resp = "fidols"),
    
    # Idols used
    prior(normal(0, 0.5), class = "b", resp = "uidols"),
    prior(normal(0, 0.5), class = "Intercept", resp = "uidols"),
    
    # Latent proficiency
    prior(normal(0, 1), class = "sd", resp = "zproficiency"),
    prior(normal(0, 0.5), class = "sigma", resp = "zproficiency")
  ),
  
  chains = 4, iter = 4000, cores = 4,
  control = list(adapt_delta = 0.95),
  seed = 123
)

