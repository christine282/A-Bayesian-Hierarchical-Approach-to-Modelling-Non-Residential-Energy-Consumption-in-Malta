# Calling the required packages
library(brms)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(bayestestR)
library(parameters)
options(scipen = 9999)
library(cmdstanr)
options(brms.backend = "cmdstanr")

# Importing the dataset.
db <- read.csv("/Users/christinegrech/Desktop/Course/Year 4/Dissertation/Data/Final Data/data_final_to_use.csv", header = TRUE)

### Model 2A (M2A) - Adding a company-specific Intercept ###
# N(0,1) prior
model_2A <- brm(
  formula = log_consumption_std ~ Year + Company_Size_z +
    (1 | Company_id) +             
    (1 | nace_section_letter) +
    (1 | Locality_id),
  data = db,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0, 1), class = "b"),
    prior(student_t(4, 0, 0.5), class = "sd"),
    prior(student_t(4, 0, 0.5), class = "sigma")
  ),
  cores = 4, chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99),
  seed = 123
)


# Summary for Model 2A
summary(model_2A)


# Posterior Checks for model_2A (M2A)
post_m2A <- as_draws_df(model_2A)

fixed_pars_m2A <- c(
  "b_Intercept",
  "b_Year",
  "b_Company_Size_z"
)

var_pars_m2A <- c(
  "sd_Company_id__Intercept",
  "sd_nace_section_letter__Intercept",
  "sd_Locality_id__Intercept",
  "sigma"
)

# Traceplots and Posterior Density Plots
# Defining a consistent theme for the plots
clean_plot_theme <- theme_classic() + 
  theme(
    aspect.ratio = 0.8,                
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

## Traceplots
for (p in fixed_pars_m2A) {
  g <- mcmc_trace(post_m2A, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m2a_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m2A) {
  g <- mcmc_trace(post_m2A, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m2a_", q, ".pdf"), g, width = 4, height = 3.5)
}

## Posterior Density Plots
for (p in fixed_pars_m2A) {
  g <- mcmc_dens_overlay(post_m2A, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m2a_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m2A) {
  g <- mcmc_dens_overlay(post_m2A, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m2a_", q, ".pdf"), g, width = 4, height = 3.5)
}


# Graphical Posterior Predictive Checks
p1 <- pp_check(model_2A, type = "dens_overlay", ndraws = 100) +
  clean_plot_theme 
p2 <- pp_check(model_2A, type = "stat", stat = "sd", ndraws = 100) +
  clean_plot_theme 
p3 <- pp_check(model_2A, type = "stat", stat = "max", ndraws = 100) +
  clean_plot_theme 
p4 <- pp_check(model_2A, type = "intervals", ndraws = 100) +
  clean_plot_theme 
p5 <- pp_check(model_2A, type = "stat", stat = "mean", ndraws = 100) +
  clean_plot_theme

# Saving
ggsave("/Users/christinegrech/Desktop/ppc_comparing_obs_rep_m2A.pdf", p1, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_sd_m2A.pdf",           p2, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_max_m2A.pdf",          p3, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_intervals_m2a.pdf",    p4, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_mean_m2A.pdf",          p5, width = 6, height = 4)


# Descriptive statistics on the posterior distribution of M2A
results <- model_parameters(model_2A, effects = "all", group_level = TRUE, centrality = "all", test = c("p_direction", "rope"), ci = 0.89, ci_method = "hdi")
print(results)



### Model 2B (M2B) ###
# Trying a more flat prior for 'b'
model_2B <- brm(
  formula = log_consumption_std ~ Year + Company_Size_z +
    (1 | Company_id) +             
    (1 | nace_section_letter) +
    (1 | Locality_id),
  data = db,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0, 1), class = "b"),
    prior(student_t(4, 0, 0.5), class = "sd"),
    prior(student_t(4, 0, 0.5), class = "sigma")
  ),
  cores = 4, chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99),
  seed = 123
)


# Summary for Model 2B
summary(model_2B)


# Posterior Checks for model 2B (M2B)
post_m2B <- as_draws_df(model_2B)

fixed_pars_m2B <- c(
  "b_Intercept",
  "b_Year",
  "b_Company_Size_z"
)

var_pars_m2B <- c(
  "sd_Company_id__Intercept",
  "sd_nace_section_letter__Intercept",
  "sd_Locality_id__Intercept",
  "sigma"
)

# Traceplots and Posterior Density Plots
# Defining a consistent theme for the plots
clean_plot_theme <- theme_classic() + 
  theme(
    aspect.ratio = 0.8,                
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

## Traceplots
for (p in fixed_pars_m2B) {
  g <- mcmc_trace(post_m2B, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m2b_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m2B) {
  g <- mcmc_trace(post_m2B, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m2b_", q, ".pdf"), g, width = 4, height = 3.5)
}

## Posterior Density Plots
for (p in fixed_pars_m2B) {
  g <- mcmc_dens_overlay(post_m2B, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m2b_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m2B) {
  g <- mcmc_dens_overlay(post_m2B, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m2b_", q, ".pdf"), g, width = 4, height = 3.5)
}


# Graphical Posterior Predictive Checks
p1 <- pp_check(model_2B, type = "dens_overlay", ndraws = 100) +
  clean_plot_theme 
p2 <- pp_check(model_2B, type = "stat", stat = "sd", ndraws = 100) +
  clean_plot_theme 
p3 <- pp_check(model_2B, type = "stat", stat = "max", ndraws = 100) +
  clean_plot_theme 
p4 <- pp_check(model_2B, type = "intervals", ndraws = 100) +
  clean_plot_theme
p5 <- pp_check(model_2B, type = "stat", stat = "mean", ndraws = 100) +
  clean_plot_theme 

# Saving
ggsave("/Users/christinegrech/Desktop/ppc_comparing_obs_rep_m2b.pdf", p1, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_sd_m2b.pdf",           p2, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_max_m2b.pdf",          p3, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_intervals_m2b.pdf",    p4, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_mean_m2b.pdf",          p5, width = 6, height = 4)


# Descriptive statistics on the posterior distribution of M2B
results <- model_parameters(model_2B, effects = "all", group_level = TRUE, centrality = "all", test = c("p_direction", "rope"), ci = 0.89, ci_method = "hdi")
print(results)


### Prior Sensitivity Analysis ###
library(tidybayes)
library(ggplot2)
library(dplyr)

# For Company Size
# Label and combine draws
draws_2A_cs <- model_2A %>% gather_draws(b_Company_Size_z) %>% mutate(model = "Standard Normal")
draws_2B_cs <- model_2B  %>% gather_draws(b_Company_Size_z) %>% mutate(model = "Flat Prior")

bind_rows(draws_2A_cs, draws_2B_cs) %>%
  ggplot(aes(x = .value, fill = model)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Prior Sensitivity: Posterior Comparison for Company Size", x = "Coefficient Value")

# For Year
# Label and combine draws
draws_2A_year <- model_2A %>% gather_draws(b_Year) %>% mutate(model = "Standard Normal")
draws_2B_year <- model_2B  %>% gather_draws(b_Year) %>% mutate(model = "Flat Prior")

bind_rows(draws_2A_year, draws_2B_year) %>%
  ggplot(aes(x = .value, fill = model)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Prior Sensitivity: Posterior Comparison for Year", x = "Coefficient Value")

