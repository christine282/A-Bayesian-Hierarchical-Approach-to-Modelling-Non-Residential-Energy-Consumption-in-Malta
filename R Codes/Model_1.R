# Loading the required packages
library(brms)
library(dplyr)
library(posterior)
library(ggplot2)
library(bayesplot)
library(bayestestR)
library(cmdstanr)
options(brms.backend = "cmdstanr")
options(scipen = 9999)

# Importing the final datasets.
db <- read.csv("/Users/christinegrech/Desktop/Course/Year 4/Dissertation/Data/Final Data/data_final_standardised.csv", header = TRUE)


### Model 1A (M1A) ###
model_1A <- brm(
  formula = log_consumption_std ~ Year + Company_Size_z +
    (1 | nace_section_letter) + (1 | Locality_id),
  data = db,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0, 1), class = "b"),
    prior(student_t(4, 0, 0.5), class = "sd"),     # random effect SDs
    prior(student_t(4, 0, 0.5), class = "sigma")   # residual SD
  ),
  cores = 4, chains = 4, iter = 4000, warmup =2000, control = list(adapt_delta = 0.95), seed = 123
)


# Summary for Model 1A
summary(model_1A)


# Posterior Checks for Model 1A (M1A)
post_m1A <- as_draws_df(model_1A)

fixed_pars_m1A <- c(
  "b_Intercept",
  "b_Year",
  "b_Company_Size_z"
)

var_pars_m1A <- c(
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
for (p in fixed_pars_m1A) {
  g <- mcmc_trace(post_m1A, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m1a_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m1A) {
  g <- mcmc_trace(post_m1A, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m1a_", q, ".pdf"), g, width = 4, height = 3.5)
}


## Posterior Density Plots
for (p in fixed_pars_m1A) {
  g <- mcmc_dens_overlay(post_m1A, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m1a_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m1A) {
  g <- mcmc_dens_overlay(post_m1A, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m1a_", q, ".pdf"), g, width = 4, height = 3.5)
}


# Graphical Posterior Predictive Checks
p1 <- pp_check(model_1A, type = "dens_overlay", ndraws = 100) +
  clean_plot_theme
p2 <- pp_check(model_1A, type = "stat", stat = "sd", ndraws = 100) +
  clean_plot_theme
p3 <- pp_check(model_1A, type = "stat", stat = "max", ndraws = 100) +
  clean_plot_theme
p4 <- pp_check(model_1A, type = "intervals", ndraws = 100) +
  clean_plot_theme
p5 <- pp_check(model_1A, type = "stat", stat = "mean", ndraws = 100) +
  clean_plot_theme

# Saving
ggsave("/Users/christinegrech/Desktop/ppc_comparing_obs_rep_m1A.pdf", p1, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_sd_m1A.pdf",           p2, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_max_m1A.pdf",          p3, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_intervals_m1a.pdf",    p4, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_mean_m1A.pdf",          p5, width = 6, height = 4)


# Descriptive statistics on the posterior distribution of M1
describe_posterior(model_1A, effects = "all", component = "all", centrality = "all", test = c("p_direction", "rope"), ci = 0.89, ci_method = "HDI")




### Model 1B (M1B) ###
# A more flat prior for 'b'
model_1B <- brm(
  formula = log_consumption_std ~ Year + Company_Size_z +
    (1 | nace_section_letter) + (1 | Locality_id),
  data = db,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0, 100), class = "b"),
    prior(student_t(4, 0, 0.5), class = "sd"),     
    prior(student_t(4, 0, 0.5), class = "sigma")
  ),
  cores = 4, chains = 4, iter = 4000, warmup =2000, control = list(adapt_delta = 0.95), seed = 123
)


# Summary for Model 1B
summary(model_1B)


# Posterior Checks for Model 1B (M1B)
post_m1B <- as_draws_df(model_1B)

fixed_pars_m1B <- c(
  "b_Intercept",
  "b_Year",
  "b_Company_Size_z"
)

var_pars_m1B <- c(
  "sd_nace_section_letter__Intercept",
  "sd_Locality_id__Intercept",
  "sigma"
)

## Traceplots
for (p in fixed_pars_m1B) {
  g <- mcmc_trace(post_m1B, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m1b_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m1B) {
  g <- mcmc_trace(post_m1B, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m1b_", q, ".pdf"), g, width = 4, height = 3.5)
}

## Posterior Density Plots
for (p in fixed_pars_m1B) {
  g <- mcmc_dens_overlay(post_m1B, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m1b_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m1B) {
  g <- mcmc_dens_overlay(post_m1B, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m1b_", q, ".pdf"), g, width = 4, height = 3.5)
}


# Graphical Posterior Predictive Checks
p1 <- pp_check(model_1B, type = "dens_overlay", ndraws = 100) +
  clean_plot_theme
p2 <- pp_check(model_1B, type = "stat", stat = "sd", ndraws = 100) +
  clean_plot_theme
p3 <- pp_check(model_1B, type = "stat", stat = "max", ndraws = 100) +
  clean_plot_theme
p4 <- pp_check(model_1B, type = "intervals", ndraws = 100) +
  clean_plot_theme
p5 <- pp_check(model_1B, type = "stat", stat = "mean", ndraws = 100) +
  clean_plot_theme

# Saving
ggsave("/Users/christinegrech/Desktop/ppc_comparing_obs_rep_m1B.pdf", p1, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_sd_m1B.pdf",           p2, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_max_m1B.pdf",          p3, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_intervals_m1B.pdf",    p4, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_mean_m1B.pdf",          p5, width = 6, height = 4)


# Descriptive statistics on the posterior distribution of M1B
describe_posterior(model_1B, effects = "all", component = "all", centrality = "all", test = c("p_direction", "rope"), ci = 0.89, ci_method = "HDI")



### Prior Sensitivity Analysis ###
library(tidybayes)
library(ggplot2)
library(dplyr)

# For Company Size
# Label and combine draws
draws_1A_cs <- model_1A %>% gather_draws(b_Company_Size_z) %>% mutate(model = "Standard Normal")
draws_1B_cs <- model_1B  %>% gather_draws(b_Company_Size_z) %>% mutate(model = "Flat Prior")

bind_rows(draws_1A_cs, draws_1B_cs) %>%
  ggplot(aes(x = .value, fill = model)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Prior Sensitivity: Posterior Comparison for Company Size", x = "Coefficient Value")

# For Year
# Label and combine draws
draws_1A_year <- model_1A %>% gather_draws(b_Year) %>% mutate(model = "Standard Normal")
draws_1B_year <- model_1B  %>% gather_draws(b_Year) %>% mutate(model = "Flat Prior")

bind_rows(draws_1A_year, draws_1B_year) %>%
  ggplot(aes(x = .value, fill = model)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Prior Sensitivity: Posterior Comparison for Year", x = "Coefficient Value")




### Model 1A* (M1A*) ###
# Without the Locality random intercept.
model_1A_nl <- brm(
  formula = log_consumption_std ~ Year + Company_Size_z +
    (1 | nace_section_letter),
  data = db,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0, 1), class = "b"),
    prior(student_t(4, 0, 0.5), class = "sd"),     # random effect SDs
    prior(student_t(4, 0, 0.5), class = "sigma")   # residual SD
  ),
  cores = 4, chains = 4, iter = 4000, warmup =2000, control = list(adapt_delta = 0.95), seed = 123
)


# Summary for Model 1A*
summary(model_1A_nl)


# Posterior Checks for Model 1A* (M1A*)
post_m1A_nl <- as_draws_df(model_1A_nl)

fixed_pars_m1A_nl <- c(
  "b_Intercept",
  "b_Year",
  "b_Company_Size_z"
)

var_pars_m1A_nl <- c(
  "sd_nace_section_letter__Intercept",
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
for (p in fixed_pars_m1A_nl) {
  g <- mcmc_trace(post_m1A_nl, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m1a_nl_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m1A_nl) {
  g <- mcmc_trace(post_m1A_nl, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m1a_nl_", q, ".pdf"), g, width = 4, height = 3.5)
}

## Posterior Density Plots
for (p in fixed_pars_m1A_nl) {
  g <- mcmc_dens_overlay(post_m1A_nl, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m1a_nl_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m1A_nl) {
  g <- mcmc_dens_overlay(post_m1A_nl, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m1a_nl_", q, ".pdf"), g, width = 4, height = 3.5)
}


# Graphical Posterior Predictive Checks
p1 <- pp_check(model_1A_nl, type = "dens_overlay", ndraws = 100) +
  clean_plot_theme
p2 <- pp_check(model_1A_nl, type = "stat", stat = "sd", ndraws = 100) +
  clean_plot_theme
p3 <- pp_check(model_1A_nl, type = "stat", stat = "max", ndraws = 100) +
  clean_plot_theme
p4 <- pp_check(model_1A_nl, type = "intervals", ndraws = 100) +
  clean_plot_theme 
p5 <- pp_check(model_1A_nl, type = "stat", stat = "mean", ndraws = 100) +
  clean_plot_theme

# Saving
ggsave("/Users/christinegrech/Desktop/ppc_comparing_obs_rep_m1a_nl.pdf", p1, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_sd_m1a_nl.pdf",           p2, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_max_m1a_nl.pdf",          p3, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_intervals_m1a_nl.pdf",    p4, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_mean_m1a_nl.pdf",          p5, width = 6, height = 4)


# Descriptive statistics on the posterior distribution of M1A*
describe_posterior(model_1A_nl, effects = "all", component = "all", centrality = "all", test = c("p_direction", "rope"), ci = 0.89, ci_method = "HDI")




### Comparing Models 1A and 1A* ###
# The aim of this part is to compare Models 1A and 1A*:
# Model 1A (model_1A) which considers a random intercept for the NACE section and the locality
# and Model 1A* (model_1A_nl) which considers only a random intercept for the NACE sector. Both 
# models are fitted and are compared in order to decide whether the random intercept for the 
# locality should be retained or not.

# Loading the required packages
library(dplyr)
library(ggplot2)


yrep_1A <- posterior_predict(model_1A)
yrep_1A_nl <- posterior_predict(model_1A_nl)

pp_loc_1A <- data.frame(
  Locality_id = db$Locality_id,
  yrep_mean = colMeans(yrep_1A)
)

pp_loc_1A_nl <- data.frame(
  Locality_id = db$Locality_id,
  yrep_mean = colMeans(yrep_1A_nl)
)

pp_loc_1A %>%
  group_by(Locality_id) %>%
  summarise(mean_pred = mean(yrep_mean)) %>%
  print(n=68)

pp_loc_1A_nl %>%
  group_by(Locality_id) %>%
  summarise(mean_pred = mean(yrep_mean)) %>%
  print(n=68)


# Observed vs Predicted Locality Means
# Observed locality means
obs_loc <- db %>%
  group_by(Locality_id) %>%
  summarise(y_obs = mean(log_consumption_std))

# Predicted locality means for each model
pred_loc_A <- pp_loc_1A %>%
  group_by(Locality_id) %>%
  summarise(y_pred = mean(yrep_mean)) %>%
  mutate(Model = "With Locality")

pred_loc_B <- pp_loc_1A_nl %>%
  group_by(Locality_id) %>%
  summarise(y_pred = mean(yrep_mean)) %>%
  mutate(Model = "Without Locality")

pred_loc <- bind_rows(pred_loc_A, pred_loc_B)
plot_data <- left_join(obs_loc, pred_loc, by = "Locality_id")

# Plot - Observed vs Predicted Locality Means
ggplot(plot_data, aes(x = y_obs, y = y_pred, colour = Model)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Observed Locality Mean",
    y = "Posterior Predictive Locality Mean",
    title = "Observed vs Predicted Locality Means"
  ) +
  theme_minimal()