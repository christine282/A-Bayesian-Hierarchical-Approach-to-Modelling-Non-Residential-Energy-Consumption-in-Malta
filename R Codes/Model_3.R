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

# Importing the dataset
db <- read.csv("/Users/christinegrech/Desktop/Course/Year 4/Dissertation/Data/Final Data/data_final_standardised.csv", header = TRUE)

### Model 3A (Considering only the meaningful interactions) ###
model_3A <- brm(
  formula = log_consumption_std ~ Year + Company_Size_z +
    (1 | Company_id) +
    (1 | nace_section_letter) +
    (1 | Locality_id) +
    (1 | nace_section_letter:Locality_id), # NACE × Locality
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


# Summary for M3A
summary(model_3A)


# Posterior Checks for Model 3A (M3A)
post_m3A <- as_draws_df(model_3A)

fixed_pars_m3A <- c(
  "b_Intercept",
  "b_Year",
  "b_Company_Size_z"
)

var_pars_m3A <- c(
  "sd_Company_id__Intercept",
  "sd_nace_section_letter__Intercept",
  "sd_Locality_id__Intercept",
  "sd_nace_section_letter:Locality_id__Intercept",
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
for (p in fixed_pars_m3) {
  g <- mcmc_trace(post_m3, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m3a_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m3) {
  g <- mcmc_trace(post_m3, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m3a_", q, ".pdf"), g, width = 4, height = 3.5)
}

## Posterior Density Plots
for (p in fixed_pars_m3) {
  g <- mcmc_dens_overlay(post_m3, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m3a_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m3) {
  g <- mcmc_dens_overlay(post_m3, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m3a_", q, ".pdf"), g, width = 4, height = 3.5)
}


# Graphical Posterior Predicitve Checks
p1 <- pp_check(model_3A, type = "dens_overlay", ndraws = 100) +
  clean_plot_theme 
p2 <- pp_check(model_3A, type = "stat", stat = "sd", ndraws = 100) +
  clean_plot_theme 
p3 <- pp_check(model_3A, type = "stat", stat = "max", ndraws = 100) +
  clean_plot_theme 
p4 <- pp_check(model_3A, type = "intervals", ndraws = 100) +
  clean_plot_theme 
p5 <- pp_check(model_3A, type = "stat", stat = "mean", ndraws = 100) +
  clean_plot_theme 

# Saving
ggsave("/Users/christinegrech/Desktop/ppc_comparing_obs_rep_m3a.pdf", p1, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_sd_m3a.pdf",           p2, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_max_m3a.pdf",          p3, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_intervals_m3a.pdf",    p4, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_mean_m3a.pdf",          p5, width = 6, height = 4)


# Descriptive statistics on the posterior distribution of M1
results_3A <- model_parameters(model_3A, effects = "all", group_level = TRUE, centrality = "all", test = c("p_direction", "rope"), ci = 0.89, ci_method = "hdi")
print(results_3A)
