# Loading the required packages
library(brms)
library(dplyr)
library(bayesplot)
library(posterior)
library(ggplot2)
library(bayesplot)
options(scipen = 9999)

# install.packages(
#   "cmdstanr",
#   repos = c("https://mc-stan.org/r-packages/", getOption("repos"))
# )

# install.packages(c("cmdstanr", "brms", "posterior", "rstan"))


# Importing the data file
db <- read.csv("/Users/christinegrech/Desktop/Course/Year 4/Dissertation/Data/Final Data/data_final_standardised.csv", header = TRUE)


# install_cmdstan()
library(cmdstanr)
options(brms.backend = "cmdstanr")

### Model 0 (M0) ###
model_0 <- brm(
  formula = log_consumption_std ~ 1 + (1 | nace_section_letter) + (1 | Locality_id),
  data = db,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(student_t(4, 0, 0.5), class = "sd"),     # random effect SDs
    prior(student_t(4, 0, 0.5), class = "sigma")   # residual SD
  ),
  cores = 4, chains = 4, iter = 4000, warmup =2000, control = list(adapt_delta = 0.95), seed = 123
)


# Summary for Model 0
summary(model_0)


# Posterior Checks for Model 0 (M0)
post_m0 <- as_draws_df(model_0)

fixed_pars_m0 <- c(
  "b_Intercept"
)

var_pars_m0 <- c(
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
for (p in fixed_pars_m0) {
  g <- mcmc_trace(post_m0, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m0_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m0) {
  g <- mcmc_trace(post_m0, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Iteration")
  
  print(g)
  ggsave(paste0("traceplot_m0_", q, ".pdf"), g, width = 4, height = 3.5)
}

## Posterior Density Plots
for (p in fixed_pars_m0) {
  g <- mcmc_dens_overlay(post_m0, pars = p) + 
    clean_plot_theme + 
    labs(title = p, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m0_", p, ".pdf"), g, width = 4, height = 3.5)
}

for (q in var_pars_m0) {
  g <- mcmc_dens_overlay(post_m0, pars = q) + 
    clean_plot_theme + 
    labs(title = q, x = "Value")
  
  print(g)
  ggsave(paste0("posterior_dist_m0_", q, ".pdf"), g, width = 4, height = 3.5)
}


# Graphical Posterior Predictive Checks
p1 <- pp_check(model_0, type = "dens_overlay", ndraws = 100) +
  clean_plot_theme
p2 <- pp_check(model_0, type = "stat", stat = "sd", ndraws = 100) +
  clean_plot_theme
p3 <- pp_check(model_0, type = "stat", stat = "max", ndraws = 100) +
  clean_plot_theme
p4 <- pp_check(model_0, type = "intervals", ndraws = 100) +
  clean_plot_theme
p5 <- pp_check(model_0, type = "stat", stat = "mean", ndraws = 100) +
  clean_plot_theme


# Saving
ggsave("/Users/christinegrech/Desktop/ppc_comparing_obs_rep_m0.pdf", p1, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_sd_m0.pdf",           p2, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_max_m0.pdf",          p3, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_intervals_m0.pdf",    p4, width = 6, height = 4)
ggsave("/Users/christinegrech/Desktop/ppc_mean_m0.pdf",           p5, width = 6, height = 4)


# Descriptive statistics on the posterior distribution of M0
library(bayestestR)
describe_posterior(model_0, centrality = "all", test = c("p_direction", "rope"), ci=0.89, ci_method = "HDI")


# Calculating the ICC
var_nace <- post_m0$sd_nace_section_letter__Intercept^2
var_loc  <- post_m0$sd_Locality_id__Intercept^2
var_res  <- post_m0$sigma^2

icc_nace  <- var_nace / (var_nace + var_loc + var_res)
icc_loc   <- var_loc  / (var_nace + var_loc + var_res)
icc_total <- (var_nace + var_loc) / (var_nace + var_loc + var_res)

posterior_summary <- function(x) {
  c(
    mean = mean(x),
    median = median(x),
    l95 = quantile(x, 0.025),
    u95 = quantile(x, 0.975)
  )
}

posterior_summary(icc_nace)
posterior_summary(icc_loc)
posterior_summary(icc_total)
