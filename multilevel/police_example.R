rm(list=ls())

library(tidyverse)
library(rstan)
library(tidybayes)
hablar::set_wd_to_script_path()

# -----------------------------------------------------------------------
# POLICE STOPS EXAMPLE

# load data
d <- read_delim("police_stops.txt")
str(d)

# NOTE:
# precincts are numbered 1-75
# ethnicity 1=black, 2=hispanic, 3=white
# crime type 1=violent, 2=weapons, 3=property, 4=drug

# -----------------------------------------------------------------------
# summary statistics

d_summaries <- d %>%
  mutate(eth = case_when(eth==1 ~ "black", 
                         eth==2 ~ "hispanic", 
                         eth==3 ~ "white")) %>%
  group_by(eth) %>%
  summarise(
    pop = sum(pop),
    total_stops   = sum(stops),
    total_arrests = sum(past.arrests),
    .groups = "drop"
  ) %>%
  # Compute proportions and ratios
  mutate(
    prop_of_all_stops      = total_stops / sum(total_stops),
    pop_fraction = pop / sum(pop),
  )


d_summaries

# d %>%
#   mutate(eth = factor(case_when(eth==1 ~ "black", 
#                          eth==2 ~ "hispanic", 
#                          eth==3 ~ "white"))) %>%
#   mutate(eth = relevel(eth, ref = "white")) %>%
#   filter(past.arrests>0) %>%
#   glm(stops ~ eth, family=poisson, offset=log(past.arrests), data=.) %>%
#   summary()

# -----------------------------------------------------------------------
# fit model 1

# correction for the values of which d$past.arrests == 0
d$past.arrests <- ifelse(d$past.arrests==0, 0.5, d$past.arrests)

# Prepare data list for Stan
stan_data <- list(
  N       = nrow(d),
  n_eth   = length(unique(d$eth)),   # should be 3 if e=1,2,3
  n_prec  = length(unique(d$precinct)),   # number of precincts in that subset
  y       = d$stops,
  eth     = d$eth,
  past_arrests  = d$past.arrests,
  precinct= d$precinct
)


# Fit the model
fit <- stan(
  file = "overdispersed_poisson.stan",
  data = stan_data,
  iter = 2000, chains = 4, cores = 4
)

print(fit, pars = c("alpha", "sigma_beta", "sigma_eps"), probs=c(0.05/2, 1-0.05/2))


print(fit, pars = c("beta"), probs=c(0.05/2, 1-0.05/2))

saveRDS(fit, file="fit_01.RDS")

# -----------------------------------------------------------------------
# prior predictive check

N <- nrow(d)
n_eth <- length(unique(d$eth))
n_prec <- length(unique(d$precinct))

# Decide how many draws from the prior to simulate
n_sims <- 200  

# We'll store all simulated Y values in a single big vector
# so we can compare them to the observed distribution
all_prior_stops <- numeric(0)

set.seed(101)  # reproducibility

for (s in 1:n_sims) {
  
  # 1) Sample the parameters from their priors
  
   
  alpha_draw <- rnorm(n_eth, mean = 0, sd = 5) # alpha: one for each ethnicity
  sigma_beta_draw <- rexp(1, rate = 1)
  beta_raw_draw <- rnorm(n_prec, 0, 1)
  beta_draw <- sigma_beta_draw * beta_raw_draw
  
  sigma_eps_draw <- rexp(1, rate = 1)          # Overdispersion
  eps_raw_draw <- rnorm(N, 0, 1)
  eps_draw <- sigma_eps_draw * eps_raw_draw
  
  # 2) Generate y (stops) from Poisson for each observation i

  past_arrests_offset <- ifelse(d$past.arrests == 0, 0.5, d$past.arrests)
  
  # compute the linear predictor for each row i
  lambda_vec <- numeric(N)
  for (i in seq_len(N)) {
    e <- d$eth[i]
    p <- d$precinct[i]
    lambda_vec[i] <- alpha_draw[e] + beta_draw[p] + eps_draw[i]
  }
  
  # Poisson means
  mu_vec <- past_arrests_offset * (15/12) * exp(lambda_vec)
  
  # sample from Poisson
  y_sim <- rpois(N, mu_vec)
  
  # 3) Store in a big vector for later comparison
  all_prior_stops <- c(all_prior_stops, y_sim)
}

# Now we have n_sims * N simulated 'stops' from the prior
# Compare with the empirical distribution:
observed_stops <- d$stops

df_plot <- tibble(
  value = c((observed_stops ), (all_prior_stops )),
  type  = rep(c("Empirical", "Prior"), c(length(observed_stops), length(all_prior_stops)))
)

# Plot densities
ggplot(df_plot, aes(x = value, fill = type)) +
  geom_density(alpha = 0.4) +
  labs(
    x = "log(stops)",
    y = "Density",
    title = "Prior Predictive vs. Empirical Distribution (log scale)",
    fill = "Data Source"
  ) +
  theme_minimal()+
  scale_x_log10()


# -----------------------------------------------------------------------
# add precinct-level predictors, the fraction of population
# that is black, white or hispanic
precinct_props <- d %>%
  distinct(precinct, eth, pop) %>%  # keep unique combos
  group_by(precinct) %>%
  summarize(
    total_pop      = sum(pop),
    black_pop      = sum(pop[eth == 1]),
    hispanic_pop   = sum(pop[eth == 2]),
    white_pop      = sum(pop[eth == 3])
  ) %>%
  mutate(
    p_black    = black_pop / total_pop,
    p_hispanic = hispanic_pop / total_pop,
    p_white  = white_pop / total_pop 
  )

precinct_props

d <- d %>%
  left_join(precinct_props, by = "precinct")
