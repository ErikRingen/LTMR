library(targets)
library(tarchetypes)
tar_source()
tar_option_set(packages = c(
  "tidyverse",
  "brms",
  "psych",
  "patchwork",
  "GGally",
  "bayesplot",
  "WeightIt",
  "gbm",
  "marginaleffects"
))

list(
# Index and clean data ---------------------------------------------------
  tar_target(raw_data_file, "raw_data/LTMR Coded data Updated.xlsx", format = "file"),
  # initial cleaning
  tar_target(data, clean_data(raw_data_file)),
  # make summary scores and join with demographic data
  tar_target(d_pid, sum_pid(data)),

# Descriptive plots -------------------------------------------------------
  tar_target(p_ritual_descriptive, plot_rituals_descriptive(data)),
  tar_target(p_SWB_descriptive, plot_rituals_descriptive(data)),
  tar_target(p_prosoc_descriptive, plot_prosoc_descriptive(data)),

# Reliability of instruments (cronbach's alpha) ---------------------------
  tar_target(cronbach, calculate_reliability(data)),
  # standard error of measurement
  tar_target(d_SE, data.frame(
    SE_rituals = sqrt(1 - cronbach$raw_alpha[cronbach$instrument == "rituals"]),
    SE_SWB = sqrt(1 - cronbach$raw_alpha[cronbach$instrument == "SWB"]),
    SE_prosoc = sqrt(1 - cronbach$raw_alpha[cronbach$instrument == "prosoc"])
  )),
# Statistical models ------------------------------------------------------
tar_target(d_models, d_pid |> mutate(weights = model_weights$weights, SE_SWB = d_SE$SE_SWB, SE_rituals = d_SE$SE_rituals, SE_prosoc = d_SE$SE_prosoc)),
# IPTW
tar_target(model_weights, WeightIt::weightitMSM(
  list(
    ritual_z ~ age + sex + location + edu + income,
    SWB_z ~ age + sex + location + edu + income
  ),
  data = d_pid, method = "energy", weightit.force = TRUE)),

# Hypothesis 1: Rituals -> Prosoc -----
  tar_target(h1_priors, prior(normal(0, 0.5), class = "Intercept") + prior(normal(0, 0.5), class = "b") + prior(normal(0, 1), class = "sigma") + prior(normal(0, 2), class = "alpha")),
  # no adjustment for potential confounders
  tar_target(m_h1_unadj, brms::brm(
    prosoc_z | se(SE_prosoc, sigma = TRUE) ~ me(ritual_z, SE_rituals), data = d_models, family = skew_normal(), prior = h1_priors, chains = 4, cores = 4, iter = 1e4, seed = 123, save_pars = save_pars(all = T)
  )),
  # IPTW for confounders
    tar_target(m_h1_IPTW, brms::brm(
      prosoc_z | se(SE_prosoc, sigma = TRUE) + weights(weights) ~ me(ritual_z, SE_rituals), data = d_models, family = skew_normal(), prior = h1_priors, chains = 4, cores = 4, iter = 1e4, seed = 123, save_pars = save_pars(latent = TRUE, all = TRUE)
  )),
  # regression adjustment for confounders
    tar_target(m_h1_reg, brms::brm(
    prosoc_z | se(SE_prosoc, sigma = TRUE) ~ me(ritual_z, SE_rituals) + age + sex + edu + location + income, data = d_models, family = skew_normal(), prior = h1_priors, chains = 4, cores = 4, iter = 1e4, seed = 123, save_pars = save_pars(latent = TRUE, all = TRUE)
  )),
   # IPTW + regression adjustment for confounders (double robust)
   tar_target(m_h1_double, brms::brm(
    prosoc_z | se(SE_prosoc, sigma = TRUE) + weights(weights) ~ me(ritual_z, SE_rituals) + age + sex + edu + location + income, data = d_models, family = skew_normal(), prior = h1_priors, chains = 4, cores = 4, iter = 1e4, seed = 123, save_pars = save_pars(latent = TRUE, all = TRUE)
  )),

  # Compare marginal effect of ritual participation in each model
  tar_target(h1_results, hypothesis_model_compare(model_list = list(
    unadjusted = m_h1_unadj,
    IPTW = m_h1_IPTW,
    reg = m_h1_reg,
    IPTW_reg = m_h1_double),
    hypothesis = "H1a")),

# Hypothesis 2: Rituals -> SWB, SWB -> Prosoc
  tar_target(h2_priors, priors <- prior(normal(0, 0.5), class = "Intercept", resp = "prosocz") +
    prior(normal(0, 0.5), class = "b", resp = "prosocz") + 
    prior(normal(0, 1), class = "sigma", resp = "prosocz") + 
    prior(normal(0, 2), class = "alpha", resp = "prosocz") +
    prior(normal(0, 0.5), class = "Intercept", resp = "SWBz") +
    prior(normal(0, 0.5), class = "b", resp = "SWBz") + 
    prior(normal(0, 1), class = "sigma", resp = "SWBz") + 
    prior(normal(0, 2), class = "alpha", resp = "SWBz")),

  # no adjustment for potential confounders
  tar_target(m_h2_unadj, brm(
    bf(SWB_z | se(SE_SWB, sigma = TRUE) + weights(weights) ~ me(ritual_z, SE_rituals)) + skew_normal() +
    bf(prosoc_z | se(SE_prosoc, sigma = TRUE) + weights(weights) ~ me(SWB_z, SE_SWB) + me(ritual_z, SE_rituals)) + skew_normal() + set_rescor(FALSE),
  data = d_models, prior = h2_priors, chains = 4, cores = 4, iter = 1e4, seed = 123, save_pars = save_pars(latent = TRUE, all = TRUE))),

  # IPTW for confounders
  tar_target(m_h2_IPTW, brm(
    bf(SWB_z | se(SE_SWB, sigma = TRUE) + weights(weights) ~ me(ritual_z, SE_rituals)) + skew_normal() +
    bf(prosoc_z | se(SE_prosoc, sigma = TRUE) + weights(weights) ~ me(SWB_z, SE_SWB) + me(ritual_z, SE_rituals)) + skew_normal() + set_rescor(FALSE),
  data = d_models, prior = h2_priors, chains = 4, cores = 4, iter = 1e4, seed = 123, save_pars = save_pars(latent = TRUE, all = TRUE))),

  # regression adjustment for confounders
  tar_target(m_h2_reg, brm(
    bf(SWB_z | se(SE_SWB, sigma = TRUE) ~ me(ritual_z, SE_rituals) + age + sex + edu + location + income) + skew_normal() + 
      bf(prosoc_z | se(SE_prosoc, sigma = TRUE) ~ me(SWB_z, SE_SWB) + me(ritual_z, SE_rituals) + age + sex + edu + location + income) + skew_normal() + set_rescor(FALSE),
    data = d_models, prior = h2_priors, chains = 4, cores = 4, iter = 1e4, init = "0", seed = 1, save_pars = save_pars(latent = TRUE, all = TRUE))),

  # IPTW + regression adjustment for confounders (double robust)
  tar_target(m_h2_double, brm(
    bf(SWB_z | se(SE_SWB, sigma = TRUE) + weights(weights) ~ me(ritual_z, SE_rituals) + age + sex + edu + location + income) + skew_normal() + 
    bf(prosoc_z | se(SE_prosoc, sigma = TRUE) + weights(weights) ~ me(SWB_z, SE_SWB) + me(ritual_z, SE_rituals) + age + sex + edu + location + income) + skew_normal() + set_rescor(FALSE),
    data = d_models, prior = h2_priors, chains = 4, cores = 4, iter = 1e4, seed = 123, save_pars = save_pars(latent = TRUE, all = TRUE))),

  # Compare marginal effect of ritual participation in each model
  tar_target(h2_results, hypothesis_model_compare(model_list = list(
    unadjusted = m_h2_unadj,
    IPTW = m_h2_IPTW,
    reg = m_h2_reg,
    IPTW_reg = m_h2_double),
    hypothesis = "H2a")),

# Plot overall results ---------------------------------------------------
  tar_target(p_model_results, plot_model_results(h1_results = h1_results, h2_results = h2_results)),

# Model checking ---------------------------------------------------------
 tar_target(p_model_checks, plot_model_checks(m_h2_double, d_models)),

# Render results report ---------------------------------------------------
  tar_quarto(results_report, path = "results_report.qmd")
)