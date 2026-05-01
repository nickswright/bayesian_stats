# 02b - Add categorical covariates (binary year, and predator identity) to the vigilance model

library(jagsUI)
library(tibble)

vigilance <- read.csv("data/bayesian_vigilance_dataframe_demo.csv")
head(vigilance)

# Scale tree cover
vigilance$tree_cover_scaled <- as.numeric(scale(vigilance$tree_cover))

# Adjust vigilance so it can't equal 0 or 1
vigilance$proportion_vigilant_adjusted <- pmin(
  pmax(vigilance$proportion_vigilant, 0.001),
  1 - 0.001
)

# Specify reference level for predator cue
vigilance$predator_cue <- relevel(factor(vigilance$predator_cue), ref = "Cheetah")
# Convert to numeric
vigilance$predator_cue_numeric <- as.numeric(vigilance$predator_cue)

# Change year to binary dummy variable (2021 = 0, 2024 = 1)
vigilance$year_binary <- ifelse(vigilance$year == 2024, 1, 0)

# Prepare data list for JAGS
jags_data_input <- list(
  VIGILANCE = vigilance$proportion_vigilant_adjusted,
  TREE_COVER = vigilance$tree_cover_scaled,
  YEAR = vigilance$year_binary,
  PREDATOR_ID = vigilance$predator_cue_numeric,
  
  N_PRED = length(unique(vigilance$predator_cue)),
  N_OBS = nrow(vigilance)
)

# Specify parameters to save
jags_parameters_output <- c("b0", 
                            "b_tree",
                            "b_year",
                            "b_pred")

# Run model
model_out <- jagsUI::jags(
  model.file = "vigilance-tree-predator-year.txt",
  data = jags_data_input,
  parameters.to.save = jags_parameters_output,
  inits = NULL,
  n.chains = 3,
  n.iter = 5000,
  n.burnin = 1000,
  n.thin = 3,
  parallel = T
)

# Explore model output

# Trace plots and R-hat values
traceplot(model_out)

MCMCvis::MCMCplot(
  model_out, 
  params = c("b_tree",
             "b_year",
             "b_pred")
)

# Look at model summary
model_out$summary

# Tidy model output (get into a data frame)
res_sum <- model_out$summary %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  tibble::as_tibble()
names(res_sum) <- c("variable", "mean", "sd", "LCI", "Q1", "Q2", "Q3", "UCI", "Rhat", "n.eff", "significant", "f")
