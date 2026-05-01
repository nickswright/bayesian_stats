# 02 - Add categorical covariates (binary year, and predator identity) to the flight model

library(jagsUI)
library(tibble)

flight <- read.csv("data/bayesian_flight_dataframe_demo.csv")
head(flight)

# Scale tree cover
flight$tree_cover_scaled <- as.numeric(scale(flight$tree_cover))
    
# Specify reference level for predator cue
flight$predator_cue <- relevel(factor(flight$predator_cue), ref = "Cheetah")
# Convert to numeric
flight$predator_cue_numeric <- as.numeric(flight$predator_cue)

# Change year to binary dummy variable (2021 = 0, 2024 = 1)
flight$year_binary <- ifelse(flight$year == 2024, 1, 0)

# Prepare data list for JAGS
jags_data_input <- list(
    
    FLIGHT_PRESENT = flight$flight_present,
    TREE_COVER = flight$tree_cover_scaled,
    YEAR = flight$year_binary,
    PREDATOR_ID = flight$predator_cue_numeric,
    
    N_PRED = length(unique(flight$predator_cue)),
    N_OBS = nrow(flight)
)

# Specify parameters to save
jags_parameters_output <- c("b0", 
                            "b_tree",
                            "b_year",
                            "b_pred")

# Run model
model_out <- jagsUI::jags(
    model.file = "flight-tree-predator-year.txt",
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
