# 05 - Add species hierarchy AND site-level random intercepts to the flight model

library(jagsUI)
library(tibble)

flight <- read.csv("data/bayesian_flight_dataframe_demo.csv")
head(flight)

# Specify reference level for predator cue
flight$predator_cue <- relevel(factor(flight$predator_cue), ref = "Cheetah")
# Convert to numeric
flight$predator_cue_numeric <- as.numeric(as.factor(flight$predator_cue))

# Change year to binary dummy variable (2021 = 0, 2024 = 1)
flight$year_binary <- ifelse(flight$year == 2024, 1, 0)

# Convert prey species to numeric
flight$species_numeric <- as.numeric(as.factor(flight$species))

# Create a new variable that is a combo of site & prey (for intercept indexing)
flight$SITE_PREY_ID <- with(
    flight,
    as.numeric(interaction(as.numeric(as.factor(flight$site)), 
                           species_numeric, 
                           drop = TRUE))
)

# Create key for which PREY_ID go with which SITE_PREY_ID
SITE_PREY_PREY_ID <- aggregate(species_numeric ~ SITE_PREY_ID, flight, unique)$species_numeric

# Prepare data list for JAGS
jags_data_input <- list(
    
    FLIGHT_PRESENT = flight$flight_present,
    YEAR = flight$year_binary,
    PREDATOR_ID = flight$predator_cue_numeric,
    SITE_ID = as.numeric(factor(flight$site)),
    PREY_ID = as.numeric(factor(flight$species)),
    SITE_PREY_ID = flight$SITE_PREY_ID,
    SITE_PREY_PREY_ID = SITE_PREY_PREY_ID,
    
    N_SITE = length(unique(flight$site)),
    N_PRED = length(unique(flight$predator_cue)),
    N_PREY = length(unique(flight$species)),
    N_SITE_PREY = length(unique(flight$SITE_PREY_ID)),
    N_OBS = nrow(flight)
)

# Specify parameters to save
jags_parameters_output <- c("b0_site", 
                            "b_year",
                            "b_pred")

# Run model
model_out <- jagsUI::jags(
    model.file = "flight-predator-year-RE-species.txt",
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
    params = c("b_year",
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









