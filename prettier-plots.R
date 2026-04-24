# These need to be run after the model has been fitted and 
# the summary data frame `res_sum` has been created

# It specifically shows the effects of year and predator cues for each prey species


library(dplyr)
library(stringr)
library(ggplot2)


prey_lookup <- tibble::tibble(
    PREY_ID = seq_along(levels(as.factor(flight$species))),
    PREY = levels(as.factor(flight$species))
)

pred_lookup <- tibble::tibble(
    PREDATOR_ID = seq_along(levels(flight$predator_cue)),
    PREDATOR = levels(flight$predator_cue)
)


b_year_df <- res_sum %>%
    filter(str_detect(variable, "^b_year\\[")) %>%
    mutate(
        PREY_ID = as.integer(str_extract(variable, "\\d+"))
    ) %>%
    left_join(prey_lookup, by = "PREY_ID")


ggplot(b_year_df,
       aes(x = mean, y = reorder(PREY, mean))) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2) +
    labs(
        x = "Year effect (log-odds scale)",
        y = "Prey species",
        title = "Year effect by prey species"
    ) +
    theme_bw()


b_pred_all <- res_sum %>%
    filter(str_detect(variable, "^b_pred\\[")) %>%
    mutate(
        PREY_ID = as.integer(str_extract(variable, "(?<=\\[)\\d+")),
        PREDATOR_ID = as.integer(str_extract(variable, "(?<=,)\\d+"))
    ) %>%
    left_join(prey_lookup, by = "PREY_ID") %>%
    left_join(pred_lookup, by = "PREDATOR_ID") %>% 
    filter(PREDATOR != "Cheetah")

ggplot(
    b_pred_all,
    aes(
        x = mean,
        y = PREY,
        colour = PREDATOR
    )
) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(
        position = position_dodge(width = 0.6),
        size = 2
    ) +
    geom_errorbarh(
        aes(xmin = LCI, xmax = UCI),
        position = position_dodge(width = 0.6),
        height = 0.25
    ) +
    labs(
        x = "Predator effect (log-odds scale)",
        y = "Prey species",
        colour = "Predator",
        title = "Predator effects on flight, by prey species"
    ) +
    theme_bw()


# colored by prey

ggplot(
    b_pred_all,
    aes(
        x = mean,
        y = PREDATOR,
        colour = PREY
    )
) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(
        position = position_dodge(width = 0.6),
        size = 2
    ) +
    geom_errorbarh(
        aes(xmin = LCI, xmax = UCI),
        position = position_dodge(width = 0.6),
        height = 0.25
    ) +
    labs(
        x = "Predator effect (log-odds scale)",
        y = "Predator species",
        colour = "Prey",
        title = "Predator effects on flight, by prey species"
    ) +
    theme_bw()



