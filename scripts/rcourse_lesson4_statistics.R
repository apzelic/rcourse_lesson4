## READ IN DATA ####
source("scripts/rcourse_lesson4_cleaning.R")


## LOAD PACKAGES ####


## ORGANIZE THE DATA for modelling purposes ####

data_stats <- data_clean %>%
    mutate(series = factor(series, levels = c("tos", "tng")))

## CREATE MODELS ####

# Does the series predict extinction?
extinct_series.glm <- glm(extinct ~ series, family = "binomial", data= data_stats)
extinct_series.glm_sum <- summary(extinct_series.glm)

# Does alignment with the Federation predit extinction?
extinct_alignment.glm <-glm(data = data_stats, extinct ~ alignment, family = "binomial")
extinct_alignment.glm_sum <-summary(extinct_alignment.glm)

# Two variable additive
extinct_seriesalignment.glm <- glm(extinct ~ series + alignment, family = "binomial", data = data_stats)
extinct_seriesalignment.glm_sum <- summary(extinct_seriesalignment.glm)

# Two variable interaction model
extinct_seriesXalignment.glm <- glm(extinct ~ series * alignment, family = "binomial", data = data_stats)
extinct_seriesXalignment.glm_sum <-summary(extinct_seriesXalignment.glm)

# Two variables interaction (CHANGE BASELINE for series to tng)
extinct_seriesXalignment_tng.glm = glm(extinct ~ relevel(series, "tng") * alignment, family = "binomial", data = data_stats)

extinct_seriesXalignment_tng.glm_sum = summary(extinct_seriesxalignment_tng.glm)

#Two variables interaction (CHANGE BASELINE for alignment to friend)
extinct_seriesXalignment_friend.glm = glm(extinct ~ series * relevel(alignment, "friend"), family=binomial,
                                          data = data_stats)
extinct_seriesXalignment_friend.glm_sum = summary(extinct_seriesXalignment_friend.glm)



extinct_seriesXalignment.glm_sum
extinct_seriesXalignment_tng.glm_sum
extinct_seriesXalignment_friend.glm_sum


