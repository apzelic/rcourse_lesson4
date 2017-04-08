## READ IN DATA FROM THE 'clean' DATA FILE ####
source("scripts/rcourse_lesson4_cleaning.R")

## LOAD PACKAGES ####
library(ggplot2)

## ORGANIZE THE DATA FOR FIGURES ####

data_figs = data_clean %>%
    mutate(series = factor(series, levels = c("tos", "tng"),
                           labels = c("The Original Series", "The Next Generation")))

# Now this call will summarize across extinction

data_figs_sum = data_figs %>%
    group_by(series, alignment) %>%
    summarise(perc_extinct = mean(extinct) *100) %>%
    ungroup()


extinct.plot = ggplot(data_figs_sum, aes(x = series, y = perc_extinct, fill = alignment)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylim(0, 100) +
    geom_hline(yintercept = 50) +
    scale_fill_manual(values = c("red", "gold")) +
    # Add a title
    ggtitle("Percentage of Possibly Extinct Aliens\nby Series and Alignment") +
    xlab("Star Trek Series") + 
    ylab("Percentage of species\nlikely to become extinct") + 
    # Remove the grey background
    theme_classic() +
    theme(text=element_text(size=14), title=element_text(size = 18),
          legend.position = "top") +
    # Center the title (I found this by searching)
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.title.y = element_text(face = "italic")) +
    theme(axis.title.x = element_text(face = "italic")) +
    theme(axis.text.x = element_text(face = "italic"))


pdf("figures/extinct.pdf")
extinct.plot
dev.off()




