#-------------
# References -
#-------------

# https://observablehq.com/@johnburnmurdoch
# https://towardsdatascience.com/how-to-do-that-animated-race-bar-chart-57f3a8ff27a8
# https://github.com/keithmcnulty/english_football_league/blob/master/R/animation.R

#----------------------------
# Loading Required Packages -
#----------------------------

library(tidyverse)
library(gganimate)
library(gifski)
library(png)
library(lubridate)
library(data.table)
theme_set(theme_classic())

#----------------------
# Setting up the Data -
#----------------------

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks")
file_list <- list.files("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks", pattern = ".csv")

historic_rank.dt <- data.table()
i = 1
for (i in 1:length(file_list)) {
  temp <- fread(file_list[i])
  historic_rank.dt <- rbind(historic_rank.dt, temp)
}
rm(temp)

table <- historic_rank.dt[Rank <= 25, ]
table <- table[wday(date)==1,]

# generate top n ranking by year group

anim_table <- table %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = min_rank(-`Bayes average`) * 1,
    Value_rel = `Bayes average` / `Bayes average`[rank == 1],
    Value_lbl = paste0(" ", `Bayes average`)
  ) %>%
  dplyr::filter(rank <= 25) %>%
  dplyr::ungroup()

# create animated barchart

p <- ggplot2::ggplot(anim_table, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = `Bayes average` / 2,
    height = `Bayes average`,
    width = 0.9,
    fill = "blue"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(ID, " ")), size = 12, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = `Bayes average`, label = Value_lbl, hjust = 0)) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "{closest_state}", x = "", y = "Geek Score",
    caption = "Source:  Board Game Geek & Github(beefsack/bgg_ranking_historicals) | Rank based on Bayes Average (Psuedo Geek Score) | Plot originally by @dr_keithmcnulty"
  ) +
  ggplot2::theme(
    plot.title = element_text(color = "darkblue", face = "bold", hjust = 0, size = 30),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "darkgray", face = "bold", hjust = 0, size = 12),
    plot.caption =element_text(color = "darkgray", face = "bold", hjust = 0, size = 8),
    plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 8, state_length = 2) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard")
gganimate::animate(p, 200, fps = 10, duration = 220, width = 2000, height = 1200, renderer = gifski_renderer("Rank Over Time 2.gif"))
