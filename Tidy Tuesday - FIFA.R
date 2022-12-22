#*******************************************************************************

# Project: FIFA Tidy Tuesday.
# Name: James Bullen
# Date: 02-12-2022
# Purpose: Create plots of FIFA penalty and extra time game results.

#*******************************************************************************

# Notes.------------------------------------------------------------------------

options(scipen = 999)

# Packages.---------------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(viridis)
library(patchwork)

# Load in datasets.-------------------------------------------------------------

df <- tidytuesdayR::tt_load("2022-11-29")

wcmatches <- df$wcmatches

worldcups <- df$worldcups

# Create new variable for extra time or penalty result.------------------------

aet_df <- wcmatches %>%
  mutate(aet = if_else(str_detect(win_conditions,
                                  "AET"), "extra time", "penalties")) %>%
  filter(year > 1949)

aet_df$aet[is.na(aet_df$aet)] <- "regular"

aet_df2 <- aet_df %>%
  mutate(aet_ordered = fct_relevel(aet, levels = c("regular",
                                                   "extra time", "penalties")))

# Plot data.

plot1 <- ggplot(data = aet_df2) +
  geom_bar(mapping = aes(x = year, fill = aet_ordered),
  position = position_dodge2(width = 2.5, preserve = "single")) +
  scale_fill_viridis_d(
    labels = c("Regular match.", "In extra time.", "On penalties.")) +
  theme_minimal(base_size = 14) +
  labs(y = "Number of games.", x = "Year") +
  labs(fill = "Game result:") +
  labs(title = "Win conditions of FIFA football games, 1950 - 2018.") +
  scale_x_continuous(name = "Year", breaks = seq(1950, 2018, by = 4))

# Plot data excluding regular matches.

aet_df3 <- aet_df2 %>%
  filter(aet_ordered != "regular")

plot2 <- ggplot(data = aet_df3) +
  geom_bar(mapping = aes(x = year, fill = aet_ordered),
  position = position_dodge2(width = 2.5, preserve = "single")) +
  scale_fill_manual(
    values = c("#238A8DFF", "#FDE725FF"),
    labels = c("In extra time.", "On penalties.")) +
  theme_minimal(base_size = 14) +
  labs(y = "Number of games.", x = "Year") +
  labs(fill = "Game result:") +
  labs(title = "Win conditions excluding regular match results.") +
  scale_x_continuous(name = "Year", breaks = seq(1950, 2018, by = 4))

plot3 <- ggplot(data = aet_df2) +
  geom_bar(mapping = aes(x = year, fill = aet_ordered), position = "fill") +
  scale_fill_viridis_d(
    labels = c("Regular match.", "In extra time.", "On penalties.")) +
  theme_minimal(base_size = 14) +
  labs(y = "% of games.", x = "Year") +
  labs(fill = "Game result:") +
  labs(title = "Win conditions as proportions.") +
  scale_x_continuous(name = "Year", breaks = seq(1950, 2018, by = 4))

# Patchwork together.

fifa_patchwork <- plot1 / plot2 / plot3

fifa_patchwork
