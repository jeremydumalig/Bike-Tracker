library(tidyverse)
library(gsheet)
library(ggplot2)
library(ggrepel)
library(gridExtra)
rm(list=ls())

#bike_tracker <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1S5dJrkBOeJllZepWV7HAlC7nCuMXqnhjWz6L3jTSDVA/edit?usp=sharing')
bike_tracker <- read_csv(file='miles.csv')
bike_tracker$Month <- substr(bike_tracker$Date, 0, 1)
bike_tracker <- 
  bike_tracker %>%
  tibble::rowid_to_column("Index") %>%
  na.omit()

summer_days <- 106

#################################################################

gg_theme <-
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(fill = "grey90", color = "black"),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15)
  )

#################################################################

gg_daily <- 
  ggplot(data=bike_tracker, aes(x=Index, y=Miles)) +
  geom_line(
    aes(x=Index, y=Average), 
    color='gray',
    linetype='dashed', 
    size=0.75
  ) +
  geom_point(
    aes(color=factor(Month)), 
    size=3.5, 
    alpha=0.6,
    show.legend = FALSE
  ) + 
  scale_color_brewer(palette="Set1") +
  geom_line(color='#707070') +
  geom_hline(
    color='#707070',
    yintercept=pull(select(tail(bike_tracker, 1), Total)/count(bike_tracker)),
    linetype='dashed'
  ) +
  ylab("Daily Miles") +
  lims(
    x=c(0, summer_days),
    y=c(0, ceiling(max(bike_tracker$Miles)))
  ) +
  gg_theme

gg_behind <-
  ggplot(bike_tracker, aes(x=Index, y=Behind)) +
  geom_point(
    aes(color=factor(Month)), 
    size=4.5, 
    alpha=0.6,
    show.legend = FALSE
  ) + 
  scale_color_brewer(palette="Set1") +
  geom_line(color='#707070') +
  geom_hline(color='#707070', yintercept=0, linetype='dashed') +
  lims(
    x=c(0, pull(count(bike_tracker))),
    y=c(min(0, floor(min(bike_tracker$Behind))), ceiling(max(bike_tracker$Behind)))
  ) +
  gg_theme

gg_total <- 
  ggplot(data = bike_tracker, aes(x=Index, y=`Total`)) +
  geom_segment(
    x=0, y=0, 
    xend=summer_days, yend=1000, 
    linetype='dashed', 
    color='#707070'
  ) +
  geom_line(aes(x=Index, y=Pace), color='#FFD565', size=0.25) +
  geom_point(aes(x=Index, y=Pace), color='#FFD565', size=1) +
  geom_segment(
    x=0, y=1000,
    xend=summer_days, yend=1000, 
    color='green'
  ) +
  geom_line(
    aes(x=Index, y=`Total`), 
    size=0.5
  ) +
  geom_point(
    aes(color=factor(Month)), 
    size=2, 
    alpha=0.6,
    show.legend = FALSE
  ) +
  scale_color_brewer(palette="Set1") +
  ylab("Total Miles") +
  lims(
    x=c(0, summer_days),
    y=c(0, max(ceiling(max(bike_tracker$Pace)), 1000))
  ) +
  gg_theme

#################################################################

grid.arrange(arrangeGrob(gg_daily, gg_behind, ncol=2), gg_total)
grid.arrange(gg_daily, gg_total, nrow=2)

#################################################################

total_miles <- select(tail(bike_tracker, 1), Total)
total_days <- count(bike_tracker)
days_until <- ceiling((1000 -  total_miles)/(total_miles/total_days))
cat(paste0(
  'Total Miles: ', round(total_miles, 2), '\n',
  'Total Days: ', total_days, '\n',
  'Total Average: ', round(total_miles / total_days, 2), '\n', '\n',
  'Remaining Miles: ', round(1000 - total_miles, 2), '\n',
  'Remaining Days: ', summer_days - total_days, '\n',
  'Remaining Average: ', round((1000 - total_miles) / (summer_days - total_days), 2), '\n', '\n',
  'On Pace to Finish on: 9/', 28 - (summer_days - total_days - days_until), ', ',
  days_until, ' more days'
  ))
