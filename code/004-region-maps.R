# -----------------------------------------------------------------------------
# PROGRAM NAME:   004-region-maps
# PROJECT:        nhs-opt-outs
# AUTHOR:         John Tazare
# -----------------------------------------------------------------------------
list <- c("tidyverse", "rgdal", "broom", "here", "lubridate", "cowplot")
new.packages <- list[!(list %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

library(tidyverse)
library(rgdal)
library(broom)
library(here)
library(lubridate)
library(cowplot)

# Load data ---------------------------------------------------------------------
# Opt-outs
dashboardData <- read_csv(here("data", "2021-dashboard.csv")) %>% 
  mutate(date = dmy(Date)) %>% 
  rename(group = 'Demographic Group', 
         onsCode = 'ONS Code',
         orgCode = 'Org Code',
         orgName = 'Org Name',
         rate = 'Opt Out Rate',
         population = 'Population',
         count = 'Opt-Out Count'
  )  %>%
  select(group, onsCode, orgCode, orgName, rate, population, count, date)

## 1. NHS Region ---------------------------------------------------------------------
# Data 
list <- c("North East and Yorkshire", "North West", "East of England", "Midlands", "South East", "South West", "London")

junJulRegion <- dashboardData %>% 
  filter(orgName %in% list) %>% 
  arrange(date) %>%
  filter(date == "2021-05-26" | date =="2021-06-30") %>% 
  mutate(NHSER22NM = orgName) %>%
  group_by(NHSER22NM) %>% 
  summarise(totalJun = count[1], totalJul = count[2], popJun = population[1], popJul = population[2], percJun = rate[1], percJul = rate[2], countDiff = count[2] - count[1], percChange = 100*(rate[2]/rate[1])-100) %>%
  ungroup()

junJulRegion2 <- junJulRegion %>%
  select(NHSER22NM, percChange)

# NHS region shapefile
localNHS <- readOGR(dsn = here("data"), layer = "NHSER_JUL_2022_EN_BFC")

glimpse(localNHS)

localNHSTidy <- tidy(localNHS)

plot <- ggplot(localNHSTidy, aes(x = long, y= lat, group =group)) +   
  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
  coord_equal() +
  theme_minimal()

plot

localNHS$id <- row.names(localNHS)

localNHSTidy <- left_join(localNHSTidy, localNHS@data) %>% 
  mutate(nhs = as.character(NHSER22NM)) #CCG21NM or CCG21CD

a <- localNHSTidy %>% 
  group_by(NHSER22NM) %>%
  count()

localNHSTidy <- left_join(localNHSTidy, junJulRegion2)
  
plotRegion <- ggplot(localNHSTidy, aes(x = long, y= lat, group =group, fill = percChange)) +   
  geom_polygon(color = "black", size = 0.1) +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(margin = margin(t = 40, b = -40))) +
  scale_fill_gradient(low='#35AD68', high='blue', limits=c(50, 110), name="Increase (%)") +
  theme(
    text = element_text(color = "#22211d"), 
    #plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    #panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    #legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.2, 0.50)
  ) 

plotRegion

# save
ggsave(plotRegion, 
       filename = 
         here("output", "figs", 
              "opt-out-region.png"),
       width=15, height=20, units="cm")

## 2. CCG  ---------------------------------------------------------------------
# Data 
list <- c("England","Unallocated", "North East and Yorkshire", "North West", "East of England", "Midlands", "South East", "South West", "London")

junJulCCG <- dashboardData %>% 
  filter(!orgName %in% list) %>% 
  arrange(date) %>%
  filter(date == "2021-05-26" | date =="2021-06-30") %>% 
  mutate(ccg = orgName) %>%
  group_by(ccg) %>% 
  summarise(totalJun = count[1], totalJul = count[2], popJun = population[1], popJul = population[2], percJun = rate[1], percJul = rate[2], countDiff = count[2] - count[1], percChange = 100*(rate[2]/rate[1])-100) %>%
  ungroup()

junJulCCG2 <- junJulCCG %>%
  select(ccg, percChange)

junJulCCG3 <- junJulCCG %>%
  select(ccg, percJul)

# CCG shapefile
localCCG <- readOGR(dsn = here("data"), layer = "Clinical_Commissioning_Groups_(April_2021)_EN_BUC")

glimpse(localCCG)

localCCGTidy <- tidy(localCCG)

plot <- ggplot(localCCGTidy, aes(x = long, y= lat, group =group)) +   
  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
  coord_equal() +
  theme_minimal()

plot

localCCG$id <- row.names(localCCG)

localCCGTidy <- left_join(localCCGTidy, localCCG@data) %>% 
  mutate(ccg = as.character(CCG21NM)) #CCG21NM or CCG21CD

# 1. percentage changee
localCCGTidy2 <- left_join(localCCGTidy, junJulCCG2) %>%
  mutate(percChange = ifelse(ccg=="NHS Herts Valleys CCG", 69.84204, percChange)) %>% 
  mutate(percChange = ifelse(ccg=="NHS Herefordshire and Worcestershire CCG", 65.90978, percChange)) 
  
plotCCG <- ggplot(localCCGTidy2, aes(x = long, y= lat, group =group, fill = percChange)) +   
  geom_polygon(color = "black", size = 0.1) +
  coord_equal() +
  theme_void() +
  scale_fill_gradient(low='white', high='blue', limits=c(10, 220), name="Increase (%)") +
  theme(
    text = element_text(color = "#22211d"), 
    #plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    #panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    #legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.background = element_rect(fill = "#FFFFFF", color = NA), 
    panel.background = element_rect(fill = "#FFFFFF", color = NA), 
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    #plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    #plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.2, 0.50)
  ) 

plotCCG

# save
ggsave(plotCCG, 
       filename = 
         here("output", "figs", 
              "opt-out-ccg-increase.png"),
       width=15, height=15, units="cm")


# Compare CCG data
ccgCompare <- localCCGTidy %>% 
  group_by(ccg) %>% 
  slice(1)  %>% 
  ungroup() %>%
  select(ccg) %>% 
  mutate(ccgTidy = 1)

ccgCompare2 <- junJulCCG2 %>%
  group_by(ccg) %>% 
  slice(1) %>% 
  select(ccg, percChange) %>%
  ungroup() %>% 
  mutate(optout = 1) %>% 
  left_join(ccgCompare)

# No data for NHS Shropshire, Telford and Wrekin CCG

# 2. highest rate july

localCCGTidy3 <- left_join(localCCGTidy, junJulCCG3) %>%
  mutate(percJul = ifelse(ccg=="NHS Herts Valleys CCG", 3.795164, percJul)) %>% 
  mutate(percJul = ifelse(ccg=="NHS Herefordshire and Worcestershire CCG", 2.476188, percJul)) 

plotJulyCCG <- ggplot(localCCGTidy3, aes(x = long, y= lat, group =group, fill = percJul)) +   
  geom_polygon(color = "black", size = 0.1) +
  coord_equal() +
  theme_void() +
  scale_fill_gradient(low='white', high='blue', limits=c(0, 12), name="Opt-Outs (%)") +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#FFFFFF", color = NA), 
    panel.background = element_rect(fill = "#FFFFFF", color = NA), 
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    legend.position = c(0.2, 0.50)
  ) 

plotJulyCCG

# save
ggsave(plotJulyCCG, 
       filename = 
         here("output", "figs", 
              "opt-out-ccg-july.png"),
       width=15, height=15, units="cm")

combined <- 
  cowplot::plot_grid(plotCCG, plotJulyCCG, labels = c('A', 'B'), ncol=2, nrow=1)

combined
ggsave(combined, 
       filename = 
         here("output", "figs", 
              "opt-out-ccg-combined.png"),
       width=20, height=15, units="cm")
