# -----------------------------------------------------------------------------
# PROGRAM NAME:   001-trend-plot
# PROJECT:        nhs-opt-outs
# AUTHOR:         John Tazare
# -----------------------------------------------------------------------------

# Load libraries --------------------------------------------------------------
list <- c("tidyverse", "lubridate", "readxl", "here", "rgdal", "broom", "viridis", "cowplot")
new.packages <- list[!(list %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(rgdal)
library(broom)
library(viridis)
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

# Google trends
googleTrends <- read_csv(here("data", "googleTrends.csv"))

# Overall Opt-out trend  ------------------------------------------------------
# Clean dashboard data
overalTrend <- dashboardData %>% 
  filter(group == "All"  & orgName == "England") %>% 
  arrange(date) %>%
  mutate(diff = count - lag(count, default = first(count))) %>%
  mutate(perc = 100* diff / population) %>% 
  filter(date >= "2021-05-12" & date <="2021-11-01")

# Plot dashboard data
plot <- overalTrend %>% 
  ggplot(aes(date, perc)) +
  geom_line(size=1.5, colour ="navy") + 
  xlab("Date (1st of Month)") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y") + 
  ylab("") + 
  theme_classic() + 
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(fill = "white", colour = NA)
  ) +
  geom_vline(xintercept = as.numeric(as.Date("2021-05-12")), 
             color = "black", 
             lwd = 0.7, 
             linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(as.Date("2021-06-23")), 
             color = "black", 
             lwd = 0.7, 
             linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2021-09-25")), 
             color = "black", 
             lwd = 0.7, 
             linetype = "dashed") +
  scale_y_continuous("Weekly Opt-Out %") +
  geom_text(x=as.numeric(as.Date("2021-05-27")), y=0.965, label="GPDPR Launch") +
  geom_text(x=as.numeric(as.Date("2021-07-08")), y=0.965, label="1st Deadline") +
  geom_text(x=as.numeric(as.Date("2021-10-10")), y=0.965, label="2nd Deadline") 
  

plot

# Clean google trend data
googleTrends <- googleTrends %>% 
  mutate(perc = interest/100) %>% 
  mutate(date = dmy(Week)) %>% 
  mutate(date = date + 3) %>% # to align weeks with  opt-out data
  filter(date >= "2021-05-12" & date <="2021-11-01") 
  

# Add google trends
trendPlot <- plot + geom_line(data = googleTrends, aes(x = date, y = perc),  
                   color = "gold", 
                   lwd = 1.5
) +
  scale_y_continuous(
    "Weekly Opt-Out %", 
    sec.axis = sec_axis(~ . * 100, name = "Google Trend Interest") 
  ) 
trendPlot
# save
ggsave(plot, 
       filename = 
         here("output", "figs", 
               "opt-out-trends.png"),
       width=20, height=14, units="cm")




