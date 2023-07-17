# -----------------------------------------------------------------------------
# PROGRAM NAME:   002-summary-stats
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


# Opt-out Descriptives------------------------------------------------------

# Overall by age, sex-------------------------------------------------------

# June - July 2021 - overall, age, sex
junJulDescriptives <- dashboardData %>% 
  filter(orgName == "England") %>% 
  arrange(date) %>%
  filter(date == "2021-05-26" | date =="2021-06-30") %>% 
  group_by(group) %>% 
  summarise(totalJun = count[1], totalJul = count[2], popJun = population[1], popJul = population[2], percJun = rate[1], percJul = rate[2], countDiff = count[2] - count[1], percChange = 100*(rate[2]/rate[1])-100) %>%
  ungroup()

# Age plot------------------------------------------------------------------
# Age
junJulAgeSex1 <- junJulDescriptives %>% 
  select(group, percJun, percJul) %>% 
  mutate(flag = "End of May")

junJulAgeSex2 <- junJulDescriptives %>% 
  select(group, percJun, percJul) %>% 
  mutate(flag = "End of June")

junJulAgeSex <- rbind(junJulAgeSex1, junJulAgeSex2) %>%
  mutate(optOut = ifelse(flag == "End of May", percJun, percJul))

list <- c("All", "Female", "Male", "Unknown")
ageTrend <- junJulAgeSex %>% 
  filter(!group %in% list) %>% 
  rename(Age = group) %>%
  rename(Month = flag) 

ageTrend$Month <- factor(ageTrend$Month, levels=c("End of May", "End of June"))

group.colors <- c("End of May" = "#5954D6", "End of June" = "#DC9549")

agePlot <- ageTrend %>% 
  ggplot(aes(fill=Month, y=optOut, x=Age)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic() + 
  ylab('Total Opt-Out %') + 
  #Specify colours
  scale_fill_manual(values=group.colors) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle=45,hjust=1))

agePlot

# Sex plot------------------------------------------------------------------
# Sex
list <- c("Female", "Male")
sexTrend <- junJulAgeSex %>% 
  filter(group %in% list) %>% 
  rename(Sex = group) %>%
  rename(Month = flag) 

sexTrend$Month <- factor(sexTrend$Month, levels=c("End of May", "End of June"))

sexPlot <- sexTrend %>% 
  ggplot(aes(fill=Month, y=optOut, x=Sex)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic() + 
  ylab('') + 
  #Specify colours
  scale_fill_manual(values=group.colors)

sexPlot

# Combine plots & save--------------------------------------------------------
combined <- 
  cowplot::plot_grid(agePlot, sexPlot, labels = c('A', 'B'), ncol=2, nrow=1, align = 'h')

combined
ggsave(combined, 
       filename = 
         here("output", "figs", 
              "opt-out-age-sex-combined.png"),
       width=20, height=15, units="cm")

# CCG-------------------------------------------------------------------------

# June - July 2021 - ccg (Unallocated included for table but not plot)
list <- c("England", "North East and Yorkshire", "North West", "East of England", "Midlands", "South East", "South West", "London")
junJulCCG <- dashboardData %>% 
  filter(!orgName %in% list) %>% 
  arrange(date) %>%
  filter(date == "2021-05-26" | date =="2021-06-30") %>% 
  group_by(orgName) %>% 
  summarise(totalJun = count[1], totalJul = count[2], popJun = population[1], popJul = population[2], percJun = rate[1], percJul = rate[2], countDiff = count[2] - count[1], percChange = 100*(rate[2]/rate[1])-100) %>%
  ungroup()

# June - July 2021 - regions
list <- c("North East and Yorkshire", "North West", "East of England", "Midlands", "South East", "South West", "London")

junJulRegion <- dashboardData %>% 
  filter(orgName %in% list) %>% 
  arrange(date) %>%
  filter(date == "2021-05-26" | date =="2021-06-30") %>% 
  mutate(NHSER22NM = orgName) %>%
  group_by(NHSER22NM) %>% 
  summarise(totalJun = count[1], totalJul = count[2], popJun = population[1], popJul = population[2], percJun = rate[1], percJul = rate[2], countDiff = count[2] - count[1], percChange = 100*(rate[2]/rate[1])-100) %>%
  ungroup()

## Save
# Overall, age, sex before and after June/July
write_csv(junJulDescriptives, here("output","tables", "overall_before_after.csv"))

# CCG before and after June/July
# Sort top 10 highest rate Before
ccgBefore_top10 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(-percJun) %>%
  slice(1:10) %>%
  mutate(ten = "top") 

# Sort bottom 10 highest rate Before
ccgBefore_bot10 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(percJun) %>%
  slice(1:10) %>%
  mutate(ten = "bottom")

# Combine
ccgBefore <-rbind(ccgBefore_top10, ccgBefore_bot10) %>%
  arrange(-percJun)

write_csv(ccgBefore,here("output","tables", "ccg_before.csv"))

# Sort by highest rate After
# Sort top 10 highest rate After
ccgAfter_top10 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(-percJul) %>%
  slice(1:10) %>%
  mutate(ten = "top") 

# Sort bottom 10 highest rate After
ccgAfter_bot10 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(percJul) %>%
  slice(1:10) %>%
  mutate(ten = "bottom")

# Combine
ccgAfter <-rbind(ccgAfter_top10, ccgAfter_bot10) %>%
  arrange(-percJun)


write_csv(ccgAfter, here("output","tables", "ccg_after.csv"))

# Sort by lowest/highest increase
# Sort top 10 highest increase
ccgIncrease_top10 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(-percChange) %>%
  slice(1:10) %>%
  mutate(ten = "top") 

# Sort top 10 lowest increase
ccgIncrease_bot10 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(percChange) %>%
  slice(1:10) %>%
  mutate(ten = "bottom")

ccgIncrease <- rbind(ccgIncrease_top10, ccgIncrease_bot10) %>% 
  arrange(-percChange) 

write_csv(ccgIncrease, here("output","tables", "ccg_increase.csv"))

