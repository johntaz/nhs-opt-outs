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

# June - July 2021 - overall, age, sex
junJulDescriptives <- dashboardData %>% 
  filter(orgName == "England") %>% 
  arrange(date) %>%
  filter(date == "2021-05-26" | date =="2021-06-30") %>% 
  group_by(group) %>% 
  summarise(totalJun = count[1], totalJul = count[2], popJun = population[1], popJul = population[2], percJun = rate[1], percJul = rate[2], countDiff = count[2] - count[1], percChange = 100*(rate[2]/rate[1])-100) %>%
  ungroup()

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

combined <- 
  cowplot::plot_grid(agePlot, sexPlot, labels = c('A', 'B'), ncol=2, nrow=1, align = 'h')

combined
ggsave(combined, 
       filename = 
         here("output", "figs", 
              "opt-out-age-sex-combined.png"),
       width=20, height=15, units="cm")

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

plot <-junJulCCG %>%
  filter(orgName != "Unallocated") %>% 
  ggplot(aes(percJun,reorder(orgName, percJun))) +
  geom_segment(aes(yend = orgName, x = percJul, xend = percJun), colour="blue") +
  geom_point(colour="blue", fill = "blue") +
  theme_bw() +
  xlab('% Opt-out June') +
  ylab('CCGs') 

plot

# CCG before and after June/July
# Sort top 20 highest rate Before
ccgBefore_top20 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(-percJun) %>%
  slice(1:20) %>%
  mutate(twenty = "top") 

# Sort bottom 20 highest rate Before
ccgBefore_bot20 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(percJun) %>%
  slice(1:20) %>%
  mutate(twenty = "bottom")

# Combine
ccgBefore <-rbind(ccgBefore_top20, ccgBefore_bot20) %>%
  arrange(-percJun)

write_csv(ccgBefore,here("output","tables", "ccg_before.csv"))

# Sort by highest rate After
# Sort top 20 highest rate After
ccgAfter_top20 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(-percJul) %>%
  slice(1:20) %>%
  mutate(twenty = "top") 

# Sort bottom 10 highest rate After
ccgAfter_bot20 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(percJul) %>%
  slice(1:20) %>%
  mutate(twenty = "bottom")

# Combine
ccgAfter <-rbind(ccgAfter_top20, ccgAfter_bot20) %>%
  arrange(-percJul)


write_csv(ccgAfter, here("output","tables", "ccg_after.csv"))

# Sort by lowest/highest increase
# Sort top 20 highest increase
ccgIncrease_top20 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(-percChange) %>%
  slice(1:20) %>%
  mutate(twenty = "top") 

# Sort top 20 lowest increase
ccgIncrease_bot20 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(percChange) %>%
  slice(1:20) %>%
  mutate(twenty = "bottom")

ccgIncrease <- rbind(ccgIncrease_top20, ccgIncrease_bot20) %>% 
  arrange(-percChange) 

write_csv(ccgIncrease, here("output","tables", "ccg_increase.csv"))

# CCG before and after June/July
# Sort top 20 highest rate Before
ccgBefore_top20 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(-percJul) %>%
  slice(1:20) %>%
  mutate(ten = "top") 

# Sort bottom 20 highest rate Before
ccgBefore_bot20 <- junJulCCG %>% 
  filter(!is.na(popJul)) %>%
  arrange(percJul) %>%
  slice(1:20) %>%
  mutate(ten = "bottom")

ccg_20 <- rbind(ccgBefore_bot20,ccgBefore_top20)

plot <-ccg_20 %>%
  filter(orgName != "Unallocated") %>% 
  mutate(orgName = str_sub(orgName, 1, str_length(orgName)-3)) %>% 
  mutate(twenty = ifelse(ten == "bottom", "Lowest 20 opt-outs", "Highest 20 opt-outs")) %>% 
  ggplot(aes(percJul,reorder(orgName, percJul), size = percChange, col = twenty)) +
  geom_point() +
  theme_bw() +
  xlab('Opt-out %') +
  ylab('Clinical commissioning group (CCG)') +
  geom_hline(yintercept = 20.5, lty = 2) +
  scale_x_continuous(breaks = seq(0, 12, by = 3), limits = c(0,12)) +
  labs(size ="Increase (%)", color = "Rank" ) +
  scale_color_manual(breaks = c('Lowest 20 opt-outs', 'Highest 20 opt-outs'), values = c('#DC9549', '#5954D6'))

plot

ggsave(plot, 
       filename = 
         here("output", "figs", 
              "ccg-top-20.png"),
       width=25, height=25, units="cm")
