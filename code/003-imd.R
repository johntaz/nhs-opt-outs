# -----------------------------------------------------------------------------
# PROGRAM NAME:   003-imd
# PROJECT:        nhs-opt-outs
# AUTHOR:         John Tazare
# -----------------------------------------------------------------------------

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

# Postcode level data -----------------------------------------------------------
postCodeOpt <- read_csv(here("data", "NDOP_reg_geo_Sep_2021.csv")) %>% 
  rename(postcode = 'POSTCODE', 
         optout = 'OPT_OUT',
         population = 'LIST_SIZE',
         practice = 'GP_PRACTICE',
         practice_name = "PRACTICE_NAME"
  )  %>%
  mutate(optout = as.numeric(optout),
        population = as.numeric(population),
        date = dmy(ACH_DATE),
        rate = 100*optout/population) %>% 
  select(postcode, date, optout, population, practice, practice_name, rate) %>%
  filter(date =="2021-07-01" & 
           practice_name!="Unallocated" & 
           !is.na(optout))

postcode <- postCodeOpt %>% 
  group_by(postcode) %>% 
  slice(1) 

# Several practices have the same postcode

# Summary 6,587 practices (6,588 - 1 row which is 'unallocated')
# 15 practices = missing opt-out/population data
# TOTAL = 6,572

# Load postcode imd data
#https://imd-by-postcode.opendatacommunities.org/imd/2019

postCodeIMD <- read_csv(here("data", "2019-deprivation-by-postcode.csv")) %>%
  rename(postcode = 'Postcode', 
         imdRank = 'Index of Multiple Deprivation Rank',
         imdDec = 'Index of Multiple Deprivation Decile') %>%
  select(postcode, imdRank, imdDec) %>% 
  filter(postcode!="POSTCODE")
  
# Join datasets
postCodeOpt2 <- postCodeOpt %>% 
  left_join(postCodeIMD) %>%
  filter(!is.na(imdRank)) # n= 2

# Plot IMD Rank vs Opt out (with loess smoothed fit curve)
plotRank2 <- ggplot(postCodeOpt2, aes(x = imdRank, y = rate)) +
  geom_point()+
  geom_smooth(method = "loess") +
  xlab('IMD Rank') +
  ylab('Opt-out % (End of June 2021)') +
  scale_fill_discrete('') +
  scale_color_discrete('') +
  scale_y_continuous(breaks=c(0, 50, 100, 140)) +
  theme(strip.text = element_text(colour ='black')) +
  theme_classic() 

plotRank2

 # save
ggsave(plotRank2, 
       filename = 
         here("output", "figs", 
              "opt-out-postcode-IMDRank.png"),
       width=20, height=15, units="cm")

# Join datasets + remove practices with large opt-out %
postCodeOpt3 <- postCodeOpt %>% 
  left_join(postCodeIMD)  %>% filter(rate<=10) %>%
  filter(!is.na(imdRank)) # n= 2

# Plot IMD Rank vs Opt out (with loess smoothed fit curve)
plotRank3 <- ggplot(postCodeOpt3, aes(x = imdRank, y = rate)) +
  geom_point()+
  geom_smooth(method = "loess") +
  xlab('IMD Rank') +
  ylab('Opt-out % (End of June 2021)') +
  scale_fill_discrete('') +
  scale_color_discrete('') +
  #scale_x_continuous(breaks=c(0, 50, 100, 150, 200)) +
  theme(strip.text = element_text(colour ='black')) +
  theme_classic() 

plotRank3

# save
ggsave(plotRank3, 
       filename = 
         here("output", "figs", 
              "opt-out-postcode-IMDRank-less-than10.png"),
       width=20, height=15, units="cm")

# Plot IMD Decile vs Opt out (with loess smoothed fit curve)
plotDecile <- ggplot(postCodeOpt2, aes(x = imdDec, y = rate)) +
  geom_point()+
  #geom_smooth(method = "loess") +
  xlab('IMD Rank') +
  ylab('Opt-out % (End of June 2021)') +
  scale_fill_discrete('') +
  scale_color_discrete('') +
  #scale_x_continuous(breaks=c(0, 50, 100, 150, 200)) +
  theme(strip.text = element_text(colour ='black')) +
  theme_classic() 

plotDecile

# save
ggsave(plotDecile, 
       filename = 
         here("output", "figs", 
              "opt-out-postcode-IMDDec.png"),
       width=20, height=15, units="cm")


combined <- 
  cowplot::plot_grid(plotRank2, plotRank3, labels = c('A', 'B'), ncol=2, nrow=1)

combined
ggsave(combined, 
       filename = 
         here("output", "figs", 
              "opt-out-postcode-imd-combined.png"),
       width=20, height=15, units="cm")

summ <- postCodeOpt2 %>% 
  group_by(imdDec) %>% 
  summarise(mean = mean(rate), median = median(rate), min = min(rate), max = max(rate))

# N = 4 with missing imdDec at postcode level
# N = 2 with missing imdRank at postcode level