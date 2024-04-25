#Preliminaries 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, modelsummary, kableExtra, broom, cobalt, fixest)

#Load Datasets

final.data <- read_tsv('data/output/acs_medicaid.txt') 

#Share of Adult Pop. w/ Direct Purchase Insurance

final.data <- final.data %>%
  group_by(year) %>%
  summarise(total_adult_pop = sum(adult_pop),
            total_direct_ins = sum(ins_direct)) %>%
  mutate(share_direct_ins = total_direct_ins / total_adult_pop)

#Create Graph

direct.graph <- ggplot(final.data, aes(x = year, y = share_direct_ins)) +
  geom_line() + geom_point() + theme_classic() +
  labs(
    x = "Year", 
    y = "Proportion of Direct Purchase Insurance", 
    title = "Share of Direct Purchase Health Insurance Over Time"
       )

direct.graph

#Share of Adult Pop. w/ Medicaid

final.data <- read_tsv('data/output/acs_medicaid.txt')

final.data <- final.data %>% 
    ungroup() %>%
  mutate(share_medicaid = ins_medicaid / adult_pop)

medicaid.graph <- final.data %>% group_by(year) %>% summarize(mean=mean(share_medicaid)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_classic() +
  labs(
    x="Year",
    y="Proportion of Medicaid",
    title="Adult Population with Medicaid over Time"
  )

medicaid.graph

#Share of Uninsured

ins.plot.dat <- final.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop) %>%
  group_by(expand_ever, year) %>% 
  summarize(mean=mean(perc_unins))

#Create Graph

uninsured.graph <- ggplot(data=ins.plot.dat, aes(x=year,y=mean,group=expand_ever,linetype=expand_ever)) + 
  geom_line() + geom_point() + theme_classic() +
  geom_text(data = ins.plot.dat %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype="none") +
  labs(
    x="Year",
    y="Proportion Uninsured",
    title="Adult Population Uninsured over Time"
  )

uninsured.graph

#Difference in Differences 

medicaid.dd <- final.data %>% 
  filter(is.na(expand_year) | expand_year==2014) %>%
  filter(year %in% c(2012, 2015)) %>%  
  group_by(expand_ever, year)

#Create Table

dd.table <- pivot_wider(medicaid.dd, names_from="year", names_prefix="year", values_from="uninsured") %>% 
  ungroup() %>%
  mutate(expand_ever=case_when(
    expand_ever==FALSE ~ 'Non-expansion',
    expand_ever==TRUE ~ 'Expansion')
  ) %>%
  rename(Group=expand_ever,
         Pre=year2012,
         Post=year2015)

dd.table

#Reread Medicaid Data

mcaid.data <- read_tsv("data/output/acs_medicaid.txt")

reg.dat <- mcaid.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

#First Reg.

dd.reg <- lm(perc_unins ~ post + expand_ever + post*expand_ever, data=reg.dat)

#Second Reg.

mcaid.data <- read_tsv("data/output/acs_medicaid.txt")

reg.dat <- mcaid.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

dd.reg2 <- lm(perc_unins ~ post + expand_ever + treat, data=reg.dat)

twfe <- feols(perc_unins ~ treat | State + year, data=reg.dat)

#Third Regression

mcaid.data <- read_tsv("data/output/acs_medicaid.txt")

reg.dat <- mcaid.data %>% filter(!is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

dd.reg3 <- lm(perc_unins ~ post + expand_ever + treat, data = reg.dat)

twfe2 <- feols(perc_unins ~ treat | State + year, data = reg.dat)

#Event Study One

reg.dat2 <- mcaid.data %>% 
  filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

mod.twfe <- feols(perc_unins~i(year, expand_ever, ref=2013) | State + year,
                  cluster=~State,
                  data=reg.dat2)

#Event Study Two

reg.dat2 <- reg.dat2 %>%
  mutate(time_to_treat=ifelse(expand_ever==TRUE, year-expand_year, -1),
         time_to_treat=ifelse(time_to_treat<=-4, -4, time_to_treat))

mod.twfe2 <- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=reg.dat2)

save.image("submission1/hw5_workspace.Rdata")
