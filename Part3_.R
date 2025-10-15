
#Total sap flow calculation


# 1.load packages

# Load packages
pacman::p_load(tidyverse, janitor, TSstudio, plotly, hrbrthemes, conflicted, readxl)


conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
install.packages("readxl")
library("readxl")

# 2.LOAD DATA

probe_correction <- read_excel("data/2025_probe_correction_winn.xlsx") %>% 
  mutate(date = lubridate::ymd(date))

probe_correction %>% 
  names() 
hi my name is bharath and iam don
