

# Load packages---
pacman::p_load(tidyverse, janitor, TSstudio, plotly, hrbrthemes)

#Parameters to calculate sap-flux---
wound_factor <- 0.25
thermal_cond <- 1
diam <- 30
bark_depth <- 1
sapwood_depth <- 1
dry_density_wood <- 0.7

#Set file path---
path<- "data/sap-flow/" 
path_files <- list.files(path, pattern = ".dat", full.names = T)


data <- tibble(files = path_files) %>% 
  mutate(contents = map(files, ~data.table::fread(.x, skip = 1))) 

data %>% 
  names()

unique(data$files)

#Clean data from pure plots(isolate the files)---
data %>%
  filter(str_detect(files,"Sap_Flow"))


data_pureplots <- data%>%
  filter(str_detect(files, "Sap_Flow"))%>%
  unnest(cols = contents) %>%
  mutate(
    species = case_when(
      files %in% c("data/sap-flow/WiFs_Sap_Flow_FS.dat", "data/sap-flow/WiFsCR350_Sap_Flow_FS.dat") ~ "FS",
      files == "data/sap-flow/WiPm_Sap_Flow_PM.dat" ~ "PM",
      TRUE~NA_character_),
    plot=species)%>%
  relocate(species, files,  plot, everything()) %>%
  
  
  identity()

unique(data_pureplots$species)

#This code was for the FS and PM in species col
data_pureplots$species[data_pureplots$species=='data/sap-flow/WiFs_Sap_Flow_FS.dat'] <- "FS"

data_pureplots$species[data_pureplots$species=='data/sap-flow/WiFsCR350_Sap_Flow_FS.dat'] <- "FS"

data_pureplots$species[data_pureplots$species=='data/sap-flow/WiPm_Sap_Flow_PM.dat'] <- "PM"

data_pureplots %>% 
  names()
install.packages("mgsub")
library("mgsub")

data_MFS <- data %>% 
  filter(str_detect(files, "Sap_Flow")) %>% 
  filter(str_detect(files, "FS")) %>% 
  unnest(cols = contents) %>%
  mutate(plots = mgsub::mgsub(files, c("data/sap-flow/WiFs_Sap_Flow_FS.dat", 
                                       "data/sap-flow/WiFsCR350_Sap_Flow_FS.dat", "data/sap-flow/WiPm_Sap_Flow_PM.dat"
  ),
  c("","",""))) %>% 
  separate(plots, c("plot", "species")) %>% 
  relocate(plot, species, everything())

#Further cleaning-----------
library(tidyverse)
library(lubridate)

data_pureplots_clean <- data_pureplots %>%
  # Remove invalid or header rows
  filter(Batt_volt != "Smp", TIMESTAMP != "TS") %>%
  
  # Reshape wide to long — separate variables and value columns at '_'
  pivot_longer(
    cols = -c(TIMESTAMP, plot, Batt_volt, species, files, RECORD),
    names_to = c("parameters", ".value"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  
  pivot_longer(
    cols = -c(TIMESTAMP, plot, Batt_volt, files, species, RECORD, parameters),
    names_to = "sensor",
    values_to = "resu"
  ) %>%
  
  select(-Batt_volt) %>%
  distinct() %>%
  
  # Reshape back to wide — one row per measurement per sensor
  pivot_wider(
    names_from = parameters,
    values_from = resu
  ) %>%
  ungroup() %>%
  
  mutate(
    TIMESTAMP = ymd_hms(TIMESTAMP),
    date = date(TIMESTAMP)
  ) %>%
  select(TIMESTAMP, RECORD, plot, species, date, sensor, everything()) %>%
  
  # Convert columns to numeric
  mutate(across(TotalSapFlow:Tmax_Inner, as.numeric))

max(data_pureplots_clean$TIMESTAMP)

data_pureplots_sapflow <- data_pureplots_clean %>%   #take this data frame
  clean_names() %>%                                  # then clean column names
  remove_empty(which = "cols") %>%                   # then remove empty columns
  filter(!is.na(date)) %>%                           # then keep rows where 'date' not NA
  filter(!is.na(total_sap_flow)) %>%                 # then keep rows where 'total_sap_flow' not NA
  remove_empty(which = "cols")                       # then remove empty columns again

unique(data_pureplots_sapflow$plot)
data_pureplots_sapflow %>%
  names

#Create check sensor of pure plots
check_sensor_pureplots  <- data_pureplots_sapflow %>%
  select("plot","species","sensor") %>%
  distinct_all()%>%
  group_by(plot, species)%>%
  mutate(si= n())

#Plot total sap flow over time by sensor and species (all data)
data_pureplots_sapflow %>% 
  ggplot(aes(date, total_sap_flow, col = interaction(sensor, species))) +  # x=date, y=total_sap_flow, color by sensor & species combination
  geom_path() +                                                            # Draws lines connecting observations
  facet_wrap(plot ~ sensor, scales = "free")                               # Facet plot by plot and sensor, each facet has its own scale

#Plot total sapflow over time in FS only
data_pureplots_sapflow %>% 
  filter(species == "FS") %>%
  filter(!total_sap_flow>10 ) %>% 
  filter(!total_sap_flow<  -1)%>%
  ggplot(aes(timestamp, total_sap_flow, col = interaction(sensor, species))) +
  geom_path() +
  facet_wrap(plot~sensor, scales = "free")

unique(data_pureplots_sapflow$plot)

data_pureplots_sapflow %>% 
  filter(species == "PM") %>% 
  ggplot(aes(timestamp, total_sap_flow, col = interaction(sensor, species))) +
  geom_path() +
  facet_wrap(plot~sensor, scales = "free")



unique(data_pureplots_sapflow$species)











