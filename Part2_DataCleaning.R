
# Load packages---
# pacmann loads and installs all the mentioned packages at once
pacman::p_load(tidyverse, janitor, TSstudio, plotly, hrbrthemes)

#Parameters to calculate sap-flux---
wound_factor <- 0.25
thermal_cond <- 1
diam <- 30
bark_depth <- 1
sapwood_depth <- 1
dry_density_wood <- 0.7

#Set file path-----------------------------------------------------------------
#this keeps organized and reproducable at any time and easy to read too
path<- "data/sap-flow/" 
path_files <- list.files(path, pattern = ".dat", full.names = T)

#this code reads dat files and each row = one file, with its path + its data.
data <- tibble(files = path_files) %>% 
  mutate(contents = map(files, ~data.table::fread(.x, skip = 1))) 

data %>% 
  names()

unique(data$files)

#Clean data from pure plots(isolate the files)---------------------------------
# selects stem_data files, then expands thm 2 new rows/columns, mutates and   
                                        #plot and sps will be same column
                                        #extracts characters 30–31 from the filename (likely PM/FS) 
data %>%
  filter(str_detect(files,"Stem_Data_PM"))%>%
  unnest()

data_pureplots_STEM <- data %>% 
  filter(str_detect(files, "Stem_Data")) %>% 
  unnest(cols = contents) %>% 
  mutate(
    species = str_sub(files, 17, 18),
    plot = species) %>% 
  identity()


unique(data_pureplots_STEM$plot)

#data_pureplots_STEM <- data%>%
# filter(str_detect(files, "Stem_Data"))%>%
#unnest(cols = contents) %>%
#mutate(
# species = case_when(
#  files %in% c("data/sap-flow/WiFs_Sap_Flow_FS.dat", "data/sap-flow/WiFsCR350_Sap_Flow_FS.dat") ~ "FS",
# files == "data/sap-flow/WiPm_Sap_Flow_PM.dat" ~ "PM",
#TRUE~NA_character_),
#plot=species)%>%
#relocate(species, files,  plot, everything()) %>%


# identity()

#unique(data_pureplots$species)

data_pureplots_STEM %>% 
  names()

#Process and clean the data-----------------------------------------------------
#removes rows !.., wider to longer column format of (), separates it to 
          #1st part parameter and 2nd part new variable, 
            #reshapes again as each sensor gets its own row, values go into resu
            #if data set has batt_volt col removes it, removes duplicate rows by distinct
            #widens/spreads parameters into columns, converts timestamp strings into real date-time format 
            #extracts only the date part and so onn

data_pureplots_STEM_clean <- data_pureplots_STEM %>%                            
  filter(!is.na(TIMESTAMP)) %>% 
  filter(!TIMESTAMP == "TS") %>% 
  pivot_longer(cols = -c(TIMESTAMP, plot, species, files,  RECORD),
               names_to = c("parameters", ".value"),
               names_pattern = "(.+)_(.+)") %>% 
  pivot_longer(cols = -c(TIMESTAMP, plot, files, species, RECORD, parameters),
               names_to = "sensor",
               values_to = "resu") %>%
  {if("Batt_volt" %in% names(.)) select(., -Batt_volt) else .} %>% 
  distinct() %>% 
  group_by(TIMESTAMP,files, RECORD, sensor, plot, species) %>% 
  pivot_wider(
    names_from = parameters,
    values_from = resu) %>% 
  ungroup() %>% 
  mutate(TIMESTAMP = lubridate::ymd_hms(TIMESTAMP),
         date = lubridate::date(TIMESTAMP)) %>% 
  select(TIMESTAMP, RECORD, plot, species, date, sensor, everything()) %>% 
  filter(!is.na(date))

max(data_pureplots_STEM_clean$TIMESTAMP)

names(data_pureplots_STEM_clean)

#Converts all columns from Pre_Temp_Down_Outer to Pulse_Duration into number type.
#then Replaces "NaN" values in Pre_Temp_Upper_Outer with 9999 and stores in Pre_Temp_Down_Outer.
#Removes all rows where Pre_Temp_Down_Outer = 9999.

data_pureplots_STEM_clean_1 <- data_pureplots_STEM_clean %>%
  mutate(across(Pre_Temp_Down_Outer:Pulse_Duration, as.numeric)) %>% 
  mutate(Pre_Temp_Down_Outer = ifelse(is.nan(Pre_Temp_Down_Outer) | is.na(Pre_Temp_Down_Outer), 9999, Pre_Temp_Down_Outer)) %>% 
  filter(Pre_Temp_Down_Outer != 9999)

data_pureplots_STEM_clean_1 %>% 
  ggplot(aes(TIMESTAMP, SWC_Outer)) +
  geom_line() + 
  facet_wrap(species~sensor, scales =  "free") +
  theme_bw()


data_pureplots_STEM <- data_pureplots_STEM_clean_1 %>%
  clean_names() %>%
  remove_empty(which = "cols") %>%
  filter(!is.na(date)) %>%
  remove_empty(which = "cols")


data_STEM <- data_pureplots_STEM %>%
  unite("ID", c(plot, sensor, species), sep = "_") %>%
  select(-files) %>%
  mutate(record = as.numeric(record))


exists("data_pureplots_STEM_clean_1")


# Here we need 2 Combine STEM Data with Sapflow  -------------------------------

data_STEM <- data_pureplots_STEM %>%
  unite("ID", c(plot, sensor, species), sep= "_")%>%
  select(-files)%>%
  mutate(record= as.numeric(record))

names(data_STEM)

unique(data_STEM$ID)

sf_temp <- data_STEM %>%
  filter(ID %in% c("PM_b_PM")) %>%
  select(timestamp, ID, pre_temp_down_outer, pre_temp_down_inner, max_temp_down_outer,
         pulse_energy, heat_capacity_inner, heat_capacity_outer,
         post_temp_down_inner, post_temp_down_outer) %>%
  mutate(year=year(timestamp),
         species=str_sub(ID, -2),
         doy= yday(timestamp)) %>% 
  mutate(across(c(starts_with("pre"), starts_with("post"), starts_with("max"),
                  starts_with("heat")), ~as.numeric(.))) %>% 
  filter(!(pre_temp_down_outer < -10 | pre_temp_down_outer > 45)) 

sf_temp %>% 
  ggplot(aes(timestamp, post_temp_down_inner, group = ID, col = ID)) +
  geom_line() +
  geom_line(aes(y = post_temp_down_outer), linetype = 2, alpha = 0.5, col = "black") +
  labs(y = "Sapwood Temperature (0.5 cm)") +
  theme_bw()
names(sf_temp)

data_sapflow <- read_csv("outputs/data_sapflow.csv") 

str(data_sapflow$timestamp)
str(data_STEM$timestamp)

data_STEM <- data_STEM %>%
  mutate(timestamp = lubridate::ymd_hms(timestamp))  #forcing both timestamps to be posixct



data_sapflow_comb <- left_join(data_sapflow, data_STEM)

max(data_sapflow_comb$date)
min(data_sapflow_comb$date)

data_sapflow_comb %>% 
  select(!c(record,  contains("temp"))) %>% 
  write_csv("outputs/data_sapflow_comb.csv")

data_sapflow_comb %>% names()

# Now exporting multiple csv files at each sensor level-------------------------

  data_sapflow_comb %>%
  mutate(timestamp = str_sub(timestamp, 1, 16)) %>% 
  select(ID, timestamp, alpha_outer:tmax_inner) %>% 
  filter(
    between(alpha_outer, -2, 3) | is.na(alpha_outer),
    between(alpha_inner, -2, 3) | is.na(alpha_inner),
    between(beta_outer, -2, 3) | is.na(beta_outer),
    between(beta_inner, -2, 3) | is.na(beta_inner)
  ) %>%
    #here just to check by ploting, not necessery------------
 # mutate(timestamp = lubridate::ymd_hm(timestamp)) %>%
  # This is the key step you were missing - reshape to long format
  #select(ID, timestamp, alpha_outer, alpha_inner, beta_outer, beta_inner) %>%
  #pivot_longer(cols = c(alpha_outer, alpha_inner, beta_outer, beta_inner),
   #            names_to = "variable",
    #           values_to = "value") %>%
  
  #ggplot(aes(x = timestamp, y = value, color = variable)) +
  #geom_line(alpha = 0.7) +
  #facet_wrap(~ID, scales = "free_y") +
  #labs(title = "All Alpha and Beta Values Over Time",
   #    x = "Timestamp", 
    #   y = "Values",
     #  color = "Variable") +
  #theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  nest(-ID) %>% 
  pwalk(~write_csv(x = .y, file = paste0(.x, ".csv") ))
  
# plotting rainy days in winnefeld 2025
  climate_2025_winn<- read_csv("outputs/2025_climate_winnefeld.csv")
  

  
  climate_2025_winn %>% 
    mutate(e_to_today = ifelse(is.na(e_to_today), 0, e_to_today),
           e_to_today_cum = cumsum(e_to_today),
           year = as.factor(year)) %>% 
    ggplot(aes(days_year, rain_tot, group = year)) +
    geom_col(fill = "gray") +theme_bw()
  
  
  climate_2025_winn %>% 
    mutate(
      e_to_today = ifelse(is.na(e_to_today), 0, e_to_today),
      e_to_today_cum = cumsum(e_to_today),
      year = as.factor(year),
      date = lubridate::make_date(year = as.numeric(as.character(year)), month = 1, day = 1) + lubridate::days(days_year - 1)
    ) %>% 
    ggplot(aes(date, rain_tot, group = year)) +
    geom_col(fill = "black") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x = "Month (2025)", y = "Daily precipitation (mm)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


 