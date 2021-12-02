# Load libraries
library(tidyverse)
library(baseballr)

#setup
# devtools::install_github(repo = "BillPetti/baseballr")

# data description
# https://baseballsavant.mlb.com/csv-docs

# Scrape all pitches from 2021 season
data1=scrape_statcast_savant(start_date="2021-03-28",end_date="2021-04-05",player_type="batter")
data2=scrape_statcast_savant(start_date="2021-04-06",end_date="2021-04-13",player_type="batter")
data3=scrape_statcast_savant(start_date="2021-04-14",end_date="2021-04-20",player_type="batter")
data4=scrape_statcast_savant(start_date="2021-04-21",end_date="2021-04-28",player_type="batter")
data5=scrape_statcast_savant(start_date="2021-04-29",end_date="2021-05-06",player_type="batter")
data6=scrape_statcast_savant(start_date="2021-05-07",end_date="2021-05-14",player_type="batter")
data7=scrape_statcast_savant(start_date="2021-05-15",end_date="2021-05-22",player_type="batter")
data8=scrape_statcast_savant(start_date="2021-05-23",end_date="2021-05-29",player_type="batter")
data9=scrape_statcast_savant(start_date="2021-05-30",end_date="2021-06-06",player_type="batter")
data10=scrape_statcast_savant(start_date="2021-06-07",end_date="2021-06-15",player_type="batter")
data11=scrape_statcast_savant(start_date="2021-06-16",end_date="2021-06-23",player_type="batter")
data12=scrape_statcast_savant(start_date="2021-06-24",end_date="2021-06-29",player_type="batter")
data13=scrape_statcast_savant(start_date="2021-06-30",end_date="2021-07-07",player_type="batter")
data14=scrape_statcast_savant(start_date="2021-07-08",end_date="2021-07-15",player_type="batter")
data15=scrape_statcast_savant(start_date="2021-07-16",end_date="2021-07-23",player_type="batter")
data16=scrape_statcast_savant(start_date="2021-07-24",end_date="2021-07-29",player_type="batter")
data17=scrape_statcast_savant(start_date="2021-07-30",end_date="2021-08-07",player_type="batter")
data18=scrape_statcast_savant(start_date="2021-08-08",end_date="2021-08-15",player_type="batter")
data19=scrape_statcast_savant(start_date="2021-08-16",end_date="2021-08-23",player_type="batter")
data20=scrape_statcast_savant(start_date="2021-08-24",end_date="2021-09-01",player_type="batter")
data21=scrape_statcast_savant(start_date="2021-09-02",end_date="2021-09-10",player_type="batter")
data22=scrape_statcast_savant(start_date="2021-09-11",end_date="2021-09-18",player_type="batter")
data23=scrape_statcast_savant(start_date="2021-09-19",end_date="2021-09-26",player_type="batter")
data24=scrape_statcast_savant(start_date="2021-09-27",end_date="2021-10-04",player_type="batter")

data=rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10, data11,data12,
           data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24)

rm(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10, data11,data12,
   data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24)


# Link catcher IDs to their name
player_ids = data.frame(get_chadwick_lu()) %>%
  select(name_first, name_last, key_mlbam) %>%
  unite("catcher_name", name_first, name_last, sep = " ")



# Unite with data
data = data %>% 
  
  # Join the player ID data frame from above to find the catcher name
  left_join(player_ids, by = c("fielder_2" = "key_mlbam")) %>%
  
  # We only care about a ball or a called strike
  filter(description %in% c("called_strike", "ball")) %>%
  
  # Make binary column if the pitch was a strike or not
  mutate(strike = ifelse(description == "called_strike", 1, 0),
         strike = as.factor(strike))  %>%
  
  # Some pitches are observed to be thrown on counts with 4 balls or 3 strikes. That's an error
  filter(balls != "4", strikes != "3") %>%
  
  # Combine balls and strikes to create one column
  unite("count", balls, strikes, sep = "-") %>%
  mutate(count = as.factor(count),
         # Create less classes for each pitch type
         pitch_type = case_when(
           pitch_type %in% c("FF", "FA") ~"Fastball",
           pitch_type %in% c("SI") ~ "Sinker",
           pitch_type %in% c("CH", "FS") ~ "Changeup",
           pitch_type %in% c("CU", "CS", "KC") ~ "Curveball",
           pitch_type %in% c("SL") ~ "Slider",
           pitch_type %in% c("FC") ~ "Cutter",
           TRUE ~ NA_character_)) %>%
  drop_na(pitch_type)

#Remove pitch location NAs
data %>%
  drop_na(plate_x, plate_z) -> data

#save data
save(data, file="Data/pitchData.Rda")

#Remove irrelevant columns
bad_cols <- c('game_date', 'player_name', 'batter', 'pitcher', 'events', 'description', 'spin_dir', 
              'spin_rate_deprecated', 'break_angle_deprecated', 'break_length_deprecated', 'des', 
              'hit_location', 'bb_type', 'on_3b', 'on_2b', 'on_1b', 'hc_x', 'hc_y', 'tfs_deprecated', 
              'tfs_zulu_deprecated', 'umpire', 'sv_id', 'hit_distance_sc', 'launch_speed', 'launch_angle', 
              'estimated_ba_using_speedangle', 'estimated_woba_using_speedangle', 'woba_value','woba_denom', 
              'babip_value', 'iso_value', 'launch_speed_angle', 'pitch_name', 'type')
usable_data <- data[ , !(names(data) %in% bad_cols)]
usable_data[,'pitch_type'] <- as.factor(usable_data[,'pitch_type'])
usable_data[,'zone'] <- as.factor(usable_data[,'zone'])
usable_data[,'game_type'] <- as.factor(usable_data[,'game_type'])
usable_data[,'stand'] <- as.factor(usable_data[,'stand'])
usable_data[,'p_throws'] <- as.factor(usable_data[,'p_throws'])
usable_data[,'pitch_type'] <- as.factor(usable_data[,'pitch_type'])
usable_data[,'home_team'] <- as.factor(usable_data[,'home_team'])
usable_data[,'away_team'] <- as.factor(usable_data[,'away_team'])
usable_data[,'game_year'] <- as.factor(usable_data[,'game_year'])
usable_data[,'pitch_type'] <- as.factor(usable_data[,'pitch_type'])
usable_data[,'outs_when_up'] <- as.factor(usable_data[,'outs_when_up'])
usable_data[,'inning'] <- as.factor(usable_data[,'inning'])
usable_data[,'inning_topbot'] <- as.factor(usable_data[,'inning_topbot'])

#save as better_data
save(usable_data, file='Data/better_data.Rda')

