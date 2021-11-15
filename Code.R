# Load libraries
library(tidyverse)
library(baseballr)
library(randomForest)
library(gbm)

# data
# https://baseballsavant.mlb.com/csv-docs

# This is the data scraped below just saved on my computer
load("~/DSCI 445/DSCI-445-Project/data2021.RData")
#


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

# Link player IDs to their name
playerid_lookup()

player_ids = data.frame(chadwick_player_lu_table) %>%
  select(name_first, name_last, key_mlbam) %>%
  unite("catcher_name", name_first, name_last, sep = " ")

# Get umpire names
umpires = get_umpire_ids_petti() %>% 
  select(id, name) %>% 
  group_by(id, name) %>% 
  summarize(n = n()) %>% 
  filter(n == max(n)) %>% 
  select(id, name) %>%
  ungroup()

umpires_game = get_umpire_ids_petti() %>% filter(position == "HP") %>%
  select(id, position, game_pk, game_date) %>% 
  left_join(umpires, by = c("id")) %>% 
  rename(umpire = name)

# Unite with data
data1 = data %>% left_join(player_ids, by = c("fielder_2" = "key_mlbam")) %>%
  left_join(umpires_game, by = c("game_pk")) %>%
  filter(description %in% c("called_strike", "ball")) %>%
  mutate(strike = ifelse(description == "called_strike", 1, 0),
         strike = as.factor(strike)) %>%
  drop_na(umpire.y) %>%
  mutate(strike = case_when(
    description == "called_strike" ~ 1,
    TRUE ~ 0),
    strike = as.factor(strike))


# Littlestrike zone path
x=c(-0.95,0.95,0.95,-0.95,-0.95)
y=c(1.5,1.5,3.5,3.5,1.5)
sz=data.frame(x,y)

# Example model
  # Take some samples
train = data1 %>% slice(1000:10000) 

# Train initial model
model = randomForest(strike ~ plate_x + plate_z, data = train, ntree = 1000)


# Create a grid of values
x = seq(-3, 3, length.out = 200)
y = seq(0, 5, length.out = 200)
grid = expand.grid(plate_x = x, plate_z = y)


# Predict prob
grid$pred = predict(model, grid, type = "prob")[,2]


# Visualize
ggplot() +
  geom_point(data = grid, aes(x = plate_x, y = plate_z, color = pred))+
  geom_path(data = sz, aes (x = x, y = y)) +
  scale_color_manual(values = c("1" = "red", "0" = "white"))



