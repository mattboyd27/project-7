# THIS IS AN EXTENSION OF cleaning.R
# Load libraries
library(tidyverse)
library(baseballr)
library(randomForest)
library(gbm)

load("Data/pitchData.Rda")

# Little strike zone path for ggplot
x=c(-0.95,0.95,0.95,-0.95,-0.95)
y=c(1.5,1.5,3.5,3.5,1.5)
sz=data.frame(x,y)



# Example model
# Take some samples to explore
train = data %>% slice(1000:50000)

# Train initial model
model = randomForest(strike ~ plate_x + plate_z + count, data = train, ntree = 1000)


# Create a grid of values
grid1 = data.frame()
x = seq(-2, 2, length.out = 150)
y = seq(1, 5, length.out = 150)
grid = expand.grid(plate_x = x, plate_z = y)

# This creates a new grid for every count factor to visualize
grid1 = grid1 %>% bind_rows(data.frame(grid, count = "0-0"),
                            data.frame(grid, count = "0-1"),
                            data.frame(grid, count = "0-2"),
                            data.frame(grid, count = "1-0"),
                            data.frame(grid, count = "1-1"),
                            data.frame(grid, count = "1-2"),
                            data.frame(grid, count = "2-0"),
                            data.frame(grid, count = "2-1"),
                            data.frame(grid, count = "2-2"),
                            data.frame(grid, count = "3-0"),
                            data.frame(grid, count = "3-1"),
                            data.frame(grid, count = "3-2")) %>%
  mutate(count = as.factor(count))


# Predict whether a pitch will be a strike or not 
grid1 = grid1 %>% 
  mutate(pred = predict(model, grid1))

# Visualize strikes by the count and location
ggplot()+
  geom_point(data = grid1, aes(x = plate_x, y = plate_z, color = pred)) +
  facet_wrap(~stand) +
  geom_path(data = sz, aes(x = x, y = y)) 


# Find the pitch that is called a strike with the lowest probability. Among observations we trained model on
train$pred = predict(model, train, type = "prob")[,2]

train %>% filter(strike == 1, count == "0-2") %>%
  select(game_date.x, count, inning, catcher_name, home_team, pred) %>%
  arrange(pred)



#Nathan's model
log_reg_fit <- glm(strike ~ as.factor(pitch_type) + count + plate_x + plate_z, data=na.omit(data), family='binomial')
summary(log_reg_fit)

raw_pred <- predict.glm(log_reg_fit, type='response')
pred[raw_pred >= 0.4] <- 'strike'
pred[raw_pred < 0.4] <- 'ball'
sum(pred=='strike')/length(pred)

table(pred=pred, true=data$strike)
length(pred)
length(data$strike)
sum(is.na(data$strike))
