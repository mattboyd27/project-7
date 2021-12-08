# THIS IS AN EXTENSION OF cleaning.R
# Load libraries
library(tidyverse)
library(baseballr)
library(randomForest)
library(gbm)
library(caret)

# https://baseballsavant.mlb.com/catcher_framing?year=2021&team=&min=q&sort=4,1

load("Data/usable_data.Rda")

# Cross validation
k = 3
folds = sample(1:k, nrow(usable_data), replace = T)
rf = data.frame()

for(i in 1:k) {
  print(paste("Fold = ", i))
  
  train = usable_data[folds != i, ] %>%
    select(-catcher_name)
  
  test = usable_data[folds == i, ] %>%
    select(-catcher_name)
  
  model = randomForest(strike ~ ., data = train, mtry = round(sqrt(8)))
  
  accuracy = mean(predict(model, test) == test$strike)
  
  rf = rf %>%
    bind_rows(data.frame(method = "rf", accuracy = accuracy))
}


df_rf = data.frame()

data = usable_data %>%
  select(-catcher_name) 

vector = createDataPartition(data$strike, p = 0.7, list = F) %>% as.numeric()

data = data[vector, ]

# Takes 64 minutes to run 5 models
time = proc.time()
for(i in seq(10, 90, 20)) {
  print(paste("Nodesize = ", i))
  
  model_rf = randomForest(strike ~ ., data = data, mtry = round(sqrt(ncol(data) - 1)),
                       nodesize = i)
  
  rf = (model_rf$confusion[1,1] + model_rf$confusion[2,2]) /
    (sum(model_rf$confusion[,1]) + sum(model_rf$confusion[,2]))
  
  df_rf = df_rf %>%
    bind_rows(data.frame(model = "rf", accuracy = rf, nodesize = i))
  
}
proc.time() - time

ggplot(df_rf, aes(x = nodesize, y = accuracy))+
  geom_line() +
  geom_point() +
  ylim(0.925, 0.935)


varImp(model_rf) %>% arrange(desc(Overall))


# mean(as.numeric(as.character(data$strike)))
# 
# usable_data = usable_data %>%
#   mutate(strike_prob = predict(model, usable_data, type = "prob")[,2],
#          predicted_strike = predict(model, usable_data),
#          strike = as.numeric(as.character(strike)),
#          predicted_strike = as.numeric(as.character(predicted_strike)))
# 
# usable_data = usable_data %>%
#   mutate(credit = case_when(
#     strike == 1 ~ 1 - strike_prob,
#     strike == 0 ~ strike_prob * -1),
#     credit2 = credit - mean(credit))
# 
# usable_data %>%
#   group_by(catcher_name) %>%
#   summarize(total =  sum(strike) / sum(predicted_strike),
#             n = n()) %>%
#   arrange(desc(total))
# 
# 
# 
# usable_data %>%
#   group_by(catcher_name) %>%
#   summarize(strikes = sum(credit2),
#             n = n(),
#             strike_100 = strikes / (n/100)) %>%
#   filter(n > 2000) %>%
#   arrange(-strike_100)
# 
# ggplot() +
#   geom_histogram(aes(x = usable_data$credit))
# 
# 
# factors <- c('pitch_type', 'stand', 'p_throws', 'location', 'outs_when_up', 'inning')
# 
# 
# data = data %>%
#   mutate_at(factors, as.factor) %>%
#   drop_na(plate_x)
# data  = data %>%
#   mutate(strike_prob = predict(model, data, type = "prob")[,2])
# 
# data %>% filter(strike == 0) %>%
#   select(game_date, count, inning, catcher_name, home_team, release_speed, strike_prob) %>%
#   arrange(desc(strike_prob))


# perform Boosting
data = data %>%
  mutate(strike = as.character(strike))
test = usable_data[-vector, ] %>%
  mutate(strike = as.character(strike))


boosting = gbm(strike ~ ., data = data, distribution = "bernoulli")

mean(as.numeric(test$strike) == (predict(boosting, test, type = "response") >= 0.5))

k = 3
folds = sample(1:k, nrow(usable_data), replace = T)

df_gbm = data.frame()

usable_data = usable_data %>%
  mutate(strike = as.character(strike))

time = proc.time()
for(i in 1:k){
  print(paste("k =",i))
  
  train = usable_data[folds != i, ] %>%
    select(-catcher_name)
  
  test = usable_data[folds == i, ] %>%
    select(-catcher_name)
  
  for(d in seq(6, 16, 5)){
    print(paste("interaction =", d))
    for(lambda in c(0.1, 0.01, 0.001)) {
      print(paste("lambda =", lambda))
      boosting = gbm(strike ~ ., data = train, distribution = "bernoulli", 
                     interaction.depth = d,
                     n.trees = 500, shrinkage = lambda)
      gbm = mean(as.numeric(test$strike) == (predict(boosting, test, type = "response") >= 0.5))
      
      df_gbm = df_gbm %>%
        bind_rows(data.frame(model = "boosting", accuracy = gbm, interaction_depth = d, lambda = lambda))
    }
  }
}

# 5.7 hours
total_time = proc.time() - time

save(df_gbm_final, file = "Model-Outputs/boosting.rda")
save(df_rf, file = "Model-Outputs/random-forest.Rda")

options(pillar.sigfig = 7)
df_gbm_final = df_gbm %>%
  group_by(interaction_depth, lambda) %>%
  summarize(accuracy = mean(accuracy)) %>%
  arrange(desc(accuracy))

df = bind_rows(df_rf %>% rename(parameter = nodesize),
               df_gbm %>% rename(parameter = interaction_depth)) %>%
  group_by(model, parameter) %>%
  summarise(accuracy = round(mean(accuracy), 7))%>%
  arrange(desc(accuracy)) 



