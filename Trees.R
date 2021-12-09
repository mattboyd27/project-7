# THIS IS AN EXTENSION OF cleaning.R
# Load libraries
library(tidyverse)
library(baseballr)
library(randomForest)
library(gbm)
library(caret)

# https://baseballsavant.mlb.com/catcher_framing?year=2021&team=&min=q&sort=4,1

load("Data/usable_data.Rda")

load("Data/pitchData.Rda")

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

data1 = usable_data %>%
  select(-catcher_name) 

vector = createDataPartition(data1$strike, p = 0.7, list = F) %>% as.numeric()

data1 = data[vector, ]

# Takes 64 minutes to run 5 models
time = proc.time()
for(i in seq(10, 90, 20)) {
  print(paste("Nodesize = ", i))
  
  model_rf = randomForest(strike ~ ., data = data1, mtry = round(sqrt(ncol(data) - 1)),
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



# Boosting
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

options(pillar.sigfig = 7)
df_gbm_final = df_gbm %>%
  group_by(interaction_depth, lambda) %>%
  summarize(accuracy = mean(accuracy)) %>%
  arrange(desc(accuracy))

ggplot(df_gbm_final, aes(x = interaction_depth, y = accuracy, color = as.character(lambda))) +
  geom_line()


save(df_gbm_final, file = "Model-Outputs/boosting.Rda")
save(df_rf, file = "Model-Outputs/random-forest.Rda")


# Trees CV
k = 3
folds = sample(1:k, nrow(usable_data), replace = T)

final_df_gbm = data.frame()

usable_data = usable_data %>%
  mutate(strike = as.character(strike))

time = proc.time()
for(i in 1:k){
  print(paste("k =",i))
  
  train = usable_data[folds != i, ] %>%
    select(-catcher_name)
  
  test = usable_data[folds == i, ] %>%
    select(-catcher_name)
  
  for(trees in seq(300, 1200, 300)){
    print(paste("trees =", trees))
    boosting = gbm(strike ~ ., data = train, distribution = "bernoulli", 
                   interaction.depth = 16,
                   n.trees = trees, 
                   shrinkage = 0.1)
      
    gbm = mean(as.numeric(test$strike) == (predict(boosting, test, type = "response") >= 0.5))
      
    final_df_gbm = final_df_gbm %>%
      bind_rows(data.frame(model = "boosting", accuracy = gbm, trees = trees))
    
  }
}
total_time2 = proc.time() - time

# Add boosting with 500 trees
final_df_gbm = final_df_gbm %>% 
  bind_rows(df_gbm %>%
              filter(interaction_depth == 16, lambda == 0.1) %>%
              mutate(trees = 500) %>%
              select(model, accuracy, trees))

save(final_df_gbm, file = "Model-Outputs/boosting2.Rda")

final_df_gbm %>%
  group_by(trees) %>%
  summarize(accuracy = mean(accuracy)) %>%
  arrange(desc(accuracy)) %>%
  ggplot(aes(x = trees, y = accuracy)) +
  geom_line(color = "goldenrod") +
  theme_minimal() +
  labs(x = "Number of Trees",
       y = "Accuracy",
       title = "Boosting Accuracy by # of Trees")


save(final_df_gbm, file = "Model-Outputs/boosting2.Rda")


# Final model
data1 = usable_data %>%
  select(-catcher_name) %>%
  mutate(strike = as.character(strike))

vector = createDataPartition(data1$strike, p = 0.7, list = F) %>% as.numeric()

data1 = data1[vector, ] 

  

final_model = gbm(strike ~ ., distribution = "bernoulli", 
                  interaction.depth = 16,
                  n.trees = 500, shrinkage = 0.1, data = data1)

# Find video examples
data = data %>%
  mutate(strike_prob = predict(final_model, data, type = "response"),
         strike = as.numeric(as.character(strike)))

data %>% filter(strike == 1, game_date != "2021-08-22") %>%
  select(game_date, count, inning, catcher_name, home_team, release_speed, strike_prob, strike) %>%
  arrange(desc(strike_prob))


# Combine all models
df = bind_rows(df_rf %>% rename(parameter = nodesize),
               final_df_gbm %>% rename(parameter = trees)) %>%
  group_by(model, parameter) %>%
  summarise(accuracy = round(mean(accuracy), 7))%>%
  arrange(desc(accuracy)) 




# Analysis
data = data %>%
  mutate(strike_prob = predict(final_model, data, type = "response"),
         strike = as.numeric(as.character(strike)))

data = data %>%
  mutate(credit = case_when(
    strike == 1 ~ 1 - strike_prob,
    strike == 0 ~ strike_prob * -1),
    credit2 = credit - mean(credit))

data %>%
  group_by(catcher_name) %>%
  summarize(total =  sum(strike) / sum(predicted_strike),
            n = n()) %>%
  arrange(desc(total))



data %>%
  group_by(catcher_name) %>%
  summarize(strikes = sum(credit),
            n = n(),
            strike_100 = strikes / (n/100)) %>%
  filter(n > 2000) %>%
  arrange(strikes)

ggplot() +
  geom_histogram(aes(x = usable_data$credit))


factors <- c('pitch_type', 'stand', 'p_throws', 'location', 'outs_when_up', 'inning')


data = data %>%
  mutate_at(factors, as.factor) %>%
  drop_na(plate_x)
data  = data %>%
  mutate(strike_prob = predict(model, data, type = "prob")[,2])



