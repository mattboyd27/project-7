# THIS IS AN EXTENSION OF cleaning.R
# Load libraries
library(tidyverse)
library(baseballr)
library(randomForest)
library(gbm)
library(caret)

# https://baseballsavant.mlb.com/catcher_framing?year=2021&team=&min=q&sort=4,1

load("Data/usable_data.Rda")


load("Data/pitchData,Rda")

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
log_reg_fit <- glm(strike ~ ., data=usable_data[train, ], family='binomial')
summary(log_reg_fit)

pred <- predict.glm(log_reg_fit, type='response')
pred[pred >= 0.4] <- 'strike'
pred[pred < 0.4] <- 'ball'
sum(pred=='strike')/length(pred)

conf_mx <- table(pred=pred, true=data$strike)
error <- (conf_mx[2] + conf_mx[3])/length(pred)

#Ridge Regression
library(glmnet)

x <- model.matrix(strike ~ ., data = usable_data)[,-1]
y <- usable_data$strike

lambda <- seq(0.001, 10, length=100)

#Create training and test data set
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
ridge_reg_fit <-  glmnet(x[train,], y[train], alpha = 0, lambda = lambda, family='binomial')

#Choose lambda
cv_ridge <- cv.glmnet(x[train, ], y[train], alpha=0, family='binomial')
plot(cv_ridge)
lambda_choice <- cv_ridge$lambda.min

#Get predictions
ridge_predict <- predict(ridge_reg_fit, s = lambda_choice, newx = x[test, ], type = 'class')

#Confusion matrix
ridge_conf_mx <- table(pred=ridge_predict, true=y[test])
ridge_conf_mx
error <- (ridge_conf_mx[2] + ridge_conf_mx[3])/length(ridge_predict)
error

#LASSO
lasso_fit <- glmnet(x[train, ], y[train], alpha = 1, lambda = lambda, family = 'binomial')
cv_lasso <- cv.glmnet(x[train, ], y[train], alpha = 1, family='binomial')
plot(cv_lasso)

#Lambda choice
lasso_lam <- cv_lasso$lambda.min

#Get predictions
lasso_predict <- predict(lasso_fit, s = lasso_lam, newx = x[test, ], type = 'class')

#Confusion matrix
lasso_mx <- table(pred=lasso_predict, true=y[test])
lasso_mx
error <- (lasso_mx[2] + lasso_mx[3])/length(lasso_predict)
error




