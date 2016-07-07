library(ggplot2)
library(dplyr)
library(Metrics)
library(tidyr)

# let's briefly remind ourselves what polynomials look like
x <- 1:1000
par(mfrow = c(2, 3)) # this allows multiple base R plots side by side
poly(x, degree = 6) %>% apply(2, plot)
par(mfrow = c(1, 1)) # good to remember to set this back

# now let's split a dataset into test/train
df <- mpg %>% filter(class == 'suv')
set.seed(2000)
train_idx <- sample.int(nrow(df), size = floor(nrow(df) / 2))
train <- df[train_idx,]
test <- df[-train_idx,]

# we can use the linear models ggplot creates to quickly
# get a sense of what overfitting looks like
header <- "Highway MPG vs Engine Displacement (suv class)"
p <- train %>% ggplot(aes(x = displ, y = hwy)) + geom_point() + ggtitle(header)
p
p + geom_smooth(method = "lm", se = F, formula = y ~ x)
p + geom_smooth(method = "lm", se = F, formula = y ~ poly(x, degree = 1)) # same
p + geom_smooth(method = "lm", se = F, formula = y ~ poly(x, degree = 2))
p + geom_smooth(method = "lm", se = F, formula = y ~ poly(x, degree = 3))
p + geom_smooth(method = "lm", se = F, formula = y ~ poly(x, degree = 4))
p + geom_smooth(method = "lm", se = F, formula = y ~ poly(x, degree = 5))
p + geom_smooth(method = "lm", se = F, formula = y ~ poly(x, degree = 6))
p + geom_smooth(method = "lm", se = F, formula = y ~ poly(x, degree = 7))

# let's create a simple model for real and check out the train and test errors
lm1 <- lm(hwy ~ displ, data = train) # now we have to name the x and y of course
pred_train <- predict(lm1, train)
pred_test <- predict(lm1, test)
actual_train <- train$hwy
actual_test <- test$hwy

# train error
rmse(actual_train, pred_train)
# test error
rmse(actual_test, pred_test)

# view fit against test data
test %>% ggplot(aes(x = displ, y = hwy)) + geom_point() + 
  geom_abline(intercept = coef(lm1)[1], slope = coef(lm1)[2], col = "red")

# okay, can we use the flexibility of polynomial terms to fit the model better?
# and when will we know when we've over fit?
deg <- c()
train_rmse <- c()
test_rmse <- c()

mpg_poly_model_fit <- function(poly_degree) {
  lm0 <- lm(hwy ~ poly(displ, degree = poly_degree), data = train)
  train_err <- rmse(train$hwy, predict(lm0, train))
  test_err <- rmse(test$hwy, predict(lm0, test))
  c(deg = poly_degree, train_rmse = train_err, test_rmse = test_err)
}

results <- sapply(1:7, mpg_poly_model_fit) %>% 
  t %>% 
  data.frame %>%
  gather(measure, error, -deg)

results %>% ggplot(
  aes(x = deg, y = error, group = measure, color = measure)
  ) + geom_line(stat = "identity")

# fourth degree seems to have a good bias-variance tradeoff- best test data fit


# Appendix ----------------------------------------------------------------

# what a polynomial regression looks like
lm(hwy ~ poly(displ, degree = 4), data = train) %>% summary
# basically it's finding a, b, c, .. for ax^(n) + bx^(n-1) + cx^(n-2)...
