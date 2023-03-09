#------------------------------------------------------------------------
## Predicting the Price of a “Small” Crypto-Currency 
## Using Twitter-Based Sentiment Indicators. The Case of Optimism

# Alessandro Ciancetta, Alessandro Tenderini, Nikita Baklazhenko
#------------------------------------------------------------------------

## Load relevant packages
library(tidyverse)
library(dynlm)
library(ggthemes)

## Load data
d_raw <- read.csv("Data/tweets_op_sentiment.csv")
d_price <- read.csv("Data/optimism.csv") %>% rename(date = dates)

## Prepare data
d_sentiment <- d_raw %>% 
  separate(col = "date", into = c("date", "time"), sep = " ") %>% 
  select(date, sent = Sentiment, sent_vader = Sentiment_dif) %>% 
  group_by(date) %>% 
  summarise(sent1 = mean(sent), sent2 = mean(sent_vader))
  
d <- d_price %>%
  left_join(d_sentiment)

## Remove last two observations (missing sentiment)
d <- d[1:264,] 
N <- nrow(d)


## EDA ----
## Convert into ts
inds <- seq(as.Date(d[1,1]), as.Date(d[N,1]), by = "day")
d.ts <- ts(d, 
           start=c(2022, as.numeric(format(inds[1], "%j"))), 
           frequency = 365)

## Plots
plot.ts(d.ts[,c(3,4,5)], main = "", xlab="")
plot.ts(diff(d.ts[,"prices"]))

par(mfrow=c(1,3))
# acf(d.ts[,"prices"])
acf(diff(d.ts[,"prices"]), lag.max = 14, main = "Price change")
acf(diff(d.ts[,"sent1"]), lag.max = 14, main = "Sentiment - Index 1")
acf(diff(d.ts[,"sent2"]), lag.max = 14, main = "Sentiment - Index 2")

## A correlation exists
cor(d.ts[,"prices"], d.ts[,"sent1"])
cor(diff(d.ts[,"prices"]), diff(d.ts[,"sent1"]))

cor(d.ts[,"prices"], d.ts[,"sent2"])
cor(diff(d.ts[,"prices"]), diff(d.ts[,"sent2"]))


## All the lines used in the following part are either active or commented
## Uncomment the appropriate lines (and adjust the syntax) 
## to get the output without the sentiment indexes

## Distributed Lag Model and Expanding window forecast evaluation ---- 
## Parameters
p <- 14           # autoregressive order
s <- 7            # sentiment order
s2 <- 7           # sentiment^2 order
n_train0 <- n_train <- 220   # number of observation in the initial training set
ground_truth <- d[(n_train+1):N, "prices"]

count <- 1
preds <- c()

while(n_train < N){
  
  
  ## Train - Test split
  d_train_matrix <- scale(d[1:n_train,-1])
  d_train <- as.data.frame(d_train_matrix)
  
  train_mean <- attr(d_train_matrix, "scaled:center")
  train_sd <- attr(d_train_matrix, "scaled:scale")
  d_test_unstand <- sweep(d[(n_train+1):N,-1],2, train_mean, "-")
  d_test <- sweep(d_test_unstand, 2, train_sd, "/")
  
  ## converting into ts object
  inds <- seq(as.Date(d[1,1]), as.Date(d[n_train,1]), by = "day")
  cat("\rThe time span of the train is", d[1,1], "-", d[n_train,1])
  d_train.ts <- ts(d_train, 
             start=c(as.numeric(format(inds[1], "%Y")), 
                     as.numeric(format(inds[1], "%j"))), 
             frequency = 365)
  
  ## Fitting the model
  m_dlm <- dynlm(d(prices) ~ 
                   # L(market_caps, 1:p) + 
                   L(d(prices), 1:p) + 
                   L(sent1, 1:s) + L(sent2, 1:s),
                   ## Quadratic terms didn't show useful in prediction
                   # I(L(market_caps, 1:p)^2) + I(L(d(prices), 1:p)^2) + 
                   # I(L(sent1, 1:s2)^2) + I(L(sent2, 1:s2)^2), 
                 data=d_train.ts)

  beta <- coef(m_dlm)
  xT <- c(1,
          # d_train.ts[,"market_caps"][(n_train-1):(n_train-p)],
          diff(d_train.ts[,"prices"])[(n_train-1):(n_train-p)],
          d_train.ts[,"sent1"][(n_train-1):(n_train-s)],
          d_train.ts[,"sent2"][(n_train-1):(n_train-s)])#,
         # (d_train.ts[,"market_caps"][(n_train-1):(n_train-p)])^2,
         # (diff(d_train.ts[,"prices"])[(n_train-1):(n_train-p)])^2,
         # (d_train.ts[,"sent1"][(n_train-1):(n_train-s2)])^2,
         # (d_train.ts[,"sent2"][(n_train-1):(n_train-s2)])^2)

  predicted_diff_scaled <- xT %*% beta
  # predicted_diff_scaled <- as.matrix(m_dlm$model[n_train-p-1,]) %*% beta
  
  pred_diff <- predicted_diff_scaled * train_sd["prices"]     # re-scale prediction
  preds[count]  <- pred_diff + d_train.ts[,"prices"][n_train] # re-add last price
  # pred_scaled <- cumsum(predicted_diff_scaled)
  # preds[count]  <- pred_scaled*train_sd["prices"] #+ train_mean["prices"]
  
  count <- count + 1
  n_train <- n_train + 1
}

results <- ts(data.frame(preds, ground_truth), 
                 start=c(as.numeric(format(as.Date(d[n_train0+1,1]), "%Y")), 
                         as.numeric(format(inds[1], "%j"))), 
                 frequency = 365)

pred_plot <- tibble(date = d[(n_train0+1):N, 1], preds, ground_truth) %>% 
  pivot_longer(-date, names_to = "type", values_to = "value") %>% 
  ggplot(aes(x = as.Date(date), y = value, color = type, group = type)) +
  geom_point(size = 3, alpha = 0.75) +
  geom_line(size = 1, alpha = 0.7) +
  scale_x_date(breaks = "week") +
  labs(title = "Predicted prices - Optimism",
       # subtitle = "Using historical data and Twitter-based sentiment",
       subtitle = "Using historical data only",
       x = "", y = "Market price",
       color = "") +
  theme_fivethirtyeight() +
  scale_color_manual(name="",
                     labels=c("Ground truth", "1-day forecast"),
                     values=c("black","darkred")) 

pred_plot

# rmse_NoSentiment <- sqrt(mean((preds-ground_truth)^2))
## 0.7005
# ggsave("Results/pred_plot_NoSentiment.png", plot = pred_plot, width = 6, height = 4)

# rmse_Sentiment <- sqrt(mean((preds-ground_truth)^2))
## 0.7076
# ggsave("Results/pred_plot_WithSentiment.png", plot = pred_plot, width = 6, height = 4)

