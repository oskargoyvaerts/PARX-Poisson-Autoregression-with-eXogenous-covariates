
#Import libraries
library(tscount)
library(VGAM)
library(glarma)
library(forecast)
library(maxLik) 
library(numDeriv)

# ------Read the data, initialize---------
file_path <- 

df <- read.csv(file_path)
str(df)
# df <- na.omit(df)

# Preallocate columns for predicted probabilities
df$pred_home_win <- NA
df$pred_draw <- NA
df$pred_away_win <- NA
df$rps <- NA
df$home_frequency <- NA
df$away_frequency <- NA
df$ts_home_x <- NA
df$ts_away_x <- NA
df$home_fit_coeff <- NA
df$away_fit_coeff <- NA

# Some parameters:
horizon_matches <- 1140 # time horizon (in terms of no. of matches) used for recent history -- roughly 3 seasons
min_matches <- 10 # minimum number of matches needed to make prediction, must be larger than p_max + q_max
head <- 1
level <- 0.95

p_max <- 0
q_max <- 0
p_range <- 0:p_max
q_range <- 0:q_max


# --------Functions (some rewritten)-----------

##------------Functions to calculate probabilities----------------

# Function to calculate the probability that one Poisson random variable is greater than another
prob_greater <- function(lambda1, lambda2) {
  sum <- 0
  for (i in 1:10) {
    sum <- sum + dpois(i, lambda1) * ppois(i-1, lambda2) 
  } 
  return(sum)
}

# Function to calculate the probability that two Poisson random variables are equal
prob_equal <- function(lambda1, lambda2) {
  sum <- 0
  for (i in 0:10) {
    sum <- sum + dpois(i, lambda1) * dpois(i, lambda2)
  }
  return(sum)
}

# Function to calculate the probabilities for three outcomes: home win, draw, and away win
predict_probabilities <- function(home_lambda, away_lambda) {
  home_win_prob <- prob_greater(home_lambda, away_lambda)
  draw_prob <- prob_equal(home_lambda, away_lambda)
  away_win_prob <- prob_greater(away_lambda, home_lambda)  # Same as 1 - home_win_prob - draw_prob, but calculated for clarity
  return(c(home_win_prob, draw_prob, away_win_prob))
}

rankProbScore <- function(observed, predictions){
  ncat <- length(predictions)
  obsvec <- observed
  cumulative <- 0
  for (i in 1:ncat){
    cumulative <- cumulative + (sum(predictions[1:i]) - sum(obsvec[1:i]))^2
  }
  rps <- (1/(ncat-1))*cumulative
  return(rps)
}

# Function for parameter fitting, including optimal p, q (0/0 is optimal most of the time):
fit_pq <- function(ts_y, ts_x, p_range, q_range){
  
  best_model <- NULL
  model_AIC <- Inf
  lowest_AIC <- Inf
  model <- NULL
  best_p <- 0
  best_q <- 0
  for (p in p_range) {
    for (q in q_range) {
      if (p == 0) {
        if (q == 0) {
          try(model <- tsglm(ts_y, model = list(past_obs = NULL, past_mean = NULL), xreg = ts_x, link = "identity", distr = "poisson"), silent = TRUE)
        } else {
          try(model <- tsglm(ts_y, model = list(past_obs = 1:q, past_mean = NULL), xreg = ts_x, link = "identity", distr = "poisson"), silent = TRUE)
        }
      } else {
        if (q == 0) {
          try(model <- tsglm(ts_y, model = list(past_obs = NULL, past_mean = 1:p), xreg = ts_x, link = "identity", distr = "poisson"), silent = TRUE)
        } else {
          try(model <- tsglm(ts_y, model = list(past_obs = 1:q, past_mean = 1:p), xreg = ts_x, link = "identity", distr = "poisson"), silent = TRUE)
        }        
      }
      
      try(model_AIC <- AIC(model, k = 2), silent = TRUE) # k is a penalty per parameter, default value is 2
      
      if (!is.na(model_AIC)&&(model_AIC < lowest_AIC)) {
        best_model <- model
        lowest_AIC <- model_AIC
        best_p <- p
        best_q <- q
      }
      print(c(p, q, model_AIC))
    }
  }
  print(c(best_p, best_q))
  return(best_model)
}

# ------Main loop-------------

# Loop through rows of the dataframe
for (i in (horizon_matches + 1):nrow(df)) { # I'll only start predicting outcomes after having 750 matches as historical data
  # Get teams for the current match:
  home_team <- df$home_team[i]
  away_team <- df$away_team[i]
  # Get the relevant historical data:
  df1 <- df[(i-horizon_matches):(i-1), ]
  
  home_data <- df1[df1$home_team == home_team, ] # restricted to recent history
  home_data_index <- c(which(df1$home_team == home_team, arr.ind = FALSE))
  home_frequency <- nrow(home_data) # restricted to recent history
  away_data <- df1[df1$away_team == away_team, ]
  away_data_index <- c(which(df1$away_team == away_team, arr.ind = FALSE))
  away_frequency <- nrow(away_data)
  
  if (home_frequency > min_matches && away_frequency > min_matches) {
    # Generate predictions for the match:
    
    # 1. find x_i_home and x_i_away (this could be done once and stored in df)
    ts_home_x <- cumsum(c(home_data$awayGoalCount)) / seq_along(c(home_data$awayGoalCount))
    ts_away_x <- cumsum(c(away_data$homeGoalCount)) / seq_along(c(away_data$homeGoalCount))
    
    # 2. find lambda_i_home and lambda_i_away by fitting parameters to historical data
    ts_home_y <- ts(home_data$homeGoalCount) # goal counts in recent history only
    ts_away_y <- ts(away_data$awayGoalCount) # goal counts in recent history only
    
    home_fit <- fit_pq(ts_home_y, ts_home_x, p_range, q_range)
    away_fit <- fit_pq(ts_away_y, ts_away_x, p_range, q_range)
    
    home_lambda <- -1
    away_lambda <- -1
    try(home_lambda <- predict(home_fit, n.head = head, newxreg = c(ts_home_x[length(ts_home_x)]), level = level)$pred[1], silent = TRUE)
    try(away_lambda <- predict(away_fit, n.head = head, newxreg = c(ts_away_x[length(ts_away_x)]), level = level)$pred[1], silent = TRUE)
    
    if ((home_lambda > 0)&&(away_lambda > 0)) {
    pred_probs <- predict_probabilities(home_lambda, away_lambda)
    } else pred_probs <- c(NA, NA, NA)
    
    # Store predictions in dataframe
    df$pred_home_win[i] <- pred_probs[1]
    df$pred_draw[i] <- pred_probs[2]
    df$pred_away_win[i] <- pred_probs[3]
    df$home_frequency[i] <- home_frequency
    df$away_frequency[i] <- away_frequency
    df$ts_home_x[i] <- ts_home_x[length(ts_home_x)]
    df$ts_away_x[i] <- ts_away_x[length(ts_away_x)]
    df$home_fit_coeff[i] <- paste(home_fit$coefficients, collapse = ", ")
    df$away_fit_coeff[i] <- paste(away_fit$coefficients, collapse = ", ")
    print(c(pred_probs[1], pred_probs[2], pred_probs[3]))
  }
  else{ # just return NA in this case, so we know there wasn't enough data to predict anything
    df$home_frequency[i] <- home_frequency
    df$away_frequency[i] <- away_frequency
  }
  
  # comparing the rps between the actual result and the prediction:
  result_home_goals <- df$homeGoalCount[i]
  result_away_goals <- df$awayGoalCount[i]
  result_vec <- c(0,0,0)
  if (result_home_goals>result_away_goals) {
    result_vec[1] <- 1
  }  else {
    if (result_home_goals<result_away_goals){
      result_vec[3] <- 1
    } else {result_vec[2] <- 1}
  }
  df$rps[i] <- rankProbScore(result_vec, c(df$pred_home_win[i], df$pred_draw[i], df$pred_away_win[i]))
}

# Save your DataFrame as a CSV file
# write.csv(df[nrow(df)-limit_match:nrow(df), ], "C:/Users/oskar/Downloads/check5.csv", row.names = FALSE)

write.csv(df, file_path, row.names = FALSE)
