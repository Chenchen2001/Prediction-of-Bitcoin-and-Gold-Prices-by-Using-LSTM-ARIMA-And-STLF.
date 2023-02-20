library(keras)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
dataframe <- read.csv('./GOLD.csv')

# plot(dataframe$USD, type="l", xlab = "Indedx of Date", ylab = "Gold Value")
max_value <- max(dataframe$USD)
min_value <- min(dataframe$USD)
spread <- max_value - min_value

dataset <- (dataframe$USD - min_value) / spread

create_dataset <- function(dataset,look_back = 1){
  l <- length(dataset)
  dataX <- array(dim = c(l - look_back, look_back))
  
  for (i in 1:ncol(dataX))
  {
    dataX[, i] <- dataset[i:(l - look_back + i - 1)]
  }
  
  dataY <- array(
    data = dataset[(look_back + 1):l],
    dim = c(l - look_back, 1))
  
  return(
    list(
      dataX = dataX,
      dataY = dataY))
}

train_size <- as.integer(length(dataset) * 0.67)
test_size <- length(dataset) - train_size

train <- dataset[1:train_size]
test <- dataset[(train_size + 1):length(dataset)]

cat(length(train), length(test))


results <- data.frame(lookback=0, train_err=0, test_err = 0)

set.seed(7)
look_backs <- seq(39,50,2)

for(look_back in look_backs){
  
  train_size <- as.integer(length(dataset) * 0.67)
  test_size <- length(dataset) - train_size
  
  train <- dataset[1:train_size]
  test <- dataset[(train_size + 1):length(dataset)]
  
  trainXY <- create_dataset(train, look_back)
  testXY <-  create_dataset(test, look_back)
  
  dim_train <- dim(trainXY$dataX)
  dim_test <- dim(testXY$dataX)
  
  dim(trainXY$dataX) <- c(dim_train[1], dim_train[2], 1)
  dim(testXY$dataX) <- c(dim_test[1], dim_test[2], 1)
  
  batch_size = 1
  
  model <- keras_model_sequential()
  
  model %>%
    layer_lstm(
      units = 4,
      batch_input_shape = c(
        batch_size,
        look_back,
        1),
      stateful = TRUE,
      return_sequences = TRUE) %>%
    layer_lstm(
      units = 4,
      batch_input_shape = c(
        batch_size,
        look_back,
        1),
      stateful = TRUE) %>%
    layer_dense(
      units = 1) %>%
    compile(
      loss = 'mean_squared_error',
      optimizer = 'adam')
  # train the model
  for (i in 1:100){
    model %>%
      fit(trainXY$dataX,
          trainXY$dataY,
          epochs = 1,
          batch_size = batch_size,
          verbose = 0, # output log? 0 for no log 1 for log
          shuffle = FALSE)
    
    model %>%
      reset_states()
    
    if(i%%10==0){
      print(paste("=====",as.character(look_back),"=====",as.character(i),"====="))
    }
  }
  # predict data based on models
  trainPredict <- model %>%
    predict(
      trainXY$dataX,
      batch_size = batch_size,
      verbose = 2
    )
  
  model %>%
    reset_states()
  
  testPredict <- model %>%
    predict(
      testXY$dataX,
      batch_size = batch_size,
      verbose = 2)
  
  trainScore <- var(trainXY$dataY - trainPredict) * spread^2
  testScore <- var(testXY$dataY - testPredict) * spread^2
  # store the RMSE of each look_back
  results <- rbind(results, c(look_back,sqrt(trainScore),sqrt(testScore)))
  print(paste("==========look back of",as.character(look_back),"finished, training ERR:" ,as.character(sqrt(trainScore)),"test ERR:" ,as.character(sqrt(testScore),"==========")))
  trainPredict <- trainPredict * spread + min_value
  testPredict <- testPredict * spread + min_value
  
  df <- data.frame(
    index = 1:length(dataset),
    value = dataset * spread + min_value,
    type = 'raw') %>%
    rbind(
      data.frame(
        index = 1:length(trainPredict) + look_back,
        value = trainPredict,
        type = 'train')) %>%
    rbind(
      data.frame(
        index = 1:length(testPredict) + look_back + length(train),
        value = testPredict,
        type = 'test'))
  
  p <- ggplot(data = df) +
    geom_line(
      mapping = aes(
        x = index,
        y = value,
        color = type)) +
    geom_point(
      mapping = aes(
        x = index,
        y = value,
        color = type)) +
    geom_vline(
      xintercept = length(train) + 0.5) +
    theme_economist() +
    scale_color_economist()
  
  ggsave(p, filename = paste("./GOLD/GOLD-lookback-",as.character(look_back),".png"), width = 600, height = 400, units = "mm")
  rm(model)
}

results <- results[which(results$lookback!=0),]


plot(results$lookback, results$train_err, type="l", bty = "n", xaxt = "n", yaxt = "n", ann = FALSE)
axis(1, labels = seq(1,50,2), at = seq(1,50,2))
title(xlab= 'look back', ylab = 'train error')
axis(2, labels = seq(0,30,5), at = seq(0,30,5))

plot(results$lookback, results$test_err, type="l", bty = "n", xaxt = "n", yaxt = "n", ann = FALSE)
axis(1, labels = seq(1,50,2), at = seq(1,50,2))
title(xlab= 'look back', ylab = 'test error')
axis(2, labels = seq(50,170,20), at = seq(50,170,20))
