

# function to optimize parameters except
build.model = function(hp) {
  model = keras_model_sequential()
  model %>% layer_gaussian_noise(stddev = 0.01, input_shape = c(ncol(x_data), 1))
  
  for (i in 1:(hp$get('num_layers')) ) {
    model %>% layer_conv_1d(filters = hp$Int(paste('filters_', i, sep = ''), 15L, 260L, 31L),
                            kernel_size = hp$Int(paste('kernel_size_', i, sep = ''), 8L, 40L, 8L),
                            activation = 'relu',
                            padding = 'same') %>%
      layer_max_pooling_1d(pool_size = hp$Int(paste('pool_size_', i, sep = ''), 2L, 5L, 1L), strides = 2)
  }
  
  model %>% layer_dropout(rate = hp$Float('dropout_bf', 0.1, 0.5, 0.1)) %>%
    layer_global_average_pooling_1d() %>%
    layer_flatten() %>%
    layer_dense(units = hp$Int('units', 32, 260, 32),
                activation = 'relu') %>%
    layer_dropout(rate = hp$Float('dropout_af', 0.1, 0.5, 0.1)) %>%
    layer_dense(units = 1, activation = 'linear') %>%
    compile(optimizer = hp$Choice('optimizer', list('adam', 'rmsprop')),
            loss = hp$Choice('loss', list('huber_loss', 'mse')),
            metrics = 'mse')
  model %>% summary()
  return(model)
}


# run k-fold cross-validation
cnnCV.fun <- function(x_data, y_data, build_model.fun, cache_file = "data/cv_dparFull_cache.rds") {
  
  # Load or initialize cache
  cache <- initialize.cache(cache_file)
  start_fold <- cache$start_fold
  results <- cache$cached_results
  
  set.seed(123) # seed for data splitting
  tensorflow::tf$random$set_seed(123)   # seed for keras operations
  
  # Create stratified folds
  folds <- get.folds(y_data, k)
  
  # Initialize storage for epoch iterations
  model_summary <- list()
  preds_train <- list()
  preds_val <- list()
  
  # k-fold cross-validation
  for (i in 1:k) {
    cat("\n", "\n","Total no. of folds:", (k), "\n")
    cat("\n", "Currently processing fold:", i, "\n", "\n")
    
    # Prepare train-val split without centering/scaling of predictors
    data.df <- data.prep(i, folds, x_data, y_data, scale_data = FALSE)
    
    # Build the Keras model (already compiled)
    cnn.cv <- build_model.fun()
    
    # Train the model (silent mode: verbose=0, track training: verbose=1)
    suppressWarnings({
      history <- cnn.cv %>% fit(
        data.df$train_x_data, data.df$train_y_data,
        validation_split = 0.1,
        epochs = num_epochs,
        batch_size = batch_size,
        verbose = 1
      )
    })
    
    # Predict on train and validation sets
    preds_train <- predict(cnn.cv, data.df$train_x_data, batch_size = 8)
    preds_val <- predict(cnn.cv, data.df$val_x_data, batch_size = 8)
    
    # Compute model metrics
    cat("evaluate model, fold", i, "\n")
    evaluate_results_val <- cnn.cv %>% evaluate(data.df$val_x_data, data.df$val_y_data, batch_size = 8, verbose = 1)
    model_summary <- rbind(model_summary, evaluate_results_val)
    
    # Store targets, predictions and model summaries
    results$all_preds_train[[i]] <- preds_train
    results$all_preds_val[[i]] <- preds_val
    results$all_targets_train[[i]] <- data.df$train_y_data
    results$all_targets_val[[i]] <- data.df$val_y_data
    results$history[[i]] <- history
    
    # Save intermediate results to cache
    save_to_cache(cache_file, results, 1:i)
    cat("\nFold no.", i, "- processing complete and results cached.\n")
  }
  
  # Optionally delete cache file
  if (file.exists(cache_file)) {
    file.remove(cache_file)}
  
  cat("\n\033[1;32mCROSS-VALIDATION COMPLETED - ALL RESULTS SAVED\033[0m\n")
  
  return(results)
}


# calculate R2 for CNN
calc.r2_folds <- function(model) {
  # Initialize an empty vector to store R^2 for each fold
  r2_folds <- numeric()
  
  # Loop through each fold to calculate R^2
  for (i in 1:length(model$all_preds_val)) {
    # Flatten predictions and true values for the current fold
    preds <- model$all_preds_val[[i]]
    true_vals <- model$all_targets_val[[i]]
    
    # Calculate residual sum of squares (ss_res) and total sum of squares (ss_tot) for the current fold
    ss_res <- sum((true_vals - preds)^2)
    ss_tot <- sum((true_vals - mean(true_vals))^2)
    # Calculate R^2 for the current fold
    r2_folds[i] <- 1 - (ss_res / ss_tot)
  }
  
  mean_r2_folds <- mean(r2_folds)
  sd_r2_folds <- sd(mean_r2_folds)
  all_preds <- unlist(model$all_preds_val)
  all_targets <- unlist(model$all_targets_val) 
  all_ss_res <- sum((all_targets - all_preds)^2)
  all_ss_tot <- sum((all_targets - mean(all_targets))^2)
  overall_r2 <- 1 - (all_ss_res / all_ss_tot)
  
  return(list(
    mean_r2_folds = round(mean_r2_folds, 2),
    sd_r2_folds = round(sd_r2_folds, 2),
    r2_folds = round(r2_folds, 2),
    overall_r2 = round(overall_r2, 2)
  ))
}

