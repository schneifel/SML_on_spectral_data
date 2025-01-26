

# calculation of RMSE
calc.rmse <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  return(rmse)
}



# stratified sampling
get.folds <- function(y_data, k) {
  # Ensure y_data is a numeric vector
  if (!is.numeric(y_data)) stop("y_data must be numeric.")
  
  # Create strata based on quantiles of y_data
  strata <- cut(y_data, 
                breaks = quantile(y_data, probs = seq(0, 1, length.out = k + 1)), 
                include.lowest = TRUE, 
                labels = FALSE)
  
  # Sample folds and order them based on strata
  folds <- sample(rep(1:k, length.out = length(y_data)))
  folds <- as.numeric(factor(strata))[order(folds)]
  
  return(folds)
}



# training-validation split
data.prep <- function(i, folds, x_data, y_data, scale_data = TRUE) {
  
  # create indices
  val_indices <- which(folds == i)
  train_indices <- which(folds != i)
  
  # create training targets and predicitons
  train_x_data <- x_data[-val_indices, ]
  train_y_data <- y_data[-val_indices]
  train_predictors <- colnames(train_x_data)
  # create validation targets and predictions
  val_x_data <- x_data[val_indices, ]
  val_y_data <- y_data[val_indices]
  
  # if scale_data = TRUE: perform data centering and scaling (GAM)
  # if sclae_data = FALSE: skip data centering and scaling (CNN)
  if (scale_data) {
    train_x_data <- scale(train_x_data, center = TRUE, scale = TRUE)
    val_x_data <- scale(val_x_data, center = attr(train_x_data, "scaled:center"),
                        scale = attr(train_x_data, "scaled:scale"))
    train_x_data <- data.frame(train_x_data)
    val_x_data <- data.frame(val_x_data)
    
    # create training and validation data frames
    train_data.df <- data.frame(year = train_y_data, train_x_data)
    val_data.df <- data.frame(year = val_y_data, val_x_data)
  } else {
  
  # create training and validation data frames
  train_data.df <- data.frame(year = train_y_data, train_x_data)
  val_data.df <- data.frame(year = val_y_data, val_x_data)
  }
  
  return(list(
    train_x_data = train_x_data,
    train_y_data = train_y_data,
    val_x_data = val_x_data,
    val_y_data = val_y_data,
    train_data.df = train_data.df,
    val_data.df = val_data.df,
    train_predictors = train_predictors)
  )
}



# handle loading or initializing the cache
initialize.cache <- function(cache_file) {
  
  # check for existing cache file
  if (file.exists(cache_file)) {
    message("Cache file found.") 
    choice <- menu(
      c("Load cached results", "Ignore cache and start from Fold 1"),
      title = "Select an option:"
    )
    
    # choice = 1: load existing cache file
    if (choice == 1) {
      message("Loading cached results...")
      cached_results <- readRDS(cache_file)
      return(list(
        start_fold = max(cached_results$completed_folds) + 1,
        cached_results = cached_results
      ))
    } else {
      # choice != 1: ignore existing cache file and start from fold 1 
      message("Starting from Fold 1, ignoring the cache file.")
      return(list(
        start_fold = 1,
        cached_results = list(
        )
      ))
    }
  }
  # if no cache file exists, start from fold 1
  message("No cache file found. Starting from fold 1.")
  return(list(
    start_fold = 1,
    cached_results = list(
    )
  ))
}

# create function for saving results to the cache
save_to_cache <- function(cache_file, results, completed_folds) {
  results$completed_folds <- completed_folds
  saveRDS(results, cache_file)
}



# calculate RMSE statistics for GAM
gam.rmse_stats <- function(model, k) {
  
  # Initialize an empty data frame to store RMSE results of all folds
  n_rows <- length(model$all_rmse_val[[1]])
  perfold_RMSE <- data.frame(matrix(ncol = 0, nrow = n_rows))
  
  # Extract RMSE values for each fold
  for (i in seq_len(k)) {
    fold_name <- paste0("fold", i)
    perfold_RMSE[[fold_name]] <- model$all_rmse_val[[i]]
  }
  
  # Extract minimum RMSE of each fold, calculate SD and variance
  min_perfold_RMSE <- apply(perfold_RMSE, 2, min)
  mean_RMSE <- mean(min_perfold_RMSE)
  sd_RMSE <- sd(min_perfold_RMSE)
  var_RMSE <- sd_RMSE^2
  # Calculate training-validation error gap
  train_val_gap <- abs(
    min(model$mean_rmse_val) - min(model$mean_rmse_train)
  )
  
  return(list(
    min_perfold_RMSE <- min_perfold_RMSE,
    mean_RMSE <- mean_RMSE,
    sd_RMSE <- sd_RMSE,
    var_RMSE <- var_RMSE,
    train_val_gap <- train_val_gap
  ))
}



# calculate RMSE statistics for CNN
cnn.rmse_stats <- function(model) {
  
  # Extract RMSE values and assign fold names
  min_rmse_val <- model$model_summary_val[, "rmse"]
  names(min_rmse_val) <- paste0("fold", seq_along(min_rmse_val))
  
  min_rmse_train <- model$model_summary_train[, "rmse"]
  names(min_rmse_train) <- paste0("fold", seq_along(min_rmse_train))
  
  # Calculate statistics
  mean_min_rmse_val <- mean(min_rmse_val)
  mean_min_rmse_train <- mean(min_rmse_train)
  sd_min_rmse_val <- sd(min_rmse_val)
  var_min_rmse_val <- sd_min_rmse_val^2
  # Calculate training-validation error gap
  train_val_gap <- abs(mean_min_rmse_val - mean_min_rmse_train)
  
  # Return results as a list
  return(list(
    min_rmse_val = min_rmse_val,
    min_rmse_train = min_rmse_train,
    mean_min_rmse_val = mean_min_rmse_val,
    mean_min_rmse_train = mean_min_rmse_train,
    sd_min_rmse_val = sd_min_rmse_val,
    var_min_rmse_val = var_min_rmse_val,
    train_val_gap = train_val_gap
  ))
}

