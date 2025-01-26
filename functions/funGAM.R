

# custom gamboost function for tuning
tune.gamboost <- list(
  type = "Regression",
  library = "mboost",
  loop = NULL,
  
  parameters = data.frame(parameter = c("nu", "knots", "df"),
                          class = rep("numeric", 3),
                          label = c("nu", "knots", "df")),
  
  grid = function(x, y, len = NULL, search = "random") {
    data.frame(nu = runif(len, 0.01, 0.15),     # draws continuous values equally distributed across range
               knots = sample(20:50, len, replace = TRUE),  # draws discrete values in range
               df = sample(3:5, len, replace = TRUE))
  },
  
  fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    data <- as.data.frame(x)
    data$year <- y
    
    # Set up formula
    formula_parameters <- sapply(colnames(x), function(predictor) {
      sprintf("bbs(%s, knots = %f, df = %d)", predictor, param$knots, param$df)
    })
    formula <- as.formula(paste("year ~", paste0(formula_parameters, collapse = " + ")))
    
    # Set up control parameters
    control <- boost_control(nu = param$nu,
                             risk = "oobag")
    
    # create model
    modelFit <- gamboost(formula, data = data, control = control)
    
    return(modelFit)
  },
  
  predict = function(modelFit, newdata, preProc = c("center", "scale"), submodels = NULL) {
    predict(modelFit, newdata)
  },
  
  prob = NULL
)


# using k-fold CV for model training during HP search
CV.fit.control <- trainControl(
  method = "repeatedcv",
  number = 5,      # 5-fold CV
  repeats = 5,     # 5 iterations for each fold
  verboseIter = TRUE,
  returnResamp = "all",
  returnData = TRUE,
  savePredictions = "all",
  search = "random")


# create formula for CV with identified optimal hyperparameter
get.formula <- function(predictors, best_hp) {
  
  # define parameters for formula with identified optimal hyperparameters
  parameters <- sapply(predictors, function(predictor) {
    sprintf("bbs(%s, knots = %d, df = %d)", 
            predictor, 
            best_hp$knots, 
            best_hp$df)
  })
  formula <- as.formula(paste("year ~", paste0(parameters, collapse = "+")))
  
  return(formula)
}


# run k-fold cross-validation
gamCV.fun <- function(x_data, y_data, best_hp, ci = TRUE, cache_file = "data/cv_dparFull_cache.rds") {
  
  # Ensure all data arguments are provided
  if (missing(x_data) || missing(y_data) || missing(best_hp)) {
    stop("Error: Check number and position of arguments: x_data, y_data, best_hp.")
  }
  
  # Load or initialize cache
  cache <- initialize.cache(cache_file)
  start_fold <- cache$start_fold
  results <- cache$cached_results
  
  set.seed(385)
  # Create stratified folds
  folds <- get.folds(y_data, k)
  
  # k-fold cross-validation
  for (i in start_fold:k) {
    cat("\n\nTotal no. of folds:", k, "\n")
    cat("\nCurrently processing fold:", i, "\n")
    
    # Prepare train-val split and perform centering and scaling of predictors
    data.df <- data.prep(i, folds, x_data, y_data)
    
    # Prepare formula
    formula <- get.formula(data.df$train_predictors, best_hp)
    
    # Train model iteratively for mstop
    gam.cv <- gamboost(formula,
                       data = data.df$train_data.df,
                       control = boost_control(nu = best_hp$nu, mstop = 1))
    
    # Initialize storage for mstop iterations
    preds_train <- list()
    preds_val <- list()
    
    for (j in 1:mstop_max) {
      if (j == 1 || j %% 5 == 0) { cat("\nCurrent iteration:", j, "\n") }
      
      # Update model to current mstop
      gam.cv[j]
      
      # Predict on train and validation sets
      preds_train_it <- predict(gam.cv, data.df$train_x_data)
      preds_val_it <- predict(gam.cv, data.df$val_x_data)
      
      preds_train[[j]] <- preds_train_it
      preds_val[[j]] <- preds_val_it
    }
    
    # Compute RMSE for train and validation
    rmse_train_fold <- sapply(preds_train, function(preds) calc.rmse(data.df$train_y_data, preds))
    rmse_val_fold <- sapply(preds_val, function(preds) calc.rmse(data.df$val_y_data, preds))
    
    baseline_preds <- predict(gam.cv, data.df$val_x_data)
    baseline_rmse <- calc.rmse(data.df$val_y_data, baseline_preds)
    
    # Initialize vector to store permutation importance
    perm_imp <- numeric(ncol(data.df$val_x_data))
    names(perm_imp) <- colnames(data.df$val_x_data)
    
    # Loop over features for permutation
    for (v in seq_along(perm_imp)) {
      permuted_data <- data.df$val_x_data
      permuted_data[, v] <- sample(permuted_data[, v])  # Permute the feature
      
      # Predict with permuted data
      perm_preds <- predict(gam.cv, permuted_data)
      
      # Compute RMSE with permuted data
      perm_rmse <- calc.rmse(data.df$val_y_data, perm_preds)
      
      # Calculate importance as the increase in RMSE
      perm_imp[v] <- ((perm_rmse - baseline_rmse) / baseline_rmse) * 100
    }
    
    # Store predictions, RMSE, and variable importance
    results$all_preds_train[[i]] <- preds_train
    results$all_preds_val[[i]] <- preds_val
    results$all_targets_train[[i]] <- data.df$train_y_data
    results$all_targets_val[[i]] <- data.df$val_y_data
    results$all_rmse_train[[i]] <- rmse_train_fold
    results$all_rmse_val[[i]] <- rmse_val_fold
    results$all_models[[i]] <- gam.cv
    results$all_perm_imp[[i]] <- perm_imp
    
    # Save intermediate results to cache
    save_to_cache(cache_file, results, 1:i)
    cat("\nFold no.", i, "- processing complete and results cached.\n")
  }
  
  # Optionally delete cache file
  if (file.exists(cache_file)) {
    file.remove(cache_file)}
  
  # Calculate mean RMSE and overall results
  mean_rmse_train <- rowMeans(do.call(cbind, lapply(results$all_rmse_train, unlist)))
  mean_rmse_val <- rowMeans(do.call(cbind, lapply(results$all_rmse_val, unlist)))
  
  results$mean_rmse_train <- mean_rmse_train
  results$mean_rmse_val <- mean_rmse_val
  
  aggregated_importance <- Reduce("+", results$all_perm_imp) / k
  results$permutation_importance <- aggregated_importance
  
  cat("\n\033[1;32mCROSS-VALIDATION COMPLETED - ALL RESULTS SAVED\033[0m\n")
  
  return(results)
}


# define formula parameters with best hp for final model
get.formula_final <- function(data, best_hp) {
  predictors <- colnames(data)[-1]
  
  parameters <- sapply(predictors, function(predictor) {
    sprintf("bbs(%s, knots = %d, df = %d)", 
            predictor, 
            best_hp$knots, 
            best_hp$df)
  })
  formula_str <- paste("year ~", paste0(parameters, collapse = "+"))  #, intercept = FALSE
  formula <- as.formula(formula_str)
  
  return(formula)
}

# get formula and control settings with best hp for fitting final model
get.settings <- function(data, best_hp, mstop) {
  # Call get.control to get the control settings
  control <- boost_control(nu = best_hp$nu,
                           mstop = mstop,
                           risk = "oobag")
  
  # Call get.formula_final to get the formula for the final model
  formula <- get.formula_final(data, best_hp)
  
  # Return both the control settings and the formula
  return(list(control = control, formula = formula))
}


# compute 10% threshold and low score variables from permutation importance determination
calc.threshold <- function(data_frame) {
  # Extract the variable importance from the model and identify maximum
  varimp.df <- data.frame(importance = data_frame$importance)
  max_varimp <- max(varimp.df$importance)
  
  # calculate 10% threshold
  threshold <- 0.1 * max_varimp
  
  # Filter for variables with importance below or equal to the threshold
  low_scores<- varimp.df$importance[varimp.df$importance <= threshold]
  low_scores <- data.frame(low_scores)
  
  return(list(
    low_scores = low_scores,
    threshold = threshold))
}

