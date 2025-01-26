

################################################################################
############################### LEARNING CURVES ################################
################################################################################

# GAM learning curves
plot.gam.curves <- function(model) {
  
  # Create data frames for mean curves and fold-specific curves
  plot.df <- data.frame(
    Iter = rep(1:mstop_max, 2),
    RMSE = c(model$mean_rmse_train, model$mean_rmse_val),
    Type = rep(c("Training", "Validation"), each = mstop_max)
  )
  fold_preds_df <- do.call(rbind, lapply(seq_along(model$all_rmse_val), function(fold) {
    data.frame(
      Iter_fold = rep(1:mstop_max, 2),
      RMSE_fold = model$all_rmse_val[[fold]],
      Fold = paste("Fold", fold))
  }))
  
  # Extract optimal number of iterations
  lowest_val_rmse_iter <- plot.df %>%
    filter(Type == "Validation") %>%
    filter(RMSE == min(RMSE)) %>%
    pull(Iter)
  
  # Create the learning curve plot
  curve_plot_gam <- ggplot() +
    # Add fold-wise validation curves
    geom_line(data = fold_preds_df, aes(x = Iter_fold, y = RMSE_fold, group = Fold),
              color = "grey40", alpha = 0.6, linewidth = 0.5) +
    geom_line(data = plot.df, aes(x = Iter, y = RMSE, color = Type),
              linewidth = 0.8) +
    labs(x = "mstop", y = "RMSE [years]") +
    theme_minimal() +
    scale_color_manual(
      values = c("seagreen", "hotpink3"), 
      labels = c("Training", "Validation")) +
    guides(color = guide_legend(title = "data")) +
    theme(
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)) +
    geom_vline(xintercept = lowest_val_rmse_iter, 
               color = "azure4", linetype = "dashed", linewidth = 0.8) +
    scale_x_continuous(breaks = seq(0, max(plot.df$Iter), by = 50))
  
  return(list(
    curve_plot_gam = curve_plot_gam,
    it_lowest_rmse = lowest_val_rmse_iter))
}


# CNN learning curves
plot.cnn.curves <- function(history){
  
  # Create data frame for plotting main curves
  plot_data <- data.frame(
    epoch = 1:length(history$metrics$rmse),
    rmse = history$metrics$rmse,
    val_rmse = history$metrics$val_rmse,
    loss = history$metrics$loss,
    val_loss = history$metrics$val_loss
  )
  
  # Create RMSE curves
  curve_rmse <- ggplot(plot_data, aes(x = epoch)) +
    geom_line(aes(y = rmse),
              color = "seagreen1", linewidth = 0.8) +
    geom_line(aes(y = val_rmse),
              color = "plum", linewidth = 0.8) +
    geom_smooth(aes(y = rmse, color = "Training"),
                method = "loess", se = FALSE, linetype = "solid", size = 0.8) +
    geom_smooth(aes(y = val_rmse, color = "Validation"),
                method = "loess", se = FALSE, linetype = "solid", size = 0.8) +
    labs(y = "RMSE [years]") +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray80", linewidth = 0.8),
      panel.grid.minor = element_line(color = "gray80", linewidth = 0.2)) +
    scale_color_manual(name = "data",
                       values = c("Training" = "seagreen", "Validation" = "hotpink3")) +
    scale_x_continuous(breaks = seq(0, max(history$metrics$rmse), by = 5))
  
  # Create loss curves
  curve_loss <- ggplot(plot_data, aes(x = epoch)) +
    geom_line(aes(y = loss),
              color = "seagreen1", linewidth = 0.8) +
    geom_line(aes(y = val_loss),
              color = "plum", linewidth = 0.8) +
    geom_smooth(aes(y = loss, color = "Training"),
                method = "loess", se = FALSE, linetype = "solid", size = 0.8) +
    geom_smooth(aes(y = val_loss, color = "Validation"),
                method = "loess", se = FALSE, linetype = "solid", size = 0.8) +
    labs(y = "Loss") +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray80", linewidth = 0.8),
      panel.grid.minor = element_line(color = "gray80", linewidth = 0.2)) +
    scale_color_manual(name = "data",
                       values = c("Training" = "seagreen", "Validation" = "hotpink3")) +
    scale_x_continuous(breaks = seq(0, max(history$metrics$rmse), by = 5))
  
  return(list(curve_rmse = curve_rmse, curve_loss = curve_loss))
}


################################################################################
####################### FOLD-SPECIFIC PREDS OVER TARGETS #######################
################################################################################

# fold-specific plots of preds over targets
foldwise.plot <- function(model) {
  
  # Extract targets and predictions based on model type
  # cnn: flatten only
  if (grepl("cnn", deparse(substitute(model)))) {
    val_preds_flat <- unlist(model$all_preds_val)
    val_targets_flat <- unlist(model$all_targets_val)
  } else if (grepl("gam", deparse(substitute(model)))) {
    # gam: compute prediction means across boosting iterations across folds, then flatten
    val_preds <- lapply(model$all_preds_val, function(fold) {
      rowMeans(do.call(cbind, fold))
    })
    val_preds_flat <- unlist(val_preds)
    val_targets_flat <- unlist(model$all_targets_val)
  } else {
    stop("Unknown model type. Please provide a model with 'cnn.cv' or 'gam.cv'.")
  }
  
  # Create indices for folds
  val_fold_index <- rep(seq_along(model$all_targets_val), sapply(model$all_targets_val, length))
  
  # Adjust x-values for separation
  max_fold_length <- max(sapply(model$all_targets_val, length))
  padding_multiplier <- 1.5
  
  adjusted_x <- unlist(lapply(seq_along(model$all_targets_val), function(i) {
    fold_length <- length(model$all_targets_val[[i]])
    seq_len(fold_length) + (i - 1) * (max_fold_length * padding_multiplier)
  }))
  
  # Create data frame for plotting
  val_data <- data.frame(
    Adjusted_Targets = adjusted_x,
    Targets = val_targets_flat,
    Predictions = val_preds_flat,
    Fold = factor(val_fold_index)
  )
  
  # Define positions for vertical dashed lines
  vline_positions <- seq(max_fold_length * padding_multiplier, 
                         (length(model$all_targets_val) - 1) * (max_fold_length * padding_multiplier), 
                         by = max_fold_length * padding_multiplier)
  
  # Create x = y line
  diagonal_data <- do.call(rbind, lapply(unique(val_fold_index), function(i) {
    fold_data <- val_data[val_data$Fold == i, ]
    data.frame(
      Adjusted_Targets = fold_data$Adjusted_Targets,
      Targets = fold_data$Targets,
      Fold = fold_data$Fold
    )
  }))
  
  # Create plot
  plot <- ggplot(val_data, aes(x = Adjusted_Targets, y = Predictions, color = Fold)) +
    geom_point(shape = 19, size = 2) +
    geom_line(data = diagonal_data, aes(x = Adjusted_Targets, y = Targets, group = Fold), 
              stat = "smooth", linetype = "twodash", linewidth = 0.5) + # Diagonal x=y line within folds
    geom_vline(xintercept = vline_positions, linetype = "dashed", color = "azure4") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "azure4") +
    labs(
      x = "Targets [years]",
      y = "Predictions [years]",
      color = "Fold"
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = seq(max_fold_length * padding_multiplier / 2, 
                                    length(model$all_targets_val) * (max_fold_length * padding_multiplier), 
                                    by = max_fold_length * padding_multiplier),
                       labels = paste("Fold", seq_along(model$all_targets_val))) +
    scale_y_continuous(limits = c(900, 1650), breaks = seq(900, 1650, by = 100))
  
  return(plot)
}


################################################################################
############################### DIAGNOSTIC PLOTS ###############################
################################################################################

# create diagnostic plots
library(qqplotr)
diagnostic.plots <- function(model = NULL, preds = NULL, targets = NULL) {
  # Determine the data source based on the input arguments
  if (!is.null(model)) {
    if (grepl("cnn", deparse(substitute(model)))) {
      preds <- unlist(model$all_preds_val)
      targets <- unlist(model$all_targets_val)
    } else if (grepl("gam", deparse(substitute(model)))) {
      val_preds <- lapply(model$all_preds_val, function(fold) {
        rowMeans(do.call(cbind, fold))
      })
      preds <- unlist(val_preds)
      targets <- unlist(model$all_targets_val)
    } else {
      stop("Unknown model type. Please provide a model with 'cnn' or 'gam'.")
    }
  } else if (is.null(preds) || is.null(targets)) {
    stop("Either a model or both 'preds' and 'targets' must be provided.")
  }
  
  residuals <- targets - preds
  
  overall_data <- data.frame(
    targets = targets,
    preds = preds,
    residuals = residuals
  )
  
  # Predictions vs. Targets
  predictions_vs_targets <- ggplot(overall_data, aes(x = targets, y = preds)) +
    geom_point(shape = 19, size = 2, color = "mediumseagreen") +
    geom_abline(slope = 1, intercept = 0, linetype = "twodash", color = "goldenrod", linewidth = 0.8) +  # Reference line y = x
    geom_smooth(method = "lm", fill = "maroon1", alpha = 0.2, color = "violet", linewidth = 0.8) + # shaded area = 95% CI, default = loess
    labs(
      x = "Targets [years]",
      y = "Predictions [years]"
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(900, 1800, by = 150)) +
    scale_y_continuous(breaks = seq(900, 1800, by = 150))
  
  # Q-Q-Plot to assess the normality of residuals
  qq_plot <- ggplot(overall_data, mapping = aes(sample = residuals)) +
    geom_qq_band(bandType = "ts", fill = "maroon1", alpha = 0.2) +
    stat_qq_point(color = "mediumseagreen", size = 1, shape = 19) +
    stat_qq_line(color = "goldenrod", linetype = "twodash", linewidth = 0.8) +
    theme(legend.position = "none") +
    labs(
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
  
  # Predictions vs. Residuals
  preds_vs_residuals <- ggplot(overall_data, aes(x = preds, y = residuals)) +
    geom_point(shape = 19, size = 2, color = "mediumseagreen") +
    geom_hline(yintercept = 0, linetype = "twodash", color = "goldenrod", linewidth = 0.8) +
    labs(
      x = "Predictions [years]",
      y = "Residuals"
    ) +
    theme_minimal()
  
  # Histogram of residuals to visualize the distribution of residuals
  histogram_residuals <- ggplot(overall_data, aes(x = residuals)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 25, fill = "darkseagreen", color = "seagreen") +    # binwidth = 12 for final model
    geom_density(color = "maroon1", linewidth = 0.8, bw = 50) + # Adjust bandwidth for smoothness
    labs(
      x = "Residuals",
      y = "Density"
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(-300, 300, by = 50)) +
    scale_y_continuous(breaks = seq(0, 0.02, by = 0.002))
  
  return(list(
    predictions_vs_targets = predictions_vs_targets,
    qq_plot = qq_plot,
    preds_vs_residuals = preds_vs_residuals,
    histogram_residuals = histogram_residuals
  ))
}



