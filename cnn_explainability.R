
library(lime)

# clear up predictor variable names
colnames(x_data) <- gsub("(\\d+\\.\\d{2})\\d*", "\\1", colnames(x_data))

# identify class of model object
class(cnn_orig_final)
# class(cnn_deriv_final)

# Setup lime::model_type()
model_type.keras.engine.sequential.Sequential <- function(x, ...) {
  "regression"
}

# Setup lime::predict_model()
predict_model.keras.engine.sequential.Sequential <- function(x, newdata, type, ...) {
  # Ensure input dimensions match what the model expects
  pred <- predict_model(object = x, x = as.matrix(newdata), type = "raw", ...)
  data.frame(pred)
}

# Create an explainer object
explainer <- lime::lime(
  as.data.frame(x_data),
  cnn_orig_final,
  bin_continuous = FALSE    # Prevent binning continuous variables
)

# initiate parallelization
numCores <- detectCores()
cl <- makeCluster(round(numCores*0.38))
registerDoParallel(cl)

system.time (
  explanation <- lime::explain (
    x = as.data.frame(x_data),
    explainer = explainer,
    # return top 25 features that are considered critical to each case
    n_features = 25,
    n_permutations = 500,
    kernel_size = 0.5)
)

# quit clusters
stopCluster(cl)

save(explanation, file = "data/CNN_feature_expl_dparFullOrig_allfeatures.RData")


library(dplyr)

top_features <- plot_explanations(explanation)

top_features +
  scale_fill_gradientn(
    colors = c("deeppink", "#BDD2F0", "forestgreen"),
    guide = "colorbar"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank()
  )

plot_features(top_features)