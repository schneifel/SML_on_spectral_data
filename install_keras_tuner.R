
# install tensorflow, keras, kerastuner
install.packages("remotes")
remotes::install_github("rstudio/tensorflow")

reticulate::install_python()

library(tensorflow)
install_tensorflow(envname = "r-tensorflow")

install.packages("keras")
library(keras)
install_keras()

library(tensorflow)

tf$constant("Hello TensorFlow!")

install.packages('kerastuneR')

kerastuneR::install_kerastuner()

