# A Comparative Study of using GAM and CNN to Date Historical Parchment with NIR Spectral Data

This repository contains all scripts and function codes necessary to investigate the predictive performance of GAM and CNN
when trained and fitted with NIR spectral data.

The study assesses the use of supervised machine learning methods Generalized Additive Models (GAMs) and Convolutional Neural
Networks (CNNs), to predict the age of historical parchment using Near Infrared (NIR) spectral data.
The main objective is to evaluate the predictive performance of optimized models on vector normalized NIR data and their respective
second-order derivatives and reduce the RMSE as much as possible.
Analysis includes tuning the respective hyperparameters, cross-validation, and evaluation based on common performance metrics.

The markdown-files "generalized_additive_model.RMD" and "convolutional_neural_networks.RMD" contain the main code for the respective model.
All functions necessary for analyses are called from these main files.
For GAMs, everything from tuning to cross-validation to the final model is fully automatic, so basically no manual manipulation is necessary.
For CNNs, after hyperparameter tuning, the model architecture must be constructed manually.

Data preprocessing steps are not part of this project.
Input data should be a dataframe of one predictor (wavenumber nm$^{-^1}$) and one sample per line.
Normalization of x-data (predictor variables) is included in the scripts where necessary.

## Contents

- **`R Markdowns`**:
    - `generalized_additive_model.rmd`: Analysis workflow for optimization and evaluation of GAMs.
    - `convolutional_neural_network.rmd`: Analysis workflow for optimization and evaluation of CNNs.

- **`Scripts`** used for specific tasks:
    - `install_keras_tuner.R`: Provides full code for installation of tensorflow, keras: R interface to keras, kerastuneR: interface to R
    - `cnn_explainability.R`: Computes feature explanation based on variable permutations using the R package LIME. Code for plot
    generation is included.

- **`Functions`** used for specific tasks:
    - `funBasics.R`: contains all functions applicable for both model types.
        - *calc.rmse*: calculation of RMSE
        - *get.folds*: perform stratified folding based on quantiles of predictor data
        - *data.prep*: split input data to training and validation datasets based on stratified sampling
        - *initialize.cache*, *save_to_cache*: included in functions for cross-validations. Creates cache files to create back-up as long as the cross-validation is running
        - *gam.rmse_stats*, *cnn.rmse_stats*: calculates all results referring to RMSE (e.g. mean, variance)
    - `funPlots.R`: contains all plotting functions applicable for both model types.
        - *plot.gam.curves*, *plot.cnn.curves*: generate learning curves to determine optimal number of boosting iterations and perform first performance evaluation
        - *foldwise.plot*: creates prediction vs. targets plots of validation data of individual folds
        - *diagnostic.plots*: creates *prediction vs. targets plot* of all predictions generated with the validation dataset during cross-validation, *Q-Q plot* to assess the normality of residuals, *predictions vs. residuals* and *histogram of residuals* to visualize residual distribution
    - `funGAM.R`: contains all functions necessary for tuning, fitting and evaluating GAMs
    - `funCNN.R`: contains all functions necessary for tuning, fitting and evaluating CNNs

- **`README.md`**: This file, providing an overview of the repository and instructions.

## Requirements

Code development and all statistical analyses were conducted in **RStudio** (2024.09.1+394) with
**R** (4.2.2).
Following packages are required:
- **General**
    - `plyr` - 1.8.9
    - `doParallel` - 1.0.17
    - `tidyr` - 1.3.1
- **Data visualization**
    - `car` - 3.1.2
    - `ggplot2` - 3.5.1
    - `patchwork` - 1.3.0
    - `qqplotr` - 0.0.6
- **Statistical Machine Learning - GAM**
    - `mboost` - 2.9.10
    - `caret` - 6.0.94
- **Statistical Machine Learning - CNN**
    - `reticulate` - 1.39.0
    - `tensorflow` - 2.16.0.9000
    - `keras: R Interface to keras` - 2.15.0
    - `kerastuneR: R Interface to kerastuneR` - 0.1.0.7
    - `lime` - 0.5.3

## License

This repository is licensed under the GNU General Public License v3.0. You may use, distribute, and modify this code under the terms of the GPL-3.0 license. For more details, see the [LICENSE](./LICENSE) file.

## Author
Felicitas Schneider, MSc
65608[at]fhwn.ac.at


