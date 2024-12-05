library(readxl) #package to read file
library(glmnet) #package for lasso and elastic net 
library(forecast) #package for time series forecasting
library(moments)
library(elasticnet)
library(sparsepca)
library(vars)
library(tsDyn)
library(bestNormalize)


# Import data Brazil
brazil_indep_data <- read_xlsx("C:/Users/external/Desktop/Y4B3/Seminar in ML/Coding/Data/Brazil - Independent Variables.xlsx")
brazil_dep_data <- read_xlsx("C:/Users/external/Desktop/Y4B3/Seminar in ML/Coding/Data/Brazil - Dependent Variables.xlsx")

# Import data Costa Rica
cr_indep_data <- read_xlsx("C:/Users/external/Desktop/Y4B3/Seminar in ML/Coding/Data/Costa Rica - Independent Variables.xlsx")
cr_dep_data <- read_xlsx("C:/Users/external/Desktop/Y4B3/Seminar in ML/Coding/Data/Costa Rica - Dependent Variables.xlsx")

# Data transform
x_brazil <- as.matrix(brazil_indep_data[,-1]) 
x_cr <- as.matrix(cr_indep_data[,-1]) 
y_brazil <- as.matrix(brazil_dep_data[,-1])
y_cr <- as.matrix(cr_dep_data[,-1]) 

# Normalize Data
x_brazil <- apply(x_brazil, 2, function(col) {
  yt <- yeojohnson(col)
  return(yt$x.t)
})
# Normalize Data
x_cr <- apply(x_cr, 2, function(col) {
  yt <- yeojohnson(col)
  return(yt$x.t)
})

#Extract dependent variables
rgdp_brazil <- y_brazil[,1]
rdgp_cr <- y_cr[,1]
cpi_brazil <- y_brazil[,2]
cpi_cr <- y_cr[,2]

#Apply log differences transformation to dependent variable (real series)
log_rgdp_brazil <- log(rgdp_brazil)
log_diff_rgdp_brazil <- diff(log_rgdp_brazil)

#Apply log differences transformation to dependent variable (real series)
log_rgdp_cr <- log(rdgp_cr)
log_diff_rgdp_cr <- diff(log_rgdp_cr)

#log second differences (price indices)
log_cpi_brazil <- log(cpi_brazil)
log_diff_cpi_brazil <- diff(log_cpi_brazil)
log_diff2_cpi_brazil <- diff(log_diff_cpi_brazil)

# Log second differences (price indices)
log_cpi_cr <- log(cpi_cr)
log_diff_cpi_cr <- diff(log_cpi_cr)
log_diff2_cpi_cr <- diff(log_diff_cpi_cr)

# Adjust size of independent variables
x_brazil_rgdp <- x_brazil[-1,]
x_cr_rdgp <- x_cr[-1,]
x_brazil_cpi <- x_brazil[-c(1, 2),]
x_cr_cpi <- x_cr[-c(1, 2),]



# Function for rolling window
rolling_window_forecast_PCA <- function(x,y,k_opt){
  # Starting Settings for rolling window
  h <- 1
  n <- length(y)
  start_window <- 40
  
  # Initiate vector to store forecast and errors
  forecasts <- rep(NA, n - start_window - h + 1)
  
  # Settings for PCA estimation (tuned)
  k <- k_opt
  
  # Loop for forecasting using SPCA
  for (i in 1:(n - start_window - h + 1)) {
    train_window <- i:(i + start_window - 1)
    
    # Extract data for training
    x_train <- x[train_window,]
    y_train <- y[train_window]
    
    # Simple principal component estimation with k factors
    pca_result <- prcomp(x=x_train, rank=k_opt)
    
    # Estimated factor
    factors <- pca_result$x
    
    # Retrieve Factors
    factors_train_df <- data.frame(factors)
    
    # Fit model using estimated factors
    lm_model <- lm(y_train ~ ., data = factors_train_df)
    
    # Project test data onto the Sparse PCA loadings for forecasting
    x_test <- x[(i + start_window),]
    
    # Acquire test factor using estimated Loading
    factors <- x_test %*% pca_result$rotation
    factor_test_df <- data.frame(factors)
    
    # Predict using the linear model and the structured scores_test_df
    prediction <- predict(lm_model, newdata = factor_test_df)
    
    # Assign 1-step ahead forecast to vector
    forecasts[i] <- prediction

    
  }
  
  # Mean squared error
  actual_values <- y[(start_window+1):n]
  mse <- (actual_values - forecasts)^2
  mse <- mean(mse, na.rm = TRUE)
  
  print(mse)
}


rolling_window_forecast_PCA(x_brazil_rgdp, log_diff_rgdp_brazil, 2)
rolling_window_forecast_PCA(x_cr_rdgp, log_diff_rgdp_cr, 1)
rolling_window_forecast_PCA(x_brazil_cpi, log_diff2_cpi_brazil, 2)
rolling_window_forecast_PCA(x_cr_cpi, log_diff2_cpi_cr, 1)
