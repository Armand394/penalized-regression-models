library(readxl) #package to read file
library(glmnet) #package for lasso and elastic net 
library(forecast) #package for time series forecasting
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
rolling_window_forecast_lasso <- function(x,y, lambda_adla, lambda_ridge){
  
  # Find initial weights for adaptive lasso with tuned ridge lambda
  lambda_ridge <- lambda_ridge
  ridge_model <- glmnet(x = x, y = y, alpha = 0, lambda = lambda_ridge)
  ridge_weights <- as.numeric(coef(ridge_model, s = ridge_model$lambda.min))[-1]
  
  # Forecast rolling window settings
  h <- 1
  n <- length(y)
  start_window <- 40
  
  # Initiate forecast vector
  forecasts <- rep(NA, n - start_window - h)
  
  for(i in 1:(n - start_window - h + 1)) {
    train_index <- i:(i + start_window - 1)
    test_index <- i + start_window
    
    x_train <- x[train_index,]
    y_train <- y[train_index]
    
    # Estimate using lambda
    alasso_model <- glmnet(x = x_train, y = y_train, alpha = 1, lambda = lambda_adla, penalty.factor = 1 / abs(ridge_weights))
    
    # Forecast 1-step ahead
    x_test <- x[test_index,]
    prediction <- predict(alasso_model, s = lambda_adla, newx = x_test)
    forecasts[i] <- prediction
    
  }
  
  # Mean squared error
  actual_values <- y[(start_window+1):n]
  mse <- (actual_values - forecasts)^2
  mse <- mean(mse, na.rm = TRUE)
  
  print(mse)
  
}

# Brazil
lambda_ridge_brazil_rgdp <- 0.00178865
lambda_adla_brazil_rgdp <- 0.001

lambda_ridge_brazil_cpi <- 100
lambda_adla_brazil_cpi <- 0.005094138

# Costa Rica
lambda_ridge_cr_rgdp <- 0.001417474
lambda_adla_cr_rgdp <- 0.001

lambda_ridge_cr_cpi <- 0.0367838
lambda_adla_cr_cpi <- 0.01450829


rolling_window_forecast_lasso(x_brazil_rgdp, log_diff_rgdp_brazil, lambda_adla_brazil_rgdp, lambda_ridge_brazil_rgdp)
rolling_window_forecast_lasso(x_cr_rdgp, log_diff_rgdp_cr, lambda_adla_cr_rgdp, lambda_ridge_cr_rgdp)
rolling_window_forecast_lasso(x_brazil_cpi, log_diff2_cpi_brazil, lambda_adla_brazil_cpi, lambda_ridge_brazil_cpi)
rolling_window_forecast_lasso(x_cr_cpi, log_diff2_cpi_cr, lambda_adla_cr_cpi, lambda_ridge_cr_cpi)

