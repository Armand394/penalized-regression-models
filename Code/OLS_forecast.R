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
x_cr_rgdp <- x_cr[-1,]
x_brazil_cpi <- x_brazil[-c(1, 2),]
x_cr_cpi <- x_cr[-c(1, 2),]


rolling_window_forecast_ols <- function(x,y){
  # Starting Settings for rolling window
  h <- 1
  n <- length(y)
  start_window <- 40
  
  forecasts_ols <- rep(NA, n - start_window - h + 1)
  
  # Forecasting using rolling window
  for(i in 1:(n - start_window - h + 1)) {
    train_index <- i:(i + start_window - 1)
    
    x_train <- x[train_index,]
    y_train <- y[train_index]
    
    ols <- lm(y_train ~ x_train) #or: x-train - 1
    
    x_test <- x[i + start_window,]
    prediction <- predict(ols, newx = x_test)
    
    # Assign 1-step ahead forecast to vector
    forecasts_ols[i] <- prediction
  
  }
  
  # Mean squared error
  actual_values <- y[(start_window+1):n]
  mse <- (actual_values - forecasts_ols)^2
  mse <- mean(mse, na.rm = TRUE)
  
  
  print(mse)
}

rolling_window_forecast_ols(x_brazil_rgdp, log_diff_rgdp_brazil)
rolling_window_forecast_ols(x_cr_rgdp, log_diff_rgdp_cr)
rolling_window_forecast_ols(x_brazil_cpi, log_diff2_cpi_brazil)
rolling_window_forecast_ols(x_cr_cpi, log_diff2_cpi_cr)



