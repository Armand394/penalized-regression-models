library(readxl) #package to read file
library(openxlsx)

# Import data Brazil
brazil_indep_data <- read_xlsx("C:/Users/external/Desktop/Y4B3/Seminar in ML/Coding/Data/Brazil - Independent Variables.xlsx")

# Import data Costa Rica
cr_indep_data <- read_xlsx("C:/Users/external/Desktop/Y4B3/Seminar in ML/Coding/Data/Costa Rica - Independent Variables.xlsx")

# Data transform
x_brazil <- as.matrix(brazil_indep_data[,-1]) 
x_cr <- as.matrix(cr_indep_data[,-1]) 

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


# choose number of factors using ICT
ICT <- function(k, V_k, n, T, c) {
  log_V_k <- log(V_k)
  penalty_term <- c * k * ((n + T)/(n * T)) * log((n * T)/(n + T))
  ICT <- log_V_k + penalty_term
  return(ICT)
}


number_of_factors_tuning <- function(x, country_name){
  
  # Initialize an empty dataframe
  result_df <- data.frame(c_values = numeric(),
                          criterion_values = numeric(),
                          factor_chosen = numeric())
  # Parameters
  T = nrow(x)
  n = ncol(x)
  
  c <- 0.8
  
  while(c < 3.0){
    
    # Initiate vector of criterion values
    ICT_values <- numeric(20)
    
    for(k in 1:25){ 
      # Simple principal component estimation with k factors
      pca_result <- prcomp(x=x, rank=k)
      
      # Estimated factor and loading
      loadings <- pca_result$rotation
      factors <- pca_result$x
      
      # Matrix multiplication
      reconstructed_data <- factors %*% t(loadings)
      
      # Compute V_k
      V_k <- 0
      
      # Nested for loops to access all elements
      for (i in 1:n) {
        for (j in 1:T) {
          V_k <- V_k + (x[j,i] - reconstructed_data[j,i])^2
        }
      }
      
      V_k <- (1/(n*T))*V_k
      ICT_values[k] <- ICT(k, V_k, n, T, c)
    }
    
    # Get the index of the minimum element
    min_index <- which.min(ICT_values)
    
    # Store the results in the dataframe
    result_df <- rbind(result_df, data.frame(c_values = c,
                                             criterion_values = ICT_values[min_index],
                                             factor_chosen = min_index))
    
    c <- c + 0.01
  }
  
  file <- paste0("C:/Users/external/Desktop/Y4B3/Seminar in ML/Coding/Results/factor_tuning_", country_name,".xlsx")
  
  # Write the dataframe to an Excel file
  write.xlsx(result_df, file = file, rowNames = FALSE)

}

number_of_factors_tuning(x_brazil, "brazil")
number_of_factors_tuning(x_cr, "costa_rica")




