data <- read.csv("data_1.csv")
head(data)
data$date <- as.Date(data$date, format = "%m/%d/%Y")
df <- data
ncol<-ncol(df)
df <- subset(df, select = 1:(ncol - 4))

#
df <- df[-(1:120), ]
df <- df[-(1717:2345), ]

any(is.na(df))

df$spprice <- as.numeric(df$spprice)
df$infl <- as.numeric(df$infl)
df$lrir10 <- as.numeric(df$lrir10)
df$realtotalreturn <- as.numeric(df$realtotalreturn)
df$realtrscaledearnings <- as.numeric(df$realtrscaledearnings)
df$excessCAPE <- as.numeric(df$excessCAPE)

df <- subset(df, select = -c(spprice, dividend, earnings, bond, dp, dy))

#install.packages("xts")
#time series
library(xts)
df$infl <- log(df$infl)
xts <- xts(df[,-1], order.by = df$date)
ts <- as.ts(xts)

#plots
library(plotly)
# Plotting
#SPPprice
plot_ly(data = df, x = ~date, y = ~realprice, type = 'scatter', mode = 'lines') %>%
  layout(title = "Real S&P 500 Price",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Real USD"))
#Real Dividend 
plot_ly(data = df, x = ~date, y = ~realdividend, type = 'scatter', mode = 'lines') %>%
  layout(title = "Real Dividend",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Real USD"))
#Real TR Scaled Earnings 
plot_ly(data = df, x = ~date, y = ~realtrscaledearnings, type = 'scatter', mode = 'lines') %>%
  layout(title = "Real Total Revenue Scaled Earnings",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Real USD"))
#TR Cape Ratio 
plot_ly(data = df, x = ~date, y = ~TRCAPE, type = 'scatter', mode = 'lines') %>%
  layout(title = "Total Revenue Scaled Cyclically Adjusted Price Earnings Ratio",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Ratio"))
#Real Bond Return
plot_ly(data = df, x = ~date, y = ~realbond, type = 'scatter', mode = 'lines') %>%
  layout(title = "Real Bond Price",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Real USD"))
#inflation 
plot_ly(data = df, x = ~date, y = ~infl, type = 'scatter', mode = 'lines') %>%
  layout(title = "Inflation",
         xaxis = list(title = "Date"),
         yaxis = list(title = "CPI"))
#Long Term Interest Rate 
plot_ly(data = df, x = ~date, y = ~lrir10, type = 'scatter', mode = 'lines') %>%
  layout(title = "Long Range Interest Rates (10 YR)",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Percentage"))

#correlation matrix
cor_matrix <- cor(xts)
print(cor_matrix)
write.csv(cor_matrix, file = "correlation_matrix.csv")

#Modified COR MATRIX 
data <- df
data <- subset(data, select = -c(realearnings, CAPE, excessCAPE, realtotalreturn))
xtsdata <- xts(data[,-1], order.by = data$date)

#autocorrelation

autocorrelation <- lapply(xtsdata, function(x) acf(x, plot = FALSE))

print(autocorrelation)

#install.packages("knitr")
library(knitr)

# Convert autocorrelation results into a data frame
autocorrelation_df <- lapply(autocorrelation, function(x) data.frame(Lag = x$lag, Autocorrelation = x$acf))

# Print the formatted table using kable
for (i in seq_along(autocorrelation_df)) {
  print(kable(autocorrelation_df[[i]], caption = names(autocorrelation_df)[i], format = "markdown"))
}

# Convert autocorrelation results into a data frame
autocorrelation_df <- lapply(autocorrelation, function(x) data.frame(Lag = x$lag, Autocorrelation = x$acf))

# Export each autocorrelation table to a CSV file
for (i in seq_along(autocorrelation_df)) {
  filename <- paste0(names(autocorrelation_df)[i], "_autocorrelation.csv")
  write.csv(autocorrelation_df[[i]], file = filename, row.names = FALSE)
}
xtsdata

#statistical testing: ADF and Phillips Perron 
#install.packages("urca")
library(urca)
# List to store test results
adf_results <- list()
pp_results <- list()

# Define the variables
variables <- c("infl", "lrir10", "realprice", "realdividend", "realtrscaledearnings", "TRCAPE", "realbond")

# Loop through each variable
for (var in variables) {
  # ADF test
  adf_results[[var]] <- ur.df(df[[var]], type = "none", selectlags = "AIC")
  
  # PP test
  pp_results[[var]] <- ur.pp(df[[var]], type = "Z-alpha")
}

# Print the ADF and PP test results
for (var in variables) {
  cat("Variable:", var, "\n")
  cat("ADF Test:\n")
  print(adf_results[[var]])
  cat("\nPP Test:\n")
  print(pp_results[[var]])
  cat("\n\n")
}
#stat test ADF and PP
results_df <- data.frame(Variable = character(),
                         ADF_Test_Statistic = numeric(),
                         PP_Test_Statistic = numeric(),
                         stringsAsFactors = FALSE)

# Loop through each variable
for (var in variables) {
  # ADF test
  adf_test_result <- summary(adf_results[[var]])
  adf_test_statistic <- adf_test_result@teststat[1]  # Get test statistic from ADF results
  
  # PP test
  pp_test_result <- summary(pp_results[[var]])
  pp_test_statistic <- pp_test_result@teststat[1]   # Get test statistic from PP results
  
  # Combine results into a df
  var_results <- data.frame(Variable = var,
                            ADF_Test_Statistic = adf_test_statistic,
                            PP_Test_Statistic = pp_test_statistic)
  
  # Bind to the main data frame
  results_df <- rbind(results_df, var_results)
}

# Print the results
print(results_df)

write.csv(results_df, file="stattest.csv")

###########################Data Visualization Stops Here ########### MSM Econometrics Begin 

#MSM################################################################################


# Load required libraries
library(msm)
library(dplyr)

# Define the MS-AR(1) model equation for realprice
# Replace 'independent_variable1', 'independent_variable2', etc. with actual variable names
model_formula <- as.formula("realprice ~ 1 + lag(realprice, 1) + lag(infl, 1) + lag(lrir10, 1) + lag(realdividend, 1) + lag(realtrscaledearnings, 1) + lag(TRCAPE, 1) + lag(realbond, 1)")

# Estimate the MS-AR(1) model
ms_model <- msmFit(model_formula, data = your_data_frame, k = 2, p = 1)  # Assuming 2 states and lag length of 1

# Extract the estimated parameters
estimated_params <- coef(ms_model)

# Identify bull and bear markets based on the estimated parameters
bull_market <- estimated_params$mean[1] > estimated_params$mean[2] & estimated_params$sigma[1] < estimated_params$sigma[2]
bear_market <- !bull_market

# Calculate the filtered probabilities of each state
filtered_probabilities <- predict(ms_model)

# Display the results
print("Bull Market:")
print(bull_market)
print("Bear Market:")
print(bear_market)
print("Filtered Probabilities:")
print(head(filtered_probabilities$states))
