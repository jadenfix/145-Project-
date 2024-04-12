data <- read.csv("data_11.csv")
head(data)
data$date <- as.Date(data$date, format = "%m/%d/%Y")
df <- data
ncol<-ncol(df)
df <- subset(df, select = 1:(ncol - 4))

#
df <- df[-(1:1008), ]
df <- df[-(829:1457), ]

any(is.na(df))

df$spprice <- as.numeric(df$spprice)
df$infl <- as.numeric(df$infl)
df$lrir10 <- as.numeric(df$lrir10)
df$realtotalreturn <- as.numeric(df$realtotalreturn)
df$realtrscaledearnings <- as.numeric(df$realtrscaledearnings)
df$excessCAPE <- as.numeric(df$excessCAPE)


#install.packages("xts")
#time series
library(xts)
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

#autocorrelation

autocorrelation <- lapply(xts, function(x) acf(x, plot = FALSE))

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

#for stock returns we use: returns=100*pricechange (pricechange=lnrealprice)

xts$realprice <- 100*log(xts$realprice)
xts$realpricelag <- 100*log(xts$realpricelag)
#Autoregressive model:
msmod1 <- lm(xts$realprice ~ xts$realpricelag)
msmod2 <- lm(xts$realprice ~ xts$realpricelag2)
msmod3 <- lm(xts$realprice ~ xts$realpricelag3)
msmod4 <- lm(xts$realprice ~ xts$realpricelag4)
#testing AR models
AIC(msmod1)
AIC(msmod2)
AIC(msmod3)
AIC(msmod4)
#testing # of regimes
ms_model2 <- msmFit(msmod1, k = 2, sw=rep(TRUE,3))
ms_model3 <- msmFit(msmod1, k = 3, sw=rep(TRUE,3))
AIC(ms_model2)
AIC(ms_model3)
#regime 1 is contraction 
#regime 2 is expansion
#2 regime model
par(mar=c(3,3,3,3))
plotProb(ms_model2, which=1)
plotProb(ms_model2, which=2)
plotDiag(ms_model2, regime=1, which=1)
plotDiag(ms_model2, regime=1, which=2)
plotDiag(ms_model2, regime=1, which=3)
plotDiag(ms_model2, regime=2, which=1)
plotDiag(ms_model2, regime=2, which=2)
plotDiag(ms_model2, regime=2, which=3)
#3 regime model
par(mar=c(3,3,3,3))
plotProb(ms_model3, which=1)
plotProb(ms_model3, which=2)
plotProb(ms_model3, which=3)
plotDiag(ms_model3, regime=1, which=1)
plotDiag(ms_model3, regime=1, which=2)
plotDiag(ms_model3, regime=1, which=3)
plotDiag(ms_model3, regime=2, which=1)
plotDiag(ms_model3, regime=2, which=2)
plotDiag(ms_model3, regime=2, which=3)
plotDiag(ms_model3, regime=3, which=1)
plotDiag(ms_model3, regime=3, which=2)
plotDiag(ms_model3, regime=3, which=3)

#smooth and filtered probs of 2 regime
smoothprobms2 <- ms_model2@Fit@smoProb
filterprobms2 <- ms_model2@Fit@filtProb









