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
xtsdata1 <- subset(xts, select = -c(CAPE, excessCAPE, realearnings))

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
#Markov process: The time of transition from one state to another
#and the duration between changes in state is random.
#MSM################################################################################


# Load required libraries
library(msm)
library(dplyr)
library(tseries)
library(sandwich)
library(MSwM)
library(car)

# Define the MS-AR(1) model equation for realprice

msmmod1 <- lm(xtsdata1$realprice ~ 1 + lag(xtsdata1$realprice, 1) + lag(xtsdata1$infl, 1) + lag(xtsdata1$lrir10, 1) + lag(xtsdata1$realdividend, 1) + lag(xtsdata1$realtotalreturn, 1) + lag(xtsdata1$realtrscaledearnings, 1) + lag(xtsdata1$TRCAPE, 1) + lag(xtsdata1$realbond, 1), data=xtsdata1)
msmmod2 <- lm(xtsdata1$realprice ~ 1 + lag(xtsdata1$realprice, 2) + lag(xtsdata1$infl, 2) + lag(xtsdata1$lrir10, 2) + lag(xtsdata1$realdividend, 2) + lag(xtsdata1$realtotalreturn, 2) + lag(xtsdata1$realtrscaledearnings, 2) + lag(xtsdata1$TRCAPE, 2) + lag(xtsdata1$realbond, 2), data=xtsdata1)
msmmod3 <- lm(xtsdata1$realprice ~ 1 + lag(xtsdata1$realprice, 3) + lag(xtsdata1$infl, 3) + lag(xtsdata1$lrir10, 3) + lag(xtsdata1$realdividend, 3) + lag(xtsdata1$realtotalreturn, 3) + lag(xtsdata1$realtrscaledearnings, 3) + lag(xtsdata1$TRCAPE, 3) + lag(xtsdata1$realbond, 3), data=xtsdata1)
msmmod6 <- lm(xtsdata1$realprice ~ 1 + lag(xtsdata1$realprice, 6) + lag(xtsdata1$infl, 6) + lag(xtsdata1$lrir10, 6) + lag(xtsdata1$realdividend, 6) + lag(xtsdata1$realtotalreturn, 6) + lag(xtsdata1$realtrscaledearnings, 6) + lag(xtsdata1$TRCAPE, 6) + lag(xtsdata1$realbond, 6), data=xtsdata1)
msmmod12 <- lm(xtsdata1$realprice ~ 1 + lag(xtsdata1$realprice, 12) + lag(xtsdata1$infl, 12) + lag(xtsdata1$lrir10, 12) + lag(xtsdata1$realdividend, 12) + lag(xtsdata1$realtotalreturn, 12) + lag(xtsdata1$realtrscaledearnings, 12) + lag(xtsdata1$TRCAPE, 12) + lag(xtsdata1$realbond, 12), data=xtsdata1)
msmmod18 <- lm(xtsdata1$realprice ~ 1 + lag(xtsdata1$realprice, 18) + lag(xtsdata1$infl, 18) + lag(xtsdata1$lrir10, 18) + lag(xtsdata1$realdividend, 18) + lag(xtsdata1$realtotalreturn, 18) + lag(xtsdata1$realtrscaledearnings, 18) + lag(xtsdata1$TRCAPE, 18) + lag(xtsdata1$realbond, 18), data=xtsdata1)

#residuals plot
residuals1 <- residuals(msmmod1)
residuals2 <- residuals(msmmod2)
residuals3 <- residuals(msmmod3)
residuals6 <- residuals(msmmod6)
residuals12 <- residuals(msmmod12)
residuals18 <- residuals(msmmod18)

#plot(residuals1, xlab="Time", ylab="Residuals", main = "Residuals Plot")
#plot(residuals(msmmod1), xlab = "Time", ylab = "Residuals", main = "Residuals Plot")
#vif testing: 
vif_valuesmsmmod1 <- car::vif(msmmod1)
print(vif_valuesmsmmod1)
# Estimate the MS-AR(1) model
#SW true means parameters can be different in each regime 
ms_model1 <- msmFit(msmmod1, k = 2, sw=rep(TRUE,10))
ms_model2 <- msmFit(msmmod2, k = 2, sw=rep(TRUE,10))
ms_model3 <- msmFit(msmmod3, k = 2, sw=rep(TRUE,10))
ms_model6 <- msmFit(msmmod6, k = 2, sw=rep(TRUE,10))
ms_model12 <- msmFit(msmmod12, k = 2, sw=rep(TRUE,10))
ms_model18 <- msmFit(msmmod18, k = 2, sw=rep(TRUE,10))
#AIC
AIC(ms_model1)
AIC(ms_model2)
AIC(ms_model3)
AIC(ms_model6)
AIC(ms_model12)
AIC(ms_model18)
#BIC
BIC(ms_model1)
BIC(ms_model2)
BIC(ms_model3)
BIC(ms_model6)
BIC(ms_model12)
BIC(ms_model18)
#summaries of models 
summary(ms_model1)
summary(ms_model2)
summary(ms_model3)
summary(ms_model6)
summary(ms_model12)
summary(ms_model18)
#filtered and smoothed probs 
#ms 1
#visualization 
par(mar=c(3,3,3,3))
plotProb(ms_model1, which=1)
plotProb(ms_model1, which=2)
plotDiag(ms_model1, regime=1, which=1)
plotDiag(ms_model1, regime=1, which=2)
plotDiag(ms_model1, regime=1, which=3)
plotDiag(ms_model1, regime=2, which=1)
plotDiag(ms_model1, regime=2, which=2)
plotDiag(ms_model1, regime=2, which=3)
#ms 2
plotProb(ms_model2, which=1)
plotProb(ms_model2, which=2)
plotProb(ms_model3, which=1)
plotProb(ms_model3, which=2)
plotProb(ms_model6, which=1)
plotProb(ms_model6, which=2)
plotProb(ms_model12, which=1)
plotProb(ms_model12, which=2)
plotProb(ms_model18, which=1)
plotProb(ms_model18, which=2)



filtered_probs1 <- msmFilter(ms_model1)
smoothed_probs1 <- msmSmooth(ms_model1)
#ms2
filtered_probs2 <- msmFilter(ms_model2)
smoothed_probs2 <- msmSmooth(ms_model2)
#ms3
filtered_probs3 <- msmFilter(ms_model3)
smoothed_probs3 <- msmSmooth(ms_model3)
#ms6
filtered_probs6 <- msmFilter(ms_model6)
smoothed_probs6 <- msmSmooth(ms_model6)
#ms12
filtered_probs12 <- msmFilter(ms_model12)
smoothed_probs12 <- msmSmooth(ms_model12)
#ms18
filtered_probs18 <- msmFilter(ms_model18)
smoothed_probs18 <- msmSmooth(ms_model18)
# Create a data frame for the results
result_table <- data.frame(
  Regime = rep(c("Regime 1", "Regime 2"), each = ncol(ms_model1) / 2),
  ms_model1
)

# Print the result table
kable(result_table, format = "markdown", digits = 3)


plot(ms_model2, xlab = "Time", ylab="Residuals")


#autoregression: 
msm_data <- data.frame(y= c(xtsdata$realprice))
msm_base <- lm(realprice~1, data=msm_data)
msm_model <- msmFit(msm_base, k=2, sw=rep(TRUE, 3), p=1)
#AR Results
par(mar=c(3,3,3,3))
plotProb(msm_model, which=1)
plotProb(msm_model, which=2)
plotDiag(msm_model, regime=1, which=1)
plotDiag(msm_model, regime=1, which=2)
plotDiag(msm_model, regime=1, which=3)
plotDiag(msm_model, regime=2, which=1)
plotDiag(msm_model, regime=2, which=2)
plotDiag(msm_model, regime=2, which=3)
#ms 2
#predictive regression: 
# Forecast and regime probabilities
forecast_probs <- msmForecast(ms_model1, n.ahead = 1)  # Change n.ahead as needed

# Extract bear market probabilities
bear_market_prob <- forecast_probs$filtered$Prob[2, 2]  # Probability of being in regime 2 (bear market)

# Print the bear market probability
print(bear_market_prob)

xt <- c(
  1,                                            # Intercept
  lag(xtsdata1$realprice, 1),                   # lag(xtsdata1$realprice, 1)
  lag(xtsdata1$infl, 1),                        # lag(xtsdata1$infl, 1)
  lag(xtsdata1$lrir10, 1),                      # lag(xtsdata1$lrir10, 1)
  lag(xtsdata1$realdividend, 1),                # lag(xtsdata1$realdividend, 1)
  lag(xtsdata1$realtotalreturn, 1),             # lag(xtsdata1$realtotalreturn, 1)
  lag(xtsdata1$realtrscaledearnings, 1),        # lag(xtsdata1$realtrscaledearnings, 1)
  lag(xtsdata1$TRCAPE, 1),                      # lag(xtsdata1$TRCAPE, 1)
  lag(xtsdata1$realbond, 1)                     # lag(xtsdata1$realbond, 1)
)

# Combine the predictor variables into a matrix
predictor_matrix <- cbind(xt)

# Estimate the predictive regression model
predictive_regression <- lm(bear_market_prob ~ ., data = cbind(predictor_matrix, bear_market_prob = xtsdata1$bear_market_prob))

# Conduct the in-sample tests
library(sandwich)
library(lmtest)
in_sample_test <- coeftest(predictive_regression, vcov. = NeweyWest(predictive_regression, lag = NULL, prewhite = FALSE))
