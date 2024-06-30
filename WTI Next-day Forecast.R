library(quantmod)
library(forecast)
library(ggplot2)

# Data WTI
getSymbols("CL=F", src = "yahoo", from = "2023-01-01", to = Sys.Date())
oil_prices <- Cl(`CL=F`)

# Data fix
oil_prices <- na.approx(oil_prices)
oil_prices[oil_prices <= 0] <- min(oil_prices[oil_prices > 0]) / 2

# Log
log_oil_prices <- log(oil_prices)

# dataframes
oil_prices_df <- data.frame(Date = index(oil_prices), Price = as.numeric(oil_prices))
log_oil_prices_df <- data.frame(Date = index(log_oil_prices), LogPrice = as.numeric(log_oil_prices))

# Priceplot
ggplot(oil_prices_df, aes(x = Date, y = Price)) +
  geom_line() +
  labs(title = "Historical Crude Oil Prices", y = "Price", x = "Year") +
  theme_minimal()

# Arima model
arima_model <- auto.arima(log_oil_prices, seasonal = FALSE)

# Next-day forecast
log_forecast <- forecast(arima_model, h = 1)
next_day_forecast <- data.frame(
  Date = index(log_oil_prices)[length(log_oil_prices)] + 1,
  Mean = exp(log_forecast$mean),
  Lower = exp(log_forecast$lower[, 2]),
  Upper = exp(log_forecast$upper[, 2])
)

print(next_day_forecast)


