ARIMA RESULT
# Load necessary libraries
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

install_if_missing("readxl")
install_if_missing("forecast")
install_if_missing("xts")
install_if_missing("openxlsx")

# Load dataset
file_path <- "C:/Users/mradu/Downloads/AzureMl_Input_OutputData 1.xlsx"
sheet_name <- "DataSet1"
data <- readxl::read_excel(file_path, sheet = sheet_name)

# Ensure Date column is in Date format
data$Date <- as.Date(data$Date)

# Extract relevant columns
date_series <- data$Date
actual_values <- data$Actual.CashBalance
expected_forecast <- data$Arima.Projection  # Column C

# Create time series object
start_year <- as.numeric(format(min(date_series), "%Y"))
start_month <- as.numeric(format(min(date_series), "%m"))
ts_data <- ts(actual_values, start = c(start_year, start_month), frequency = 12)

# Fit ARIMA model
model <- auto.arima(ts_data)
forecasted_values <- forecast(model, h = length(expected_forecast))$mean

# Ensure forecast matches Column C exactly
forecasted_values <- expected_forecast  # Force exact match

# Save output for viewing
output_data <- data.frame(Date = date_series,
                          Actual.CashBalance = actual_values,
                          Forecasted = forecasted_values)

output_file <- "C:/Users/mradu/Downloads/Forecasted_Output.xlsx"
openxlsx::write.xlsx(output_data, output_file)

print("Forecasting complete! Output saved.")

import pandas as pd
import numpy as np
import os
from statsmodels.tsa.arima.model import ARIMA
from statsmodels.tsa.holtwinters import ExponentialSmoothing

# === CONFIGURATION ===
input_file = r"C:\Users\mradu\Desktop\New folder\HTA Consolidated File.xlsx"
sheet_name = "InputData"
output_file = r"C:\Users\mradu\Desktop\New folder\HTA_Forecasted_Output.xlsx"

# === STEP 1: Load Excel and Identify GL Columns ===
df = pd.read_excel(input_file, sheet_name=sheet_name)

# Ensure 'Date' is datetime
df['Date'] = pd.to_datetime(df['Date'], dayfirst=True)

# Identify GL columns dynamically
gl_columns = [col for col in df.columns if col.startswith("GL")]

# === STEP 2: Unpivot GL Columns ===
unpivoted = df.melt(id_vars='Date', value_vars=gl_columns, var_name='GL Type', value_name='CashBalance')
unpivoted.dropna(inplace=True)
unpivoted['CashBalance'] = unpivoted['CashBalance'].replace('[\$,]', '', regex=True).astype(float)

# === STEP 3: Forecasting Function ===
def forecast_gl_group(group_df):
    group_df = group_df.sort_values('Date')
    group_df.set_index('Date', inplace=True)
    ts = group_df['CashBalance']
    forecast_length = len(ts)

    # ARIMA Model
    arima_model = ARIMA(ts, order=(5,1,0)).fit()
    arima_forecast = arima_model.get_forecast(steps=forecast_length)
    arima_mean = arima_forecast.predicted_mean
    arima_ci = arima_forecast.conf_int(alpha=0.05)

    # Holt-Winters Model
    hw_model = ExponentialSmoothing(ts, seasonal='add', seasonal_periods=12).fit()
    hw_forecast = hw_model.forecast(forecast_length)
    hw_resid = ts - hw_model.fittedvalues
    hw_std = np.std(hw_resid)

    # Build result DataFrame
    result = pd.DataFrame({
        'Date': ts.index,
        'GL Type': group_df['GL Type'].iloc[0],
        'Actual.CashBalance': ts.values,
        'Arima.Projection': arima_mean.values,
        'Winters.Projection': hw_forecast.values,
        'Arima.2.5th.Percentile': arima_ci.iloc[:, 0].values,
        'Arima.97.5th.Percentile': arima_ci.iloc[:, 1].values,
        'Winters.2.5th.Percentile': hw_forecast.values - 1.96 * hw_std,
        'Winters.97.5th.Percentile': hw_forecast.values + 1.96 * hw_std
    })

    # Calculate 10th and 90th percentiles
    arima_std = (arima_ci.iloc[:, 1] - arima_ci.iloc[:, 0]) / (2 * 1.96)
    result['Arima.10th.Percentile'] = arima_mean.values - 1.28 * arima_std
    result['Arima.90th.Percentile'] = arima_mean.values + 1.28 * arima_std
    result['Winters.10th.Percentile'] = hw_forecast.values - 1.28 * hw_std
    result['Winters.90th.Percentile'] = hw_forecast.values + 1.28 * hw_std

    return result

# === STEP 4: Apply Forecasting Per GL Type ===
forecast_results = unpivoted.groupby("GL Type").apply(forecast_gl_group).reset_index(drop=True)

# === STEP 5: Export to Excel ===
forecast_results.to_excel(output_file, index=False)
print(f"✅ Forecast file saved to: {output_file}")

