# =============================================================
# Project: Tail Risk in Pakistani Financial Markets
# Script 1: Data Download, Cleaning, and Alignment
# Author: Ahmer | GitHub: ahmer-econ | August 2026
# =============================================================

setwd("D:/Documents/APPLIED ECONOMETRICS WORK/VaR_ES_Pakistan Model")

# --- Load libraries ---
library(quantmod)
library(dplyr)
library(lubridate)
library(readr)

# =============================================================
# PART 1: KSE-100 — Load from Kaggle CSV
# =============================================================

# Always check exact filename first
list.files("data_raw/")

# Load the CSV
kse_raw <- read_csv("data_raw/KSE100-20years.csv")

# Inspect
head(kse_raw)
tail(kse_raw)
str(kse_raw)
colnames(kse_raw)

# =============================================================
# =============================================================
# PART 2: KSE-100 — Clean
# =============================================================

kse_clean <- kse_raw %>%
  # Standardise column names to lowercase
  rename_with(tolower) %>%
  # Parse date — adjust format if needed after inspecting head()
  mutate(date = as.Date(date, format = "%d-%b-%y")) %>%
  # Keep only date and close price
  select(date, close) %>%
  rename(kse_close = close) %>%
  # Remove rows where close is zero or NA (data errors seen in raw file)
  filter(!is.na(kse_close), kse_close > 0) %>%
  # Filter to our sample period
  filter(date >= as.Date("2010-01-01") & date <= as.Date("2024-08-31")) %>%
  # Sort ascending
  arrange(date)

# Inspect cleaned KSE
head(kse_clean)
tail(kse_clean)
nrow(kse_clean)
sum(is.na(kse_clean))
# =============================================================
# PART 3: COMEX Gold — Download from Yahoo Finance
# =============================================================

getSymbols("GC=F",
           src    = "yahoo",
           from   = "2010-01-01",
           to     = "2024-08-31",
           auto.assign = TRUE)

# Inspect raw download
head(`GC=F`)
tail(`GC=F`)
# =============================================================
# PART 4: COMEX Gold — Clean
# =============================================================

gold_clean <- data.frame(
  date      = index(`GC=F`),
  gold_close = as.numeric(`GC=F`[, 4])  # Column 4 = Close price
) %>%
  filter(!is.na(gold_close), gold_close > 0) %>%
  filter(date >= as.Date("2010-01-01") & date <= as.Date("2024-08-31")) %>%
  arrange(date)

# Inspect cleaned Gold
head(gold_clean)
tail(gold_clean)
nrow(gold_clean)
sum(is.na(gold_clean))
# =============================================================
# PART 5: Align — Keep only dates common to both series
# =============================================================

# Inner join on date — only trading days present in BOTH series
combined <- inner_join(kse_clean, gold_clean, by = "date")

# Inspect aligned dataset
head(combined)
tail(combined)
nrow(combined)
cat("KSE-100 obs before alignment:", nrow(kse_clean), "\n")
cat("Gold obs before alignment:    ", nrow(gold_clean), "\n")
cat("Common obs after alignment:   ", nrow(combined), "\n")

# Check for any remaining NAs
colSums(is.na(combined))
# =============================================================
# PART 6: Save clean files
# =============================================================

write_csv(kse_clean,  "data_clean/kse100_clean.csv")
write_csv(gold_clean, "data_clean/gold_clean.csv")
write_csv(combined,   "data_clean/combined_aligned.csv")

cat("All clean files saved to data_clean/ \n")

