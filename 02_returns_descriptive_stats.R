# =============================================================
# Project: Tail Risk in Pakistani Financial Markets
# Script 2: Return Series and Descriptive Statistics
# Author: Ahmer | GitHub: ahmer-econ | August 2026
# =============================================================

setwd("D:/Documents/APPLIED ECONOMETRICS WORK/VaR_ES_Pakistan Model")

# --- Load libraries ---
library(readr)
library(dplyr)
library(ggplot2)
library(moments)    # for skewness, kurtosis, jarque.test
library(tseries)    # for jarque.bera.test

# =============================================================
# PART 1: Load aligned data and compute log returns
# =============================================================

combined <- read_csv("data_clean/combined_aligned.csv")

# Compute daily log returns (in percent)
returns <- combined %>%
  mutate(
    kse_ret  = c(NA, diff(log(kse_close)))  * 100,
    gold_ret = c(NA, diff(log(gold_close))) * 100
  ) %>%
  filter(!is.na(kse_ret), !is.na(gold_ret))

# Inspect
head(returns)
tail(returns)
nrow(returns)
# =============================================================
# PART 2: Descriptive Statistics Table
# =============================================================

desc_stats <- data.frame(
  Statistic = c("Observations", "Mean (%)", "Std Dev (%)",
                "Minimum (%)", "Maximum (%)",
                "Skewness", "Excess Kurtosis",
                "Jarque-Bera Statistic", "Jarque-Bera p-value"),
  KSE100 = c(
    nrow(returns),
    round(mean(returns$kse_ret), 4),
    round(sd(returns$kse_ret), 4),
    round(min(returns$kse_ret), 4),
    round(max(returns$kse_ret), 4),
    round(skewness(returns$kse_ret), 4),
    round(kurtosis(returns$kse_ret) - 3, 4),
    round(jarque.bera.test(returns$kse_ret)$statistic, 4),
    round(jarque.bera.test(returns$kse_ret)$p.value, 6)
  ),
  COMEX_Gold = c(
    nrow(returns),
    round(mean(returns$gold_ret), 4),
    round(sd(returns$gold_ret), 4),
    round(min(returns$gold_ret), 4),
    round(max(returns$gold_ret), 4),
    round(skewness(returns$gold_ret), 4),
    round(kurtosis(returns$gold_ret) - 3, 4),
    round(jarque.bera.test(returns$gold_ret)$statistic, 4),
    round(jarque.bera.test(returns$gold_ret)$p.value, 6)
  )
)

print(desc_stats)

# Save descriptive stats table
write_csv(desc_stats, "outputs/tables/descriptive_statistics.csv")
cat("Descriptive statistics saved.\n")

# =============================================================
# PART 3: Time Series Plot — KSE-100 Returns
# =============================================================

p1 <- ggplot(returns, aes(x = date, y = kse_ret)) +
  geom_line(color = "#2C7BB6", linewidth = 0.4, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  labs(
    title    = "KSE-100 Daily Log Returns (2010–2024)",
    subtitle = "Volatility clustering visible around crisis episodes",
    x        = "Date",
    y        = "Log Return (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40")
  )

ggsave("outputs/plots/01_kse100_returns.png",
       plot = p1, width = 10, height = 4, dpi = 150)
cat("Plot 1 saved.\n")

# =============================================================
# PART 4: Time Series Plot — COMEX Gold Returns
# =============================================================

p2 <- ggplot(returns, aes(x = date, y = gold_ret)) +
  geom_line(color = "#D4A017", linewidth = 0.4, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  labs(
    title    = "COMEX Gold Daily Log Returns (2010–2024)",
    subtitle = "Notable spike during COVID-19 (March 2020)",
    x        = "Date",
    y        = "Log Return (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40")
  )

ggsave("outputs/plots/02_gold_returns.png",
       plot = p2, width = 10, height = 4, dpi = 150)
cat("Plot 2 saved.\n")

# =============================================================
# PART 5: Histogram with Normal Overlay — KSE-100
# =============================================================

kse_mean <- mean(returns$kse_ret)
kse_sd   <- sd(returns$kse_ret)

p3 <- ggplot(returns, aes(x = kse_ret)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 80, fill = "#2C7BB6",
                 color = "white", alpha = 0.7) +
  stat_function(
    fun  = dnorm,
    args = list(mean = kse_mean, sd = kse_sd),
    color = "red", linewidth = 1, linetype = "dashed"
  ) +
  labs(
    title    = "KSE-100 Return Distribution vs Normal",
    subtitle = "Red dashed line = fitted normal distribution",
    x        = "Log Return (%)",
    y        = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40")
  )

ggsave("outputs/plots/03_kse100_histogram.png",
       plot = p3, width = 8, height = 5, dpi = 150)
cat("Plot 3 saved.\n")

# =============================================================
# PART 6: Histogram with Normal Overlay — COMEX Gold
# =============================================================

gold_mean <- mean(returns$gold_ret)
gold_sd   <- sd(returns$gold_ret)

p4 <- ggplot(returns, aes(x = gold_ret)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 80, fill = "#D4A017",
                 color = "white", alpha = 0.7) +
  stat_function(
    fun  = dnorm,
    args = list(mean = gold_mean, sd = gold_sd),
    color = "red", linewidth = 1, linetype = "dashed"
  ) +
  labs(
    title    = "COMEX Gold Return Distribution vs Normal",
    subtitle = "Red dashed line = fitted normal distribution",
    x        = "Log Return (%)",
    y        = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40")
  )

ggsave("outputs/plots/04_gold_histogram.png",
       plot = p4, width = 8, height = 5, dpi = 150)
cat("Plot 4 saved.\n")

# =============================================================
# PART 7: Save returns data for use in later scripts
# =============================================================

write_csv(returns, "data_clean/returns.csv")
cat("Returns data saved to data_clean/returns.csv\n")

cat("\n--- Script 2 complete ---\n")
cat("Files saved:\n")
cat("  outputs/tables/descriptive_statistics.csv\n")
cat("  outputs/plots/01_kse100_returns.png\n")
cat("  outputs/plots/02_gold_returns.png\n")
cat("  outputs/plots/03_kse100_histogram.png\n")
cat("  outputs/plots/04_gold_histogram.png\n")
cat("  data_clean/returns.csv\n")
