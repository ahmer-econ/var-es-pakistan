# =============================================================
# Project: Tail Risk in Pakistani Financial Markets
# Script 5: GARCH(1,1) Estimation and Filtered Residuals
# Author: Ahmer | GitHub: ahmer-econ | August 2026
# =============================================================

setwd("D:/Documents/APPLIED ECONOMETRICS WORK/VaR_ES_Pakistan Model")

# --- Load libraries ---
library(readr)
library(dplyr)
library(ggplot2)
library(rugarch)   # GARCH estimation

# =============================================================
# PART 1: Load returns data
# =============================================================

returns <- read_csv("data_clean/returns.csv")

head(returns)
nrow(returns)

# =============================================================
# PART 2: Specify and Fit GARCH(1,1) — KSE-100
# =============================================================

# GARCH(1,1) with normal innovation distribution
# We use normal innovations here — fat tails come from
# resampling the standardised residuals empirically in Script 6

garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

# Fit to KSE-100
kse_garch_fit <- ugarchfit(spec = garch_spec, data = returns$kse_ret)

# Print summary
cat("=== GARCH(1,1) — KSE-100 ===\n")
print(kse_garch_fit)
# =============================================================
# PART 3: Specify and Fit GARCH(1,1) — COMEX Gold
# =============================================================

gold_garch_fit <- ugarchfit(spec = garch_spec, data = returns$gold_ret)

cat("\n=== GARCH(1,1) — COMEX Gold ===\n")
print(gold_garch_fit)
# =============================================================
# PART 4: Extract Key Coefficients
# =============================================================

kse_coef  <- coef(kse_garch_fit)
gold_coef <- coef(gold_garch_fit)

cat("\n=== Extracted Coefficients ===\n\n")
cat("KSE-100:\n")
cat("  mu (mean):    ", round(kse_coef["mu"],     6), "\n")
cat("  omega:        ", round(kse_coef["omega"],  6), "\n")
cat("  alpha1 (ARCH):", round(kse_coef["alpha1"], 6), "\n")
cat("  beta1 (GARCH):", round(kse_coef["beta1"],  6), "\n")
cat("  alpha1 + beta1 (persistence):", 
    round(kse_coef["alpha1"] + kse_coef["beta1"], 6), "\n\n")

cat("COMEX Gold:\n")
cat("  mu (mean):    ", round(gold_coef["mu"],     6), "\n")
cat("  omega:        ", round(gold_coef["omega"],  6), "\n")
cat("  alpha1 (ARCH):", round(gold_coef["alpha1"], 6), "\n")
cat("  beta1 (GARCH):", round(gold_coef["beta1"],  6), "\n")
cat("  alpha1 + beta1 (persistence):",
    round(gold_coef["alpha1"] + gold_coef["beta1"], 6), "\n")

# =============================================================
# PART 5: Extract Conditional Volatility and Residuals
# =============================================================

# Conditional volatility (sigma_t) — time-varying std dev
kse_sigma_t  <- sigma(kse_garch_fit)
gold_sigma_t <- sigma(gold_garch_fit)

# Standardised residuals: z_t = (r_t - mu) / sigma_t
kse_z  <- residuals(kse_garch_fit,  standardize = TRUE)
gold_z <- residuals(gold_garch_fit, standardize = TRUE)

# Add to returns dataframe
returns_garch <- returns %>%
  mutate(
    kse_sigma  = as.numeric(kse_sigma_t),
    gold_sigma = as.numeric(gold_sigma_t),
    kse_z      = as.numeric(kse_z),
    gold_z     = as.numeric(gold_z)
  )

# Inspect
head(returns_garch)
cat("\nStd residuals — KSE-100:\n")
cat("  Mean:", round(mean(returns_garch$kse_z), 4),
    "| SD:", round(sd(returns_garch$kse_z), 4), "\n")
cat("Std residuals — COMEX Gold:\n")
cat("  Mean:", round(mean(returns_garch$gold_z), 4),
    "| SD:", round(sd(returns_garch$gold_z), 4), "\n")

# =============================================================
# PART 6: Plot — Conditional Volatility — KSE-100
# =============================================================

p9 <- ggplot(returns_garch, aes(x = date, y = kse_sigma)) +
  geom_line(color = "#2C7BB6", linewidth = 0.5) +
  labs(
    title    = "KSE-100 — GARCH(1,1) Conditional Volatility (2010–2024)",
    subtitle = "Time-varying sigma_t; spikes correspond to market stress episodes",
    x        = "Date",
    y        = "Conditional Std Dev (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40")
  )

ggsave("outputs/plots/09_kse100_garch_volatility.png",
       plot = p9, width = 10, height = 4, dpi = 150)
cat("Plot 9 saved.\n")

# =============================================================
# PART 7: Plot — Conditional Volatility — COMEX Gold
# =============================================================

p10 <- ggplot(returns_garch, aes(x = date, y = gold_sigma)) +
  geom_line(color = "#D4A017", linewidth = 0.5) +
  labs(
    title    = "COMEX Gold — GARCH(1,1) Conditional Volatility (2010–2024)",
    subtitle = "Time-varying sigma_t; COVID-19 spike clearly visible (March 2020)",
    x        = "Date",
    y        = "Conditional Std Dev (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40")
  )

ggsave("outputs/plots/10_gold_garch_volatility.png",
       plot = p10, width = 10, height = 4, dpi = 150)
cat("Plot 10 saved.\n")

# =============================================================
# PART 8: Plot — Standardised Residuals — KSE-100
# =============================================================

p11 <- ggplot(returns_garch, aes(x = date, y = kse_z)) +
  geom_line(color = "#2C7BB6", linewidth = 0.4, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  labs(
    title    = "KSE-100 — GARCH(1,1) Standardised Residuals",
    subtitle = "Should appear closer to i.i.d. after volatility filtering",
    x        = "Date",
    y        = "Standardised Residual (z)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40")
  )

ggsave("outputs/plots/11_kse100_std_residuals.png",
       plot = p11, width = 10, height = 4, dpi = 150)
cat("Plot 11 saved.\n")

# =============================================================
# PART 9: Plot — Standardised Residuals — COMEX Gold
# =============================================================

p12 <- ggplot(returns_garch, aes(x = date, y = gold_z)) +
  geom_line(color = "#D4A017", linewidth = 0.4, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  labs(
    title    = "COMEX Gold — GARCH(1,1) Standardised Residuals",
    subtitle = "Should appear closer to i.i.d. after volatility filtering",
    x        = "Date",
    y        = "Standardised Residual (z)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40")
  )

ggsave("outputs/plots/12_gold_std_residuals.png",
       plot = p12, width = 10, height = 4, dpi = 150)
cat("Plot 12 saved.\n")

# =============================================================
# PART 10: Save GARCH results for Script 6
# =============================================================

write_csv(returns_garch, "data_clean/returns_garch.csv")

# Save coefficients table
garch_coef_table <- data.frame(
  Parameter = c("mu", "omega", "alpha1 (ARCH)",
                "beta1 (GARCH)", "Persistence (alpha+beta)"),
  KSE100 = round(c(
    kse_coef["mu"],
    kse_coef["omega"],
    kse_coef["alpha1"],
    kse_coef["beta1"],
    kse_coef["alpha1"] + kse_coef["beta1"]
  ), 6),
  COMEX_Gold = round(c(
    gold_coef["mu"],
    gold_coef["omega"],
    gold_coef["alpha1"],
    gold_coef["beta1"],
    gold_coef["alpha1"] + gold_coef["beta1"]
  ), 6)
)

print(garch_coef_table)
write_csv(garch_coef_table, "outputs/tables/garch_coefficients.csv")

cat("\n--- Script 5 complete ---\n")
cat("Files saved:\n")
cat("  data_clean/returns_garch.csv\n")
cat("  outputs/tables/garch_coefficients.csv\n")
cat("  outputs/plots/09_kse100_garch_volatility.png\n")
cat("  outputs/plots/10_gold_garch_volatility.png\n")
cat("  outputs/plots/11_kse100_std_residuals.png\n")
cat("  outputs/plots/12_gold_std_residuals.png\n")
