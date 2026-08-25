# =============================================================
# Project: Tail Risk in Pakistani Financial Markets
# Script 7: Kupiec POF Backtesting
# Author: Ahmer | GitHub: ahmer-econ | August 2026
# =============================================================

setwd("D:/Documents/APPLIED ECONOMETRICS WORK/VaR_ES_Pakistan Model")

# --- Load libraries ---
library(readr)
library(dplyr)
library(ggplot2)

# =============================================================
# PART 1: Load data
# =============================================================

returns_garch <- read_csv("data_clean/returns_garch.csv")
master_table  <- read_csv("outputs/tables/master_var_es_comparison.csv")

head(returns_garch)
nrow(returns_garch)

# =============================================================
# PART 2: Kupiec POF Test — Function
# =============================================================

# POF test:
# H0: The failure rate equals the nominal rate (1 - alpha)
# Test statistic: LR = -2 * log(L0/L1) ~ chi-sq(1) under H0
# Reject H0 (bad model) if p-value < 0.05

kupiec_pof <- function(actual_ret, var_estimate, alpha) {
  n         <- length(actual_ret)
  # Number of breaches: actual loss exceeds VaR
  breaches  <- sum(actual_ret < var_estimate)
  # Observed failure rate
  p_hat     <- breaches / n
  # Theoretical failure rate
  p         <- 1 - alpha
  # Log-likelihood ratio statistic
  # Guard against log(0)
  if (p_hat == 0 | p_hat == 1) {
    lr_stat <- NA
    p_value <- NA
  } else {
    lr_stat <- -2 * (
      log((1 - p)^(n - breaches) * p^breaches) -
        log((1 - p_hat)^(n - breaches) * p_hat^breaches)
    )
    p_value <- 1 - pchisq(lr_stat, df = 1)
  }
  return(list(
    n            = n,
    breaches     = breaches,
    expected     = round(p * n, 1),
    failure_rate = round(p_hat * 100, 4),
    nominal_rate = round(p * 100, 4),
    lr_stat      = round(lr_stat, 4),
    p_value      = round(p_value, 4),
    result       = ifelse(is.na(p_value), "NA",
                          ifelse(p_value >= 0.05, "PASS", "FAIL"))
  ))
}

# =============================================================
# PART 3: Retrieve VaR estimates for backtesting
# =============================================================

# Pull VaR values from master table
get_var <- function(method, asset, conf) {
  master_table %>%
    filter(Method == method, Asset == asset,
           Confidence == conf) %>%
    pull(VaR)
}

# KSE-100 VaR estimates
kse_hs_95_var    <- get_var("Historical Simulation", "KSE-100", "95%")
kse_hs_99_var    <- get_var("Historical Simulation", "KSE-100", "99%")
kse_param_95_var <- get_var("Parametric",            "KSE-100", "95%")
kse_param_99_var <- get_var("Parametric",            "KSE-100", "99%")
kse_fhs_95_var   <- get_var("GARCH-FHS",             "KSE-100", "95%")
kse_fhs_99_var   <- get_var("GARCH-FHS",             "KSE-100", "99%")

# COMEX Gold VaR estimates
gold_hs_95_var    <- get_var("Historical Simulation", "COMEX Gold", "95%")
gold_hs_99_var    <- get_var("Historical Simulation", "COMEX Gold", "99%")
gold_param_95_var <- get_var("Parametric",            "COMEX Gold", "95%")
gold_param_99_var <- get_var("Parametric",            "COMEX Gold", "99%")
gold_fhs_95_var   <- get_var("GARCH-FHS",             "COMEX Gold", "95%")
gold_fhs_99_var   <- get_var("GARCH-FHS",             "COMEX Gold", "99%")

# =============================================================
# PART 4: Run Kupiec POF — KSE-100
# =============================================================

cat("=== Kupiec POF Backtesting — KSE-100 ===\n\n")

kse_tests <- list(
  HS_95    = kupiec_pof(returns_garch$kse_ret,  kse_hs_95_var,    0.95),
  HS_99    = kupiec_pof(returns_garch$kse_ret,  kse_hs_99_var,    0.99),
  Param_95 = kupiec_pof(returns_garch$kse_ret,  kse_param_95_var, 0.95),
  Param_99 = kupiec_pof(returns_garch$kse_ret,  kse_param_99_var, 0.99),
  FHS_95   = kupiec_pof(returns_garch$kse_ret,  kse_fhs_95_var,   0.95),
  FHS_99   = kupiec_pof(returns_garch$kse_ret,  kse_fhs_99_var,   0.99)
)

for (nm in names(kse_tests)) {
  t <- kse_tests[[nm]]
  cat(nm, "| Breaches:", t$breaches, "| Expected:", t$expected,
      "| Failure rate:", t$failure_rate, "%",
      "| LR:", t$lr_stat,
      "| p-value:", t$p_value,
      "| Result:", t$result, "\n")
}

# =============================================================
# PART 5: Run Kupiec POF — COMEX Gold
# =============================================================

cat("\n=== Kupiec POF Backtesting — COMEX Gold ===\n\n")

gold_tests <- list(
  HS_95    = kupiec_pof(returns_garch$gold_ret, gold_hs_95_var,    0.95),
  HS_99    = kupiec_pof(returns_garch$gold_ret, gold_hs_99_var,    0.99),
  Param_95 = kupiec_pof(returns_garch$gold_ret, gold_param_95_var, 0.95),
  Param_99 = kupiec_pof(returns_garch$gold_ret, gold_param_99_var, 0.99),
  FHS_95   = kupiec_pof(returns_garch$gold_ret, gold_fhs_95_var,   0.95),
  FHS_99   = kupiec_pof(returns_garch$gold_ret, gold_fhs_99_var,   0.99)
)

for (nm in names(gold_tests)) {
  t <- gold_tests[[nm]]
  cat(nm, "| Breaches:", t$breaches, "| Expected:", t$expected,
      "| Failure rate:", t$failure_rate, "%",
      "| LR:", t$lr_stat,
      "| p-value:", t$p_value,
      "| Result:", t$result, "\n")
}

# =============================================================
# PART 6: Build Backtesting Results Table
# =============================================================

build_bt_row <- function(method, asset, conf, test_obj) {
  data.frame(
    Method       = method,
    Asset        = asset,
    Confidence   = conf,
    Obs          = test_obj$n,
    Breaches     = test_obj$breaches,
    Expected     = test_obj$expected,
    Failure_Rate = test_obj$failure_rate,
    Nominal_Rate = test_obj$nominal_rate,
    LR_Stat      = test_obj$lr_stat,
    P_Value      = test_obj$p_value,
    Result       = test_obj$result
  )
}

backtest_table <- bind_rows(
  build_bt_row("Historical Simulation", "KSE-100",    "95%", kse_tests$HS_95),
  build_bt_row("Historical Simulation", "KSE-100",    "99%", kse_tests$HS_99),
  build_bt_row("Parametric",            "KSE-100",    "95%", kse_tests$Param_95),
  build_bt_row("Parametric",            "KSE-100",    "99%", kse_tests$Param_99),
  build_bt_row("GARCH-FHS",             "KSE-100",    "95%", kse_tests$FHS_95),
  build_bt_row("GARCH-FHS",             "KSE-100",    "99%", kse_tests$FHS_99),
  build_bt_row("Historical Simulation", "COMEX Gold", "95%", gold_tests$HS_95),
  build_bt_row("Historical Simulation", "COMEX Gold", "99%", gold_tests$HS_99),
  build_bt_row("Parametric",            "COMEX Gold", "95%", gold_tests$Param_95),
  build_bt_row("Parametric",            "COMEX Gold", "99%", gold_tests$Param_99),
  build_bt_row("GARCH-FHS",             "COMEX Gold", "95%", gold_tests$FHS_95),
  build_bt_row("GARCH-FHS",             "COMEX Gold", "99%", gold_tests$FHS_99)
)

print(backtest_table)
write_csv(backtest_table, "outputs/tables/kupiec_backtest_results.csv")
cat("\nBacktesting table saved.\n")

# =============================================================
# PART 7: Plot — Breach Count vs Expected — KSE-100
# =============================================================

kse_bt <- backtest_table %>%
  filter(Asset == "KSE-100") %>%
  mutate(
    Label  = paste(Method, Confidence),
    Method = factor(Method, levels = c("Historical Simulation",
                                       "Parametric", "GARCH-FHS")),
    Result = factor(Result, levels = c("PASS", "FAIL"))
  )

p15 <- ggplot(kse_bt, aes(x = reorder(Label, Breaches))) +
  geom_bar(aes(y = Breaches, fill = Result),
           stat = "identity", width = 0.6) +
  geom_point(aes(y = Expected), color = "black",
             size = 3, shape = 18) +
  geom_hline(aes(yintercept = mean(Expected)),
             linetype = "dashed", color = "grey50") +
  scale_fill_manual(values = c("PASS" = "#009E73",
                               "FAIL" = "#D55E00")) +
  coord_flip() +
  labs(
    title    = "KSE-100 — Kupiec POF Backtesting",
    subtitle = "Bars = actual breaches | Diamond = expected breaches | Green = PASS | Red = FAIL",
    x        = NULL,
    y        = "Number of VaR Breaches",
    fill     = "Test Result"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40", size = 9),
    legend.position = "bottom"
  )

ggsave("outputs/plots/15_kse100_kupiec_backtest.png",
       plot = p15, width = 9, height = 5, dpi = 150)
cat("Plot 15 saved.\n")

# =============================================================
# PART 8: Plot — Breach Count vs Expected — COMEX Gold
# =============================================================

gold_bt <- backtest_table %>%
  filter(Asset == "COMEX Gold") %>%
  mutate(
    Label  = paste(Method, Confidence),
    Method = factor(Method, levels = c("Historical Simulation",
                                       "Parametric", "GARCH-FHS")),
    Result = factor(Result, levels = c("PASS", "FAIL"))
  )

p16 <- ggplot(gold_bt, aes(x = reorder(Label, Breaches))) +
  geom_bar(aes(y = Breaches, fill = Result),
           stat = "identity", width = 0.6) +
  geom_point(aes(y = Expected), color = "black",
             size = 3, shape = 18) +
  geom_hline(aes(yintercept = mean(Expected)),
             linetype = "dashed", color = "grey50") +
  scale_fill_manual(values = c("PASS" = "#009E73",
                               "FAIL" = "#D55E00")) +
  coord_flip() +
  labs(
    title    = "COMEX Gold — Kupiec POF Backtesting",
    subtitle = "Bars = actual breaches | Diamond = expected breaches | Green = PASS | Red = FAIL",
    x        = NULL,
    y        = "Number of VaR Breaches",
    fill     = "Test Result"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40", size = 9),
    legend.position = "bottom"
  )

ggsave("outputs/plots/16_gold_kupiec_backtest.png",
       plot = p16, width = 9, height = 5, dpi = 150)
cat("Plot 16 saved.\n")

cat("\n--- Script 7 complete ---\n")
cat("Files saved:\n")
cat("  outputs/tables/kupiec_backtest_results.csv\n")
cat("  outputs/plots/15_kse100_kupiec_backtest.png\n")
cat("  outputs/plots/16_gold_kupiec_backtest.png\n")