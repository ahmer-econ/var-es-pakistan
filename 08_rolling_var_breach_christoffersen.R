# =============================================================
# Project: Tail Risk in Pakistani Financial Markets
# Script 8: Rolling VaR, Breach Plots, Christoffersen Test
# Author: Ahmer | GitHub: ahmer-econ | August 2026
# =============================================================

setwd("D:/Documents/APPLIED ECONOMETRICS WORK/VaR_ES_Pakistan Model")

# --- Load libraries ---
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# =============================================================
# PART 1: Load data
# =============================================================

returns_garch <- read_csv("data_clean/returns_garch.csv")
master_table  <- read_csv("outputs/tables/master_var_es_comparison.csv")

head(returns_garch)
nrow(returns_garch)

# =============================================================
# PART 2: Rolling Historical Simulation VaR (250-day window)
# =============================================================

# 250 trading days ~ 1 year, standard rolling window in practice

roll_window <- 250

n <- nrow(returns_garch)

kse_roll_var95  <- rep(NA, n)
kse_roll_var99  <- rep(NA, n)
gold_roll_var95 <- rep(NA, n)
gold_roll_var99 <- rep(NA, n)

for (i in (roll_window + 1):n) {
  window_kse  <- returns_garch$kse_ret[(i - roll_window):(i - 1)]
  window_gold <- returns_garch$gold_ret[(i - roll_window):(i - 1)]
  
  kse_roll_var95[i]  <- quantile(window_kse,  0.05)
  kse_roll_var99[i]  <- quantile(window_kse,  0.01)
  gold_roll_var95[i] <- quantile(window_gold, 0.05)
  gold_roll_var99[i] <- quantile(window_gold, 0.01)
}

returns_garch <- returns_garch %>%
  mutate(
    kse_roll_var95  = kse_roll_var95,
    kse_roll_var99  = kse_roll_var99,
    gold_roll_var95 = gold_roll_var95,
    gold_roll_var99 = gold_roll_var99
  )

cat("Rolling VaR computed.\n")
cat("First non-NA row:", roll_window + 1, "\n")

# =============================================================
# PART 3: Plot — Rolling VaR — KSE-100
# =============================================================

kse_roll_plot <- returns_garch %>%
  filter(!is.na(kse_roll_var95))

p17 <- ggplot(kse_roll_plot, aes(x = date)) +
  geom_line(aes(y = kse_ret), color = "grey70",
            linewidth = 0.3, alpha = 0.8) +
  geom_line(aes(y = kse_roll_var95), color = "#E69F00",
            linewidth = 0.7, linetype = "dashed") +
  geom_line(aes(y = kse_roll_var99), color = "#D55E00",
            linewidth = 0.7, linetype = "solid") +
  geom_hline(yintercept = 0, color = "black",
             linewidth = 0.3) +
  labs(
    title    = "KSE-100 — Rolling 250-Day Historical Simulation VaR",
    subtitle = "Grey = daily returns | Orange dashed = 95% VaR | Red solid = 99% VaR",
    x        = "Date",
    y        = "Log Return / VaR (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40", size = 9)
  )

ggsave("outputs/plots/17_kse100_rolling_var.png",
       plot = p17, width = 11, height = 5, dpi = 150)
cat("Plot 17 saved.\n")

# =============================================================
# PART 4: Plot — Rolling VaR — COMEX Gold
# =============================================================

gold_roll_plot <- returns_garch %>%
  filter(!is.na(gold_roll_var95))

p18 <- ggplot(gold_roll_plot, aes(x = date)) +
  geom_line(aes(y = gold_ret), color = "grey70",
            linewidth = 0.3, alpha = 0.8) +
  geom_line(aes(y = gold_roll_var95), color = "#E69F00",
            linewidth = 0.7, linetype = "dashed") +
  geom_line(aes(y = gold_roll_var99), color = "#D55E00",
            linewidth = 0.7, linetype = "solid") +
  geom_hline(yintercept = 0, color = "black",
             linewidth = 0.3) +
  labs(
    title    = "COMEX Gold — Rolling 250-Day Historical Simulation VaR",
    subtitle = "Grey = daily returns | Orange dashed = 95% VaR | Red solid = 99% VaR",
    x        = "Date",
    y        = "Log Return / VaR (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40", size = 9)
  )

ggsave("outputs/plots/18_gold_rolling_var.png",
       plot = p18, width = 11, height = 5, dpi = 150)
cat("Plot 18 saved.\n")

# =============================================================
# PART 5: VaR Breach Plot — KSE-100 (HS 99%)
# =============================================================

kse_hs_99_var <- master_table %>%
  filter(Method == "Historical Simulation",
         Asset == "KSE-100", Confidence == "99%") %>%
  pull(VaR)

returns_garch <- returns_garch %>%
  mutate(
    kse_breach_99  = kse_ret  < kse_hs_99_var,
    gold_breach_99 = gold_ret < (master_table %>%
                                   filter(Method == "Historical Simulation",
                                          Asset == "COMEX Gold", Confidence == "99%") %>%
                                   pull(VaR))
  )

p19 <- ggplot(returns_garch, aes(x = date, y = kse_ret)) +
  geom_line(color = "grey60", linewidth = 0.3, alpha = 0.8) +
  geom_hline(yintercept = kse_hs_99_var, color = "#D55E00",
             linewidth = 0.8, linetype = "dashed") +
  geom_point(data = filter(returns_garch, kse_breach_99),
             aes(x = date, y = kse_ret),
             color = "#D55E00", size = 1.2, alpha = 0.8) +
  annotate("text",
           x = as.Date("2010-06-01"),
           y = kse_hs_99_var - 0.3,
           label = paste0("99% HS VaR = ",
                          round(kse_hs_99_var, 2), "%"),
           color = "#D55E00", size = 3.2, hjust = 0) +
  labs(
    title    = "KSE-100 — VaR Breach Plot (Historical Simulation, 99%)",
    subtitle = paste0("Red dots = breach days | Red dashed = VaR threshold | ",
                      "Total breaches: ",
                      sum(returns_garch$kse_breach_99, na.rm = TRUE)),
    x        = "Date",
    y        = "Log Return (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40", size = 9)
  )

ggsave("outputs/plots/19_kse100_breach_plot.png",
       plot = p19, width = 11, height = 5, dpi = 150)
cat("Plot 19 saved.\n")

# =============================================================
# PART 6: VaR Breach Plot — COMEX Gold (HS 99%)
# =============================================================

gold_hs_99_var <- master_table %>%
  filter(Method == "Historical Simulation",
         Asset == "COMEX Gold", Confidence == "99%") %>%
  pull(VaR)

p20 <- ggplot(returns_garch, aes(x = date, y = gold_ret)) +
  geom_line(color = "grey60", linewidth = 0.3, alpha = 0.8) +
  geom_hline(yintercept = gold_hs_99_var, color = "#D55E00",
             linewidth = 0.8, linetype = "dashed") +
  geom_point(data = filter(returns_garch, gold_breach_99),
             aes(x = date, y = gold_ret),
             color = "#D55E00", size = 1.2, alpha = 0.8) +
  annotate("text",
           x = as.Date("2010-06-01"),
           y = gold_hs_99_var - 0.3,
           label = paste0("99% HS VaR = ",
                          round(gold_hs_99_var, 2), "%"),
           color = "#D55E00", size = 3.2, hjust = 0) +
  labs(
    title    = "COMEX Gold — VaR Breach Plot (Historical Simulation, 99%)",
    subtitle = paste0("Red dots = breach days | Red dashed = VaR threshold | ",
                      "Total breaches: ",
                      sum(returns_garch$gold_breach_99, na.rm = TRUE)),
    x        = "Date",
    y        = "Log Return (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40", size = 9)
  )

ggsave("outputs/plots/20_gold_breach_plot.png",
       plot = p20, width = 11, height = 5, dpi = 150)
cat("Plot 20 saved.\n")

# =============================================================
# PART 7: Christoffersen Conditional Coverage Test
# =============================================================

# Extends Kupiec by testing BOTH unconditional coverage
# AND independence of breaches (no clustering)
# LR_cc = LR_uc + LR_ind ~ chi-sq(2) under H0

christoffersen_cc <- function(actual_ret, var_estimate, alpha) {
  
  n      <- length(actual_ret)
  hits   <- as.integer(actual_ret < var_estimate)  # 1 = breach
  
  # Transition counts
  n00 <- sum(hits[-n] == 0 & hits[-1] == 0)  # no breach -> no breach
  n01 <- sum(hits[-n] == 0 & hits[-1] == 1)  # no breach -> breach
  n10 <- sum(hits[-n] == 1 & hits[-1] == 0)  # breach -> no breach
  n11 <- sum(hits[-n] == 1 & hits[-1] == 1)  # breach -> breach
  
  # Transition probabilities
  p01  <- n01 / max(n00 + n01, 1)
  p11  <- n11 / max(n10 + n11, 1)
  p    <- 1 - alpha
  p_hat <- (n01 + n11) / n
  
  # LR unconditional coverage (Kupiec)
  if (p_hat == 0 | p_hat == 1) {
    lr_uc <- NA
  } else {
    breaches <- n01 + n11
    lr_uc <- -2 * (
      log((1 - p)^(n - breaches) * p^breaches) -
        log((1 - p_hat)^(n - breaches) * p_hat^breaches)
    )
  }
  
  # LR independence
  if (p01 == 0 | p01 == 1 | p11 == 0 | p11 == 1) {
    lr_ind <- NA
  } else {
    lr_ind <- -2 * (
      log((1 - p_hat)^(n00 + n10) * p_hat^(n01 + n11)) -
        log((1 - p01)^n00 * p01^n01 *
              (1 - p11)^n10 * p11^n11)
    )
  }
  
  # LR conditional coverage
  lr_cc <- lr_uc + lr_ind
  
  p_uc  <- round(1 - pchisq(lr_uc,  df = 1), 4)
  p_ind <- round(1 - pchisq(lr_ind, df = 1), 4)
  p_cc  <- round(1 - pchisq(lr_cc,  df = 2), 4)
  
  return(data.frame(
    n11      = n11,
    p11      = round(p11, 4),
    LR_UC    = round(lr_uc,  4),
    P_UC     = p_uc,
    LR_IND   = round(lr_ind, 4),
    P_IND    = p_ind,
    LR_CC    = round(lr_cc,  4),
    P_CC     = p_cc,
    Result   = ifelse(p_cc >= 0.05, "PASS", "FAIL")
  ))
}

# =============================================================
# PART 8: Run Christoffersen — All Models
# =============================================================

get_var <- function(method, asset, conf) {
  master_table %>%
    filter(Method == method, Asset == asset,
           Confidence == conf) %>%
    pull(VaR)
}

cc_results <- bind_rows(
  data.frame(Method = "Historical Simulation", Asset = "KSE-100",
             Confidence = "95%") %>%
    bind_cols(christoffersen_cc(returns_garch$kse_ret,
                                get_var("Historical Simulation","KSE-100","95%"), 0.95)),
  data.frame(Method = "Historical Simulation", Asset = "KSE-100",
             Confidence = "99%") %>%
    bind_cols(christoffersen_cc(returns_garch$kse_ret,
                                get_var("Historical Simulation","KSE-100","99%"), 0.99)),
  data.frame(Method = "Parametric", Asset = "KSE-100",
             Confidence = "95%") %>%
    bind_cols(christoffersen_cc(returns_garch$kse_ret,
                                get_var("Parametric","KSE-100","95%"), 0.95)),
  data.frame(Method = "Parametric", Asset = "KSE-100",
             Confidence = "99%") %>%
    bind_cols(christoffersen_cc(returns_garch$kse_ret,
                                get_var("Parametric","KSE-100","99%"), 0.99)),
  data.frame(Method = "GARCH-FHS", Asset = "KSE-100",
             Confidence = "95%") %>%
    bind_cols(christoffersen_cc(returns_garch$kse_ret,
                                get_var("GARCH-FHS","KSE-100","95%"), 0.95)),
  data.frame(Method = "GARCH-FHS", Asset = "KSE-100",
             Confidence = "99%") %>%
    bind_cols(christoffersen_cc(returns_garch$kse_ret,
                                get_var("GARCH-FHS","KSE-100","99%"), 0.99)),
  data.frame(Method = "Historical Simulation", Asset = "COMEX Gold",
             Confidence = "95%") %>%
    bind_cols(christoffersen_cc(returns_garch$gold_ret,
                                get_var("Historical Simulation","COMEX Gold","95%"), 0.95)),
  data.frame(Method = "Historical Simulation", Asset = "COMEX Gold",
             Confidence = "99%") %>%
    bind_cols(christoffersen_cc(returns_garch$gold_ret,
                                get_var("Historical Simulation","COMEX Gold","99%"), 0.99)),
  data.frame(Method = "Parametric", Asset = "COMEX Gold",
             Confidence = "95%") %>%
    bind_cols(christoffersen_cc(returns_garch$gold_ret,
                                get_var("Parametric","COMEX Gold","95%"), 0.95)),
  data.frame(Method = "Parametric", Asset = "COMEX Gold",
             Confidence = "99%") %>%
    bind_cols(christoffersen_cc(returns_garch$gold_ret,
                                get_var("Parametric","COMEX Gold","99%"), 0.99)),
  data.frame(Method = "GARCH-FHS", Asset = "COMEX Gold",
             Confidence = "95%") %>%
    bind_cols(christoffersen_cc(returns_garch$gold_ret,
                                get_var("GARCH-FHS","COMEX Gold","95%"), 0.95)),
  data.frame(Method = "GARCH-FHS", Asset = "COMEX Gold",
             Confidence = "99%") %>%
    bind_cols(christoffersen_cc(returns_garch$gold_ret,
                                get_var("GARCH-FHS","COMEX Gold","99%"), 0.99))
)

print(cc_results)
write_csv(cc_results, "outputs/tables/christoffersen_results.csv")
cat("\nChristoffersen results saved.\n")

cat("\n--- Script 8 complete ---\n")
cat("Files saved:\n")
cat("  outputs/plots/17_kse100_rolling_var.png\n")
cat("  outputs/plots/18_gold_rolling_var.png\n")
cat("  outputs/plots/19_kse100_breach_plot.png\n")
cat("  outputs/plots/20_gold_breach_plot.png\n")
cat("  outputs/tables/christoffersen_results.csv\n")