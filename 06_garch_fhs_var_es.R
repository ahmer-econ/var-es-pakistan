# =============================================================
# Project: Tail Risk in Pakistani Financial Markets
# Script 6: GARCH-Based Filtered Historical Simulation VaR & ES
# Author: Ahmer | GitHub: ahmer-econ | August 2026
# =============================================================

setwd("D:/Documents/APPLIED ECONOMETRICS WORK/VaR_ES_Pakistan Model")

# --- Load libraries ---
library(readr)
library(dplyr)
library(ggplot2)

# =============================================================
# PART 1: Load GARCH results
# =============================================================

returns_garch <- read_csv("data_clean/returns_garch.csv")

head(returns_garch)
nrow(returns_garch)

# =============================================================
# PART 2: Filtered Historical Simulation — Concept
# =============================================================

# FHS VaR procedure:
# 1. Use the last observation's conditional sigma (sigma_T)
#    as our forward-looking volatility estimate
# 2. Resample empirically from the full set of standardised
#    residuals z_t to get the innovation distribution
# 3. Construct simulated returns: r_sim = mu + sigma_T * z_t
# 4. Compute VaR and ES from the simulated return distribution

# This combines GARCH's time-varying volatility with the
# empirical fat-tailed innovation distribution —
# no normality assumption required

# =============================================================
# PART 3: FHS VaR and ES Function
# =============================================================

fhs_var_es <- function(mu, sigma_last, z_vec, alpha) {
  # Simulate returns using last sigma and empirical residuals
  r_sim  <- mu + sigma_last * z_vec
  # VaR: empirical quantile of simulated returns
  var_fhs <- quantile(r_sim, probs = 1 - alpha, names = FALSE)
  # ES: mean of simulated returns below VaR
  es_fhs  <- mean(r_sim[r_sim <= var_fhs])
  return(list(VaR = var_fhs, ES = es_fhs))
}

# =============================================================
# PART 4: Compute FHS VaR and ES — KSE-100
# =============================================================

# Parameters for KSE-100
kse_mu         <- mean(returns_garch$kse_ret)
kse_sigma_last <- tail(returns_garch$kse_sigma, 1)
kse_z_vec      <- returns_garch$kse_z

cat("KSE-100 last conditional sigma:", round(kse_sigma_last, 4), "%\n")

kse_fhs_95 <- fhs_var_es(kse_mu, kse_sigma_last, kse_z_vec, 0.95)
kse_fhs_99 <- fhs_var_es(kse_mu, kse_sigma_last, kse_z_vec, 0.99)

# =============================================================
# PART 5: Compute FHS VaR and ES — COMEX Gold
# =============================================================

# Parameters for COMEX Gold
gold_mu         <- mean(returns_garch$gold_ret)
gold_sigma_last <- tail(returns_garch$gold_sigma, 1)
gold_z_vec      <- returns_garch$gold_z

cat("COMEX Gold last conditional sigma:", round(gold_sigma_last, 4), "%\n")

gold_fhs_95 <- fhs_var_es(gold_mu, gold_sigma_last, gold_z_vec, 0.95)
gold_fhs_99 <- fhs_var_es(gold_mu, gold_sigma_last, gold_z_vec, 0.99)

# =============================================================
# PART 6: Print FHS Results
# =============================================================

cat("\n=== GARCH Filtered Historical Simulation Results ===\n\n")
cat("KSE-100:\n")
cat("  95% VaR:", round(kse_fhs_95$VaR, 4), "%\n")
cat("  95% ES: ", round(kse_fhs_95$ES,  4), "%\n")
cat("  99% VaR:", round(kse_fhs_99$VaR, 4), "%\n")
cat("  99% ES: ", round(kse_fhs_99$ES,  4), "%\n\n")
cat("COMEX Gold:\n")
cat("  95% VaR:", round(gold_fhs_95$VaR, 4), "%\n")
cat("  95% ES: ", round(gold_fhs_95$ES,  4), "%\n")
cat("  99% VaR:", round(gold_fhs_99$VaR, 4), "%\n")
cat("  99% ES: ", round(gold_fhs_99$ES,  4), "%\n")

# =============================================================
# PART 7: Save FHS Results Table
# =============================================================

fhs_results <- data.frame(
  Method     = "GARCH-FHS",
  Asset      = c("KSE-100", "KSE-100", "COMEX Gold", "COMEX Gold"),
  Confidence = c("95%", "99%", "95%", "99%"),
  VaR        = round(c(kse_fhs_95$VaR,  kse_fhs_99$VaR,
                       gold_fhs_95$VaR, gold_fhs_99$VaR), 4),
  ES         = round(c(kse_fhs_95$ES,   kse_fhs_99$ES,
                       gold_fhs_95$ES,  gold_fhs_99$ES), 4)
)

print(fhs_results)
write_csv(fhs_results, "outputs/tables/fhs_var_es_results.csv")
cat("\nFHS results saved.\n")

# =============================================================
# PART 8: Build Master Comparison Table — All Three Methods
# =============================================================

hs_results    <- read_csv("outputs/tables/hs_var_es_results.csv")
param_results <- read_csv("outputs/tables/param_var_es_results.csv")

master_table <- bind_rows(hs_results, param_results, fhs_results) %>%
  arrange(Asset, Confidence, Method)

print(master_table)
write_csv(master_table, "outputs/tables/master_var_es_comparison.csv")
cat("Master comparison table saved.\n")

# =============================================================
# PART 9: Plot — Three-Method VaR Comparison — KSE-100
# =============================================================

kse_plot_data <- master_table %>%
  filter(Asset == "KSE-100") %>%
  mutate(
    Method     = factor(Method,
                        levels = c("Historical Simulation",
                                   "Parametric",
                                   "GARCH-FHS")),
    Confidence = factor(Confidence, levels = c("95%", "99%"))
  )

p13 <- ggplot(kse_plot_data,
              aes(x = Confidence, y = abs(VaR),
                  fill = Method)) +
  geom_bar(stat = "identity", position = "dodge",
           width = 0.6, color = "white") +
  geom_text(aes(label = paste0(round(abs(VaR), 2), "%")),
            position = position_dodge(width = 0.6),
            vjust = -0.4, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c(
    "Historical Simulation" = "#D55E00",
    "Parametric"            = "#009E73",
    "GARCH-FHS"             = "#2C7BB6"
  )) +
  labs(
    title    = "KSE-100 — VaR Comparison Across Three Methods",
    subtitle = "Absolute values shown | Higher bar = more conservative risk estimate",
    x        = "Confidence Level",
    y        = "VaR (absolute, %)",
    fill     = "Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "bottom"
  )

ggsave("outputs/plots/13_kse100_var_comparison.png",
       plot = p13, width = 9, height = 5, dpi = 150)
cat("Plot 13 saved.\n")

# =============================================================
# PART 10: Plot — Three-Method VaR Comparison — COMEX Gold
# =============================================================

gold_plot_data <- master_table %>%
  filter(Asset == "COMEX Gold") %>%
  mutate(
    Method     = factor(Method,
                        levels = c("Historical Simulation",
                                   "Parametric",
                                   "GARCH-FHS")),
    Confidence = factor(Confidence, levels = c("95%", "99%"))
  )

p14 <- ggplot(gold_plot_data,
              aes(x = Confidence, y = abs(VaR),
                  fill = Method)) +
  geom_bar(stat = "identity", position = "dodge",
           width = 0.6, color = "white") +
  geom_text(aes(label = paste0(round(abs(VaR), 2), "%")),
            position = position_dodge(width = 0.6),
            vjust = -0.4, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c(
    "Historical Simulation" = "#D55E00",
    "Parametric"            = "#009E73",
    "GARCH-FHS"             = "#D4A017"
  )) +
  labs(
    title    = "COMEX Gold — VaR Comparison Across Three Methods",
    subtitle = "Absolute values shown | Higher bar = more conservative risk estimate",
    x        = "Confidence Level",
    y        = "VaR (absolute, %)",
    fill     = "Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "bottom"
  )

ggsave("outputs/plots/14_gold_var_comparison.png",
       plot = p14, width = 9, height = 5, dpi = 150)
cat("Plot 14 saved.\n")

cat("\n--- Script 6 complete ---\n")
cat("Files saved:\n")
cat("  outputs/tables/fhs_var_es_results.csv\n")
cat("  outputs/tables/master_var_es_comparison.csv\n")
cat("  outputs/plots/13_kse100_var_comparison.png\n")
cat("  outputs/plots/14_gold_var_comparison.png\n")