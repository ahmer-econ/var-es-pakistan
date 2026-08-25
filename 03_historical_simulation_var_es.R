# =============================================================
# Project: Tail Risk in Pakistani Financial Markets
# Script 3: Historical Simulation VaR and ES
# Author: Ahmer | GitHub: ahmer-econ | August 2026
# =============================================================

setwd("D:/Documents/APPLIED ECONOMETRICS WORK/VaR_ES_Pakistan Model")

# --- Load libraries ---
library(readr)
library(dplyr)
library(ggplot2)

# =============================================================
# PART 1: Load returns data
# =============================================================

returns <- read_csv("data_clean/returns.csv")

head(returns)
nrow(returns)

# =============================================================
# PART 2: Historical Simulation VaR and ES — Function
# =============================================================

# HS VaR: the empirical quantile at confidence level alpha
# HS ES:  the mean of all returns BELOW the VaR threshold

hs_var_es <- function(ret, alpha) {
  var_hs <- quantile(ret, probs = 1 - alpha, names = FALSE)
  es_hs  <- mean(ret[ret <= var_hs])
  return(list(VaR = var_hs, ES = es_hs))
}

# =============================================================
# PART 3: Compute HS VaR and ES at 95% and 99%
# =============================================================

# KSE-100
kse_95 <- hs_var_es(returns$kse_ret, 0.95)
kse_99 <- hs_var_es(returns$kse_ret, 0.99)

# COMEX Gold
gold_95 <- hs_var_es(returns$gold_ret, 0.95)
gold_99 <- hs_var_es(returns$gold_ret, 0.99)

# Print results
cat("=== Historical Simulation Results ===\n\n")
cat("KSE-100:\n")
cat("  95% VaR:", round(kse_95$VaR, 4), "%\n")
cat("  95% ES: ", round(kse_95$ES,  4), "%\n")
cat("  99% VaR:", round(kse_99$VaR, 4), "%\n")
cat("  99% ES: ", round(kse_99$ES,  4), "%\n\n")
cat("COMEX Gold:\n")
cat("  95% VaR:", round(gold_95$VaR, 4), "%\n")
cat("  95% ES: ", round(gold_95$ES,  4), "%\n")
cat("  99% VaR:", round(gold_99$VaR, 4), "%\n")
cat("  99% ES: ", round(gold_99$ES,  4), "%\n")

# =============================================================
# PART 4: Save HS Results Table
# =============================================================

hs_results <- data.frame(
  Method     = "Historical Simulation",
  Asset      = c("KSE-100", "KSE-100", "COMEX Gold", "COMEX Gold"),
  Confidence = c("95%", "99%", "95%", "99%"),
  VaR        = round(c(kse_95$VaR, kse_99$VaR,
                       gold_95$VaR, gold_99$VaR), 4),
  ES         = round(c(kse_95$ES,  kse_99$ES,
                       gold_95$ES,  gold_99$ES), 4)
)

print(hs_results)
write_csv(hs_results, "outputs/tables/hs_var_es_results.csv")
cat("\nHS results saved.\n")

# =============================================================
# PART 5: Plot — HS VaR/ES on Return Distribution — KSE-100
# =============================================================

p5 <- ggplot(returns, aes(x = kse_ret)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 80, fill = "#2C7BB6",
                 color = "white", alpha = 0.7) +
  geom_vline(xintercept = kse_95$VaR, color = "#E69F00",
             linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = kse_99$VaR, color = "#D55E00",
             linewidth = 1, linetype = "solid") +
  geom_vline(xintercept = kse_95$ES,  color = "#E69F00",
             linewidth = 0.8, linetype = "dotted") +
  geom_vline(xintercept = kse_99$ES,  color = "#D55E00",
             linewidth = 0.8, linetype = "dotted") +
  annotate("text", x = kse_95$VaR - 0.05, y = 0.45,
           label = "95% VaR", color = "#E69F00",
           hjust = 1, size = 3.5, fontface = "bold") +
  annotate("text", x = kse_99$VaR - 0.05, y = 0.40,
           label = "99% VaR", color = "#D55E00",
           hjust = 1, size = 3.5, fontface = "bold") +
  annotate("text", x = kse_95$ES - 0.05, y = 0.35,
           label = "95% ES", color = "#E69F00",
           hjust = 1, size = 3.5) +
  annotate("text", x = kse_99$ES - 0.05, y = 0.30,
           label = "99% ES", color = "#D55E00",
           hjust = 1, size = 3.5) +
  labs(
    title    = "KSE-100 — Historical Simulation VaR and ES",
    subtitle = "Dashed = VaR threshold | Dotted = Expected Shortfall",
    x        = "Log Return (%)",
    y        = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40")
  )

ggsave("outputs/plots/05_kse100_hs_var_es.png",
       plot = p5, width = 9, height = 5, dpi = 150)
cat("Plot 5 saved.\n")

# =============================================================
# PART 6: Plot — HS VaR/ES on Return Distribution — Gold
# =============================================================

p6 <- ggplot(returns, aes(x = gold_ret)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 80, fill = "#D4A017",
                 color = "white", alpha = 0.7) +
  geom_vline(xintercept = gold_95$VaR, color = "#E69F00",
             linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = gold_99$VaR, color = "#D55E00",
             linewidth = 1, linetype = "solid") +
  geom_vline(xintercept = gold_95$ES,  color = "#E69F00",
             linewidth = 0.8, linetype = "dotted") +
  geom_vline(xintercept = gold_99$ES,  color = "#D55E00",
             linewidth = 0.8, linetype = "dotted") +
  annotate("text", x = gold_95$VaR - 0.05, y = 0.45,
           label = "95% VaR", color = "#E69F00",
           hjust = 1, size = 3.5, fontface = "bold") +
  annotate("text", x = gold_99$VaR - 0.05, y = 0.40,
           label = "99% VaR", color = "#D55E00",
           hjust = 1, size = 3.5, fontface = "bold") +
  annotate("text", x = gold_95$ES - 0.05, y = 0.35,
           label = "95% ES", color = "#E69F00",
           hjust = 1, size = 3.5) +
  annotate("text", x = gold_99$ES - 0.05, y = 0.30,
           label = "99% ES", color = "#D55E00",
           hjust = 1, size = 3.5) +
  labs(
    title    = "COMEX Gold — Historical Simulation VaR and ES",
    subtitle = "Dashed = VaR threshold | Dotted = Expected Shortfall",
    x        = "Log Return (%)",
    y        = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40")
  )

ggsave("outputs/plots/06_gold_hs_var_es.png",
       plot = p6, width = 9, height = 5, dpi = 150)
cat("Plot 6 saved.\n")

cat("\n--- Script 3 complete ---\n")
cat("Files saved:\n")
cat("  outputs/tables/hs_var_es_results.csv\n")
cat("  outputs/plots/05_kse100_hs_var_es.png\n")
cat("  outputs/plots/06_gold_hs_var_es.png\n")