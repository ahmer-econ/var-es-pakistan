# =============================================================
# Project: Tail Risk in Pakistani Financial Markets
# Script 4: Parametric (Variance-Covariance) VaR and ES
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
# PART 2: Parametric VaR and ES — Function
# =============================================================

# Parametric VaR: mu + sigma * qnorm(1 - alpha)
# Parametric ES:  mu - sigma * dnorm(qnorm(alpha)) / alpha
# Both assume normally distributed returns

param_var_es <- function(ret, alpha) {
  mu    <- mean(ret)
  sigma <- sd(ret)
  z     <- qnorm(1 - alpha)          # negative z-score for left tail
  var_p <- mu + sigma * z
  es_p  <- mu - sigma * dnorm(qnorm(alpha)) / (1 - alpha)
  return(list(VaR = var_p, ES = es_p, mu = mu, sigma = sigma))
}

# =============================================================
# PART 3: Compute Parametric VaR and ES at 95% and 99%
# =============================================================

# KSE-100
kse_95p  <- param_var_es(returns$kse_ret, 0.95)
kse_99p  <- param_var_es(returns$kse_ret, 0.99)

# COMEX Gold
gold_95p <- param_var_es(returns$gold_ret, 0.95)
gold_99p <- param_var_es(returns$gold_ret, 0.99)

# Print results
cat("=== Parametric VaR and ES Results ===\n\n")
cat("KSE-100:\n")
cat("  Mean:", round(kse_95p$mu, 4), "% | Std Dev:", round(kse_95p$sigma, 4), "%\n")
cat("  95% VaR:", round(kse_95p$VaR, 4), "%\n")
cat("  95% ES: ", round(kse_95p$ES,  4), "%\n")
cat("  99% VaR:", round(kse_99p$VaR, 4), "%\n")
cat("  99% ES: ", round(kse_99p$ES,  4), "%\n\n")
cat("COMEX Gold:\n")
cat("  Mean:", round(gold_95p$mu, 4), "% | Std Dev:", round(gold_95p$sigma, 4), "%\n")
cat("  95% VaR:", round(gold_95p$VaR, 4), "%\n")
cat("  95% ES: ", round(gold_95p$ES,  4), "%\n")
cat("  99% VaR:", round(gold_99p$VaR, 4), "%\n")
cat("  99% ES: ", round(gold_99p$ES,  4), "%\n")

# =============================================================
# PART 4: Save Parametric Results Table
# =============================================================

param_results <- data.frame(
  Method     = "Parametric",
  Asset      = c("KSE-100", "KSE-100", "COMEX Gold", "COMEX Gold"),
  Confidence = c("95%", "99%", "95%", "99%"),
  VaR        = round(c(kse_95p$VaR,  kse_99p$VaR,
                       gold_95p$VaR, gold_99p$VaR), 4),
  ES         = round(c(kse_95p$ES,   kse_99p$ES,
                       gold_95p$ES,  gold_99p$ES), 4)
)

print(param_results)
write_csv(param_results, "outputs/tables/param_var_es_results.csv")
cat("\nParametric results saved.\n")

# =============================================================
# PART 5: Plot — Parametric vs HS comparison — KSE-100
# =============================================================

# Reload HS results for comparison
hs_results <- read_csv("outputs/tables/hs_var_es_results.csv")

kse_hs_95_var <- hs_results %>%
  filter(Asset == "KSE-100", Confidence == "95%") %>% pull(VaR)
kse_hs_99_var <- hs_results %>%
  filter(Asset == "KSE-100", Confidence == "99%") %>% pull(VaR)

kse_mean  <- kse_95p$mu
kse_sigma <- kse_95p$sigma

p7 <- ggplot(returns, aes(x = kse_ret)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 80, fill = "#2C7BB6",
                 color = "white", alpha = 0.6) +
  # Normal curve overlay
  stat_function(
    fun  = dnorm,
    args = list(mean = kse_mean, sd = kse_sigma),
    color = "black", linewidth = 0.8, linetype = "solid"
  ) +
  # Parametric VaR lines
  geom_vline(xintercept = kse_95p$VaR, color = "#009E73",
             linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = kse_99p$VaR, color = "#009E73",
             linewidth = 1, linetype = "solid") +
  # HS VaR lines
  geom_vline(xintercept = kse_hs_95_var, color = "#D55E00",
             linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = kse_hs_99_var, color = "#D55E00",
             linewidth = 1, linetype = "solid") +
  annotate("text", x = kse_95p$VaR + 0.05,    y = 0.50,
           label = "95% Param VaR", color = "#009E73",
           hjust = 0, size = 3, fontface = "bold") +
  annotate("text", x = kse_99p$VaR + 0.05,    y = 0.45,
           label = "99% Param VaR", color = "#009E73",
           hjust = 0, size = 3, fontface = "bold") +
  annotate("text", x = kse_hs_95_var - 0.05,  y = 0.40,
           label = "95% HS VaR", color = "#D55E00",
           hjust = 1, size = 3, fontface = "bold") +
  annotate("text", x = kse_hs_99_var - 0.05,  y = 0.35,
           label = "99% HS VaR", color = "#D55E00",
           hjust = 1, size = 3, fontface = "bold") +
  labs(
    title    = "KSE-100 — Parametric vs Historical Simulation VaR",
    subtitle = "Green = Parametric (Normal) | Red = Historical Simulation | Dashed = 95% | Solid = 99%",
    x        = "Log Return (%)",
    y        = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40", size = 9)
  )

ggsave("outputs/plots/07_kse100_param_vs_hs.png",
       plot = p7, width = 10, height = 5, dpi = 150)
cat("Plot 7 saved.\n")

# =============================================================
# PART 6: Plot — Parametric vs HS comparison — COMEX Gold
# =============================================================

gold_hs_95_var <- hs_results %>%
  filter(Asset == "COMEX Gold", Confidence == "95%") %>% pull(VaR)
gold_hs_99_var <- hs_results %>%
  filter(Asset == "COMEX Gold", Confidence == "99%") %>% pull(VaR)

gold_mean  <- gold_95p$mu
gold_sigma <- gold_95p$sigma

p8 <- ggplot(returns, aes(x = gold_ret)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 80, fill = "#D4A017",
                 color = "white", alpha = 0.6) +
  stat_function(
    fun  = dnorm,
    args = list(mean = gold_mean, sd = gold_sigma),
    color = "black", linewidth = 0.8, linetype = "solid"
  ) +
  geom_vline(xintercept = gold_95p$VaR, color = "#009E73",
             linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = gold_99p$VaR, color = "#009E73",
             linewidth = 1, linetype = "solid") +
  geom_vline(xintercept = gold_hs_95_var, color = "#D55E00",
             linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = gold_hs_99_var, color = "#D55E00",
             linewidth = 1, linetype = "solid") +
  annotate("text", x = gold_95p$VaR + 0.05,   y = 0.50,
           label = "95% Param VaR", color = "#009E73",
           hjust = 0, size = 3, fontface = "bold") +
  annotate("text", x = gold_99p$VaR + 0.05,   y = 0.45,
           label = "99% Param VaR", color = "#009E73",
           hjust = 0, size = 3, fontface = "bold") +
  annotate("text", x = gold_hs_95_var - 0.05, y = 0.40,
           label = "95% HS VaR", color = "#D55E00",
           hjust = 1, size = 3, fontface = "bold") +
  annotate("text", x = gold_hs_99_var - 0.05, y = 0.35,
           label = "99% HS VaR", color = "#D55E00",
           hjust = 1, size = 3, fontface = "bold") +
  labs(
    title    = "COMEX Gold — Parametric vs Historical Simulation VaR",
    subtitle = "Green = Parametric (Normal) | Red = Historical Simulation | Dashed = 95% | Solid = 99%",
    x        = "Log Return (%)",
    y        = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40", size = 9)
  )

ggsave("outputs/plots/08_gold_param_vs_hs.png",
       plot = p8, width = 10, height = 5, dpi = 150)
cat("Plot 8 saved.\n")

cat("\n--- Script 4 complete ---\n")
cat("Files saved:\n")
cat("  outputs/tables/param_var_es_results.csv\n")
cat("  outputs/plots/07_kse100_param_vs_hs.png\n")
cat("  outputs/plots/08_gold_param_vs_hs.png\n")