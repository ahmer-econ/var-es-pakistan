# Tail Risk in Pakistani Financial Markets
## Value at Risk and Expected Shortfall for KSE-100 Equities and COMEX Gold

**Author:** Ahmer | **GitHub:** ahmer-econ | **August 2026**

---

## Overview

This project estimates and compares Value at Risk (VaR) and Expected Shortfall (ES) for two assets — the **KSE-100 Index** (Pakistan Stock Exchange) and **COMEX Gold** — using three methodologically distinct approaches of increasing sophistication. Formal backtesting validates each model's accuracy over a 14.5-year daily sample.

---

## Assets and Sample

| Item | Detail |
|---|---|
| **KSE-100 Index** | Daily closing prices, Kaggle (aleemaher, CC0) |
| **COMEX Gold (GC=F)** | Daily closing prices, Yahoo Finance via quantmod |
| **Sample period** | January 2010 – August 2024 |
| **Observations** | 3,487 daily log returns (after alignment) |

---

## Methods

### 1. Historical Simulation (HS)
Non-parametric. VaR and ES computed directly from the empirical return distribution. No distributional assumption required.

### 2. Parametric (Variance-Covariance)
Assumes normally distributed returns. VaR and ES derived analytically from estimated mean and standard deviation. Serves as a benchmark to demonstrate the cost of the normality assumption.

### 3. GARCH(1,1) Filtered Historical Simulation (FHS)
Fits a GARCH(1,1) model to capture time-varying volatility. Standardised residuals are resampled empirically — combining GARCH's forward-looking conditional volatility with the fat-tailed innovation distribution.

---

## Key Results

### Distributional Properties

| Statistic | KSE-100 | COMEX Gold |
|---|---|---|
| Mean (%) | 0.061 | 0.022 |
| Std Dev (%) | 1.045 | 1.042 |
| Skewness | −0.476 | −0.265 |
| Excess Kurtosis | 3.985 | 8.865 |
| Jarque-Bera p-value | < 0.0001 | < 0.0001 |

Both series decisively reject normality. COMEX Gold's excess kurtosis of 8.87 reflects extreme safe-haven demand shocks and the April 2013 flash crash.

---

### VaR and ES Estimates

| Asset | Conf. | HS VaR | Param VaR | FHS VaR |
|---|---|---|---|---|
| KSE-100 | 95% | −1.61% | −1.66% | −1.59% |
| KSE-100 | 99% | −3.04% | −2.37% | −2.80% |
| COMEX Gold | 95% | −1.65% | −1.69% | −1.80% |
| COMEX Gold | 99% | −2.80% | −2.40% | −3.03% |

At 99%, the Parametric method underestimates KSE-100 tail risk by 67 basis points relative to Historical Simulation. The ES gap reaches 129 basis points.

---

### GARCH(1,1) Coefficients

| Parameter | KSE-100 | COMEX Gold |
|---|---|---|
| α₁ (ARCH) | 0.1252 | 0.0440 |
| β₁ (GARCH) | 0.8108 | 0.9391 |
| Persistence (α+β) | 0.9361 | 0.9831 |

Volatility persistence is extremely high for both assets — shocks decay very slowly.

---

### Backtesting — Kupiec POF

| Method | Asset | 95% | 99% |
|---|---|---|---|
| Historical Simulation | KSE-100 | ✅ PASS | ✅ PASS |
| Parametric | KSE-100 | ✅ PASS | ❌ FAIL |
| GARCH-FHS | KSE-100 | ✅ PASS | ❌ FAIL |
| Historical Simulation | COMEX Gold | ✅ PASS | ✅ PASS |
| Parametric | COMEX Gold | ✅ PASS | ❌ FAIL |
| GARCH-FHS | COMEX Gold | ❌ FAIL | ✅ PASS |

Historical Simulation is the only method that passes all Kupiec tests across both assets.

---

### Backtesting — Christoffersen Conditional Coverage

The Christoffersen test reveals **systematic breach clustering in KSE-100** across all methods — even where Kupiec passes. Only HS and Parametric on COMEX Gold at 95% pass the full conditional coverage test. This indicates that no static, single-regime VaR model fully resolves volatility regime persistence in Pakistani equities.


---

## R Packages Used

| Package | Purpose |
|---|---|
| `quantmod` | COMEX Gold data download |
| `rugarch` | GARCH(1,1) estimation |
| `ggplot2` | All visualisations |
| `moments` | Skewness, kurtosis |
| `tseries` | Jarque-Bera test |
| `dplyr` | Data manipulation |
| `readr` | CSV import/export |
| `lubridate` | Date handling |

---

## Data Sources

- **KSE-100:** [Kaggle — Pakistan Stock Exchange KSE100 20 Years](https://www.kaggle.com/datasets/aleemaher/psx-kse100-20-years) (CC0 Public Domain)
- **COMEX Gold:** Yahoo Finance via `quantmod::getSymbols("GC=F")`

---

## Key References

- Kupiec, P. (1995). Techniques for verifying the accuracy of risk measurement models. *Journal of Derivatives*, 3(2), 73–84.
- Christoffersen, P. (1998). Evaluating interval forecasts. *International Economic Review*, 39(4), 841–862.
- McNeil, A. J., Frey, R., and Embrechts, P. (2015). *Quantitative Risk Management*. Princeton University Press.
- Bollerslev, T. (1986). Generalized autoregressive conditional heteroskedasticity. *Journal of Econometrics*, 31(3), 307–327.

---


---

## Repository Structure
