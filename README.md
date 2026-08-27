# 🛒 Predicting Customer Lifetime Value with RFM Feature Engineering & Regression

An end-to-end data science project predicting which customers of an online retailer will be most valuable in the next 3 months — using RFM (Recency, Frequency, Monetary) feature engineering and interpretable regression modelling in R.

# Overview

- **Dataset:** [UCI "Online Retail" dataset](https://archive.ics.uci.edu/dataset/352/online+retail) — 541,909 real e-commerce transactions from a UK-based online retailer
- **Cleaning:** Removed 135,080 rows with missing Customer IDs and filtered noise/outliers from bulk cancellations → 406,829 usable transactions
- **Feature engineering:** Built Recency, Frequency, Monetary (RFM) profiles per customer
- **Target construction:** Leakage-safe temporal split (75% past / 25% future) to predict each customer's actual future spend — not a random split
- **Model:** Multiple linear regression on log-transformed, standardized RFM features (chosen over Random Forest/XGBoost for interpretability with a small, well-understood feature set)
- **Refinement:** Two iterations — a baseline model, then a refined version after winsorizing extreme values at the 99th percentile
- **Output:** 3-month forward income prediction for 3,287 customers, exported to `CLV_Predictions.xlsx`

# Headline Result

- **Revenue concentration:** the top 10% of customers by predicted value account for **92.8%** of total projected 3-month revenue
- **Customer segmentation:** quantile-based RFM scoring split the base into 5 cohorts — Champions, Loyal Customers, Potential Loyalists, At-Risk, and Lost — with Champions (22.1% of customers) driving the large majority of projected income
- **Key driver:** Frequency and Monetary are strongly correlated (r = 0.73) and together dominate the model's predictions, with Recency contributing a comparable-magnitude negative effect

# What's in this repo

| File | Description |
|---|---|
| `Data_Wrangling.R` | Cleaning, outlier removal, RFM construction |
| `Model_Training.R` | Full pipeline: temporal split → feature transforms → model training → evaluation → segmentation → export |
| `project.R` | Exploratory data analysis (distributions, outlier bounds, skewness checks) |
| `CLV_Predictions.xlsx` | Final exported 3-month income predictions per customer |
| `CLV_Project_Report.docx` | Full written report — methodology, evaluation, critical analysis, business recommendations |

# Methodology Highlights

- **Leakage-safe target:** used a temporal (not random) past/future split to build the prediction target, avoiding a common mistake in CLV modelling
- **Log1p + z-score transforms** applied to correct severe right-skew in Monetary, with train-set statistics only (no leakage into test set)
- **Iterative refinement:** residual diagnostics on the baseline model directly informed outlier winsorisation in the second model version
- **Deployment sanity check:** validated the model against 5 synthetic customer profiles before scoring the real population

# Limitations (self-documented in the full report)

- Back-transformation from log space can amplify extreme predictions for high-value customers
- A small number of predictions (0.3%) came out negative, which isn't meaningful for a monetary value — flagged for a zero-floor fix
- Model uses only R, F, M — doesn't yet include seasonality, product category, or channel
- Single 80/20 holdout split rather than cross-validation

# Team

Muhammad Hamiz Bilal · Ahmed Haseeb Khan · Hamza Bin Naseer · Abdul Wasay

*B.S. Data Science, GIFT University — July 2026*
