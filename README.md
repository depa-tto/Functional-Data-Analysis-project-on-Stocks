# Functional Data Analysis of Stocks During COVID-19

## Project Summary

This project applies Functional Data Analysis (FDA) techniques to study the behavior of weekly stock returns from eight industrial sectors during the COVID-19 pandemic (2020–2022). We explore depth measures, smoothing techniques, functional PCA, clustering, hypothesis testing, and forecasting with functional autoregressive models.

---

## Dataset

- **Time period**: January 2020 – December 2022  
- **Frequency**: Weekly closing prices (156 weeks)  
- **Industries**:
  - Automobile
  - Fashion & Clothing
  - Food & Beverage
  - Healthcare
  - Tech
  - Logistics
  - Oil & Gas
  - Travel & Tourism

Each sector includes 8 representative companies (e.g., Apple, Toyota, Nike, Tesla, Amazon).

---

## Smoothing Techniques

To model stocks as smooth functions:

- **B-splines (optimal λ ≈ 371)**
- **Local & global kernel smoothers** (e.g., Nadaraya-Watson, normal, triweight)
- Chose optimal parameters via **Generalized Cross-Validation (GCV)**

---

## Exploratory Functional Analysis

- First and second derivatives of smoothed curves to detect market acceleration patterns  
- Outlier detection using functional depth  
- Bivariate covariance and contour plots of functional returns  

---

## Functional Principal Component Analysis (FPCA)

| **Component** | **Contribution** |
|---------------|------------------|
| PCA 1         | 34%              |
| PCA 2         | 24%              |
| PCA 3         | 15%              |
| PCA 4         | 9%               |
| **Total**     | **82%**          |

- **VARIMAX Rotation** revealed distinct temporal effects across the pandemic phases  
- PCA 1 captured overall market variability; PCA 4 emphasized early-pandemic effects  

---

## Clustering (Functional)

Methods:
- **K-Means** (K=5 based on BIC)
- **Hierarchical (HCLUST)**
- **DBSCAN**
- **FunFEM**

### Cluster Insights

| **Cluster** | **Description** |
|-------------|-----------------|
| Cluster 1 | Stable, mature firms (e.g., IBM, MCD, WMT) |
| Cluster 2 | High-growth tech stocks (e.g., AAPL, TSLA, AMZN) |
| Cluster 3 | Streaming & digital-focused firms (e.g., SPOT, META) |
| Cluster 4 | Mixed/hybrid behavior (e.g., GOOG, UPS) |
| Cluster 5 | Travel, automotive, and cyclical sectors (e.g., BMW, TRIP) |

---

## Hypothesis Testing

- Conducted **ANOVA** and **Post-hoc Tukey tests**
- Only **Food** and **Healthcare** sectors showed statistically similar behavior

---

## Forecasting: Functional Autoregressive Models

### FAR(1)
- **Train set**: 146 weeks  
- **Test set**: 10 weeks  
- **MSE (test)**: 1.38  

### FPCA + FAR(3)
- **PCA coverage**: 89%  
- **MSE (test)**: 1.82  

> FAR(1) yielded better predictive performance, though both models showed limited forecasting power.

---

## Conclusions

- No strict clusters by sector — more structure found in volatility and behavior
- Pandemic phases were reflected in PCA and derivatives
- Consistency across clustering methods, especially for Clusters 1 and 5
- Moderate forecasting success using functional models, better for short-term predictions

---

## Team & Institution

- **Danial Yntykbay, Antonio De Patto, Jackie Islam**  
- Vilnius University, Functional Data Analysis Project, May 2025


