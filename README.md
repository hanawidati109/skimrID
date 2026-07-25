# skimrID

## Overview

**skimrID** is a derivative package developed from the original **skimr** package. The package extends the functionality of skimr by providing additional tools for exploratory data analysis, including Indonesian descriptive summaries, extended statistical summaries, and automatic data preprocessing.

This package was developed as part of the Computational Statistics course project.

---

## Attribution

This package is a derivative work based on the original **skimr** package developed by the **ropensci** community.

Original package:

https://github.com/ropensci/skimr

The original authors retain copyright of the inherited source code.
Only the additional features and modifications were developed in this package.

---

## Features

### 1. skim_indo()

Provides descriptive statistical summaries in Indonesian.

Features:

- Dataset summary
- Missing values
- Completeness rate
- Number of outliers
- Inline histograms
- Indonesian variable labels

Example:

```r
skim_indo(iris)
```

---

### 2. skim_extended()

Provides additional descriptive statistics not available in the original skimr package.

Additional statistics include:

- Mean
- Standard Deviation
- Coefficient of Variation (CV)
- Skewness
- Kurtosis
- Range
- Interquartile Range (IQR)
- Shapiro-Wilk Normality Test
- Normality Status

Example

```r
skim_extended(iris)
```

---

### 3. skim_preprocess()

Automatically preprocesses datasets before analysis.

Available preprocessing:

- Remove duplicated rows
- Missing value imputation
- Winsorization
- Log transformation
- Detailed preprocessing report
- Preprocessing summary

Example

```r
hasil <- skim_preprocess(airquality)

hasil$data
hasil$detail
hasil$summary
```

---

## Installation

```r
devtools::install_github("hanawidati109/skimrID")
```

---

## Required Packages

- skimr
- moments

---

## Authors

- Siti Hana Widati (24611058)
- Salma Nurbayani (24611053)
- Aninditya Cantika Putri Ramadhani (24611035)

Department of Statistics  
Universitas Islam Indonesia
