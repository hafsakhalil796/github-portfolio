# Happiness Analysis Project

## Overview
This project examines the determinants of global happiness using country-level data.  
It focuses on how economic, social, and institutional factors influence happiness rankings and whether these effects differ across happiness levels and development status.

## Research Question
How do GDP per capita, social support, freedom, and health influence global happiness rankings?  
Do these effects differ across levels of happiness and between developed and developing countries?

## Data
- Source: Global happiness dataset (`homework2.xlsx`)
- Observations: ~140 countries
- Key variables:
  - Happiness score and ranking
  - GDP per capita
  - Social support
  - Freedom to make life choices
  - Health (life expectancy)
  - Corruption perception
  - Generosity
  - Development status (developed vs developing)

## Methodology
The analysis applies standard econometric techniques used in applied research:

- Log-linear OLS regression
- Group-wise comparison (low vs high happiness countries)
- F-test for equality of variances
- Generalized Least Squares (GLS)
- Robust (White) standard errors
- Interaction models
- Multicollinearity diagnostics
- Endogeneity testing and IV regression
- Serial correlation testing (Durbin–Watson) and AR(1) correction

## Key Models
Main OLS model:
log(Ranking) ~ GDPPC + Social + Freedom + log(Health)
Interaction model:
Happy ~ Corruption + Health + developed + developed*Corruption
IV model:
Happy ~ Health + Corruption + developed
Instrument: Generosity
## Tools & Technologies
- R (version 4.x)
- Packages:
  - openxlsx
  - sandwich
  - lmtest
  - nlme
  - AER
  - corrplot
  - psych
  - stats

## Results
- GDP per capita is a significant predictor of happiness ranking.
- Social support and freedom show strong associations with happiness.
- Structural differences exist between low- and high-happiness countries.
- Development status moderates the effect of corruption and health.
- Endogeneity and serial correlation tests provide additional robustness.

## Key Models
Main OLS model:
