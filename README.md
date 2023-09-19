# Electricity price forecasting using distance-based learning methods

Master's Thesis on Statistics for Data Science

Author: Alba Diego Velarde

Supervisor: Andrés Modesto Alonso Fernández

This repository contains all the code used in the Master's Thesis.  

## Description 

In the Master's Thesis, the supply curves and prices of year 2021 of one of the Spanish electricity markets, the secondary market, are forecasted. Two benchmark methods, based on a k-NN procedure and two methods based on a combination of k-NN and random forest are used. All these methods require defining a distance between the supply curves. An approximation of the $L^2$ distance between functions and two distances called pseudo-Fréchet and pseudo- $L^2$ are used. A comparison between the four methods and between the three distances is carried out.

## Data

1.  Cumulative offers and prices of the upward secondary market: `MercadoSecundarioSubir.RData`
2.  Cumulative offers and prices of the downward secondary market: `MercadoSecundarioBajar.RData`
3.  Estimated power requirements of the upward secondary market: `RequerimientosSecundariaASubir.xlsx`
4.  Estimated power requirements of the downward secondary market: `RequerimientosSecundariaABajar.xlsx`

## Scripts 

1. `Matrices_upward_secondary_market.R`: It contains the code related to the computation of the distance matrices using $L^2$, pseudo-Fréchet and pseudo- $L^2$ distances in the upward secondary market.
2. `Matrices_downward_secondary_market.R`: It contains the code related to the computation of the distance matrices using $L^2$, pseudo-Fréchet and pseudo- $L^2$ distances in the downward secondary market.
3. `Descriptive_analysis.R`: It contains the code related to the descriptive analysis section of the Master's Thesis.
4. `Frechet_distance_tests.R`: It contains all the code related to the tests that have been done to try to use the discrete Fréchet distance in the methods of the work.
5. `Comparison_Frechet_pseudoFrechet_pseudoL2.R`: It contains all the code related to the comparison between the pseudo-Fréchet, pseudo- $L^2$ and discrete Fréchet distances.
6. `R2_comparison.R`: It contains a comparison of the $R^2$ values of the random forest models of the methods using the different distances.
7. `Price_prediction_errors_comparison`: It contains a comparison of the price prediction errors using the different distances.
8. `Correlations.R`: It contains the Spearman correlation coefficients between the independent and the dependent variables of the random forest models using the different distances.

Scripts containing the training and testing of the methods and the price prediction with the different distances:
1. `Methods_L2_distance_upward.R`: Upward secondary market and $L^2$ distance.
2. `Methods_L2_distance_downward.R`: Downward secondary market and $L^2$ distance.
3. `Methods_pseudo_Frechet_distance_upward.R`: Upward secondary market and pseudo-Fréchet distance.
4. `Methods_pseudo_Frechet_distance_downward.R`: Downward secondary market and pseudo-Fréchet distance.
5. `Methods_pseudo_L2_distance_upward.R`: Upward secondary market and pseudo- $L^2$ distance.
6. `Methods_pseudo_L2_distance_downward.R`: Downward secondary market and pseudo- $L^2$ distance.



## Instructions

1. Download all the scripts and data in the same folder.
2. Compile `Matrices_upward_secondary_market.R` and `Matrices_downward_secondary_market.R` scripts to obtain the distance matrices which are input to the methods.
3. Compile scripts containing the training and testing of the methods and the price prediction with the different distances.
4. Compile `R2_comparison.R`, `Price_prediction_errors_comparison.R` and `Correlations.R` scripts.

The rest of the scripts do not require to have compiled any other file before.
