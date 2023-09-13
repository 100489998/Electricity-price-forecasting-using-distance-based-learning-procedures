# Electricity price forecasting using distance-based learning methods

Master's Thesis on statistics for data science

Author: Alba Diego Velarde

Supervisor: Andrés Modesto Alonso Fernández

This repository contains all the code used in the Master's thesis.  

## Description 

In the master's thesis, the supply curves and prices of year 2021 of one of the Spanish electricity markets, the secondary market, are forecasted. Two benchmark methods, based on a k-NN procedure and two methods based on a combination of k-NN and random forest, are used. All these methods require defining a distance between the supply curves. An approximation of the $L^2$ distance between functions and two distances called pseudo-Fréchet and pseudo- $L^2$ are used. A comparison between the four methods and between the three distances is carried out.

## Data

1.  Agregate offers and prices of the secondary market to go up: `MercadoSecundarioSubir.RData`
2.  Aggregate offers and prices of the secondary market to go down: `MercadoSecundarioBajar.RData`
3.  Estimated power requirements of the secondary market to go up: `RequerimientosSecundariaASubir.xlsx`
4.  Estimated power requirements of the secondary market to go down: `RequerimientosSecundariaABajar.xlsx`

## Scripts 

1. `Matrices_upward_secondary_market.R`: It contains the code related to the computation of the distance matrices using $L^2$, pseudo-Fréchet and pseudo- $L^2$ distances of the secondary market to go up.
2. `Matrices_downward_secondary_market.R`: It contains the code related to the computation of the the distance matrices using $L^2$, pseudo-Fréchet and pseudo- $L^2$ distances of the secondary market to go down.
3. `Descriptive_analysis.R`: It contains the code related to the descriptive analysis section of the Master's Thesis.
4. `Frechet_distance_tests.R`: It contains all the code related to the tests that have been done to try to use the Fréchet distance in the methods of the work.
5. `Comparison_Frechet_pseudoFrechet_pseudoL2.R`: It contains all the code related to the comparison between the pseudo-Fréchet, pseudo- $L^2$ and Fréchet distances.
6. `R2_comparison.R`: It contains a comparison of the $R^2$ values of the random forest models of the methods.
7. `Price_prediction_errors_comparison`: It contains a comparison of the price prediction errors using the different distances

Scripts containing the training and testing of the methods and the price prediction with the different distances:
1. `Methods_L2_distance_upward.R`: Upward secondary market and $L^2$ distance.
2. `Methods_L2_distance_downward.R`: Downward secondary market and $L^2$ distance.
3. `Methods_pseudo_Frechet_distance_upward.R`: Upward secondary market and pseudo-Fréchet distance.
4. `Methods_pseudo_Frechet_distance_downward.R`: Downward secondary market and pseudo-Fréchet distance.
5. `Methods_pseudo_L2_distance_upward.R`: Upward secondary market and pseudo- $L^2$ distance.
6. `Methods_pseudo_L2_distance_downward.R`: Downward secondary market and pseudo- $L^2$ distance.



## Instructions

1. Download all the scripts and data in the same folder.
2. Compile `Matrices_upward_secondary_market.R` and `Matrices_downward_secondary_market.R` scripts to obtain distance matrices which are input to the methods.
3. Compile scripts containing the training and testing of the methods and the price prediction with the different distances.
4. Compile `R2_comparison.R` and `Price_prediction_errors_comparison.R`.

The rest of the scripts do not require to have compiled any other file before.
