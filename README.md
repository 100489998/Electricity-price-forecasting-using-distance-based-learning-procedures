# Electricity price forecasting using distance-based learning methods

Master's Thesis on statistics for data science

Author: Alba Diego Velarde

Tutor: Andrés Modesto Alonso Fernández

This repository contains all the code used in the Master's thesis.  

## Description 

In the master's thesis, the supply curves and prices of year 2021 of one of the Spanish electricity market, the secondary market, are forecasted. Two benchmark methods, based on a k-NN procedure and two methods based on a combination of k-NN and random forest, are used. All these methods require defining a distance between the supply curves. An approximation of the $L^2$ distance between functions and two distances called pseudo-Fréchet and pseudo- $L^2$ are used. A comparison between the three distances is carried out.

## Data

1.  Agregate offers and prices of the secondary market to go up: `MercadoSecundarioSubir.RData`
2.  Aggregate offers and prices of the secondary market to go down: `MercadoSecundarioBajar.RData`
3.  Estimated power requirements of the secondary market to go up: `RequerimientosSecundariaASubir.xlsx`
4.  Estimated power requirements of the secondary market to go down: `RequerimientosSecundariaABajar.xlsx`

## Scripts 

1. `Matrices_secondary_market_go_up.R`: It contains the code related to the computation of the distance matrices using $L^2$, pseudo-Fréchet and pseudo- $L^2$ distances of the secondary market to go up.
2. `Matrices_secondary_market_go_down.R`: It contains the code related to the computation of the the distance matrices using $L^2$, pseudo-Fréchet and pseudo- $L^2$ distances of the secondary market to go down.
3. `Descriptive_analysis.R`: It contains the code related to the descriptive analysis section of the Master's Thesis.
4. `Frechet_distance_tests.R`: It contains all the code related to the tests that have been done to try to use the Fréchet distance in the methods of the work.
5. `Comparison_Frechet_pseudoFrechet_distances.R`: It contains all the code related to the comparison between the pseudo-Fréchet, pseudo- $L^2$ and Fréchet distances.
6. 

## Instructions

1. Download all the scripts in the same folder
2. Compile `Matrices_secondary_market_go_up.R` and `Matrices_secondary_market_go_down.R` files to obtain distance matrices.
3. 
