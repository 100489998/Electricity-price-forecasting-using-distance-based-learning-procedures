##########################
# Fréchet distance tests
##########################

# Required libraries
library(TSdist) # for Frechet distance
library(longitudinalData) # for Frechet distance
library(ggplot2)
library(dplyr)

# Read the MercadoSecundarioSubir data
load("MercadoSecundarioSubir.RData")

par(cex.axis = 1.8, cex.lab = 1.6, cex.main = 2)
par(mar = c(4, 4, 4, 1) + 2) 
par(mgp = c(4.5, 1.2, 0))  



###############
# Years 19-20
###############


offer.all_19_20 = subset(offer.all_19_21, substr(date, 1, 4) == c("2019")  | substr(date, 1, 4) == c("2020"))
price.all_19_20 = subset(price.all_19_21, substr(date, 1, 4) == c("2019")  | substr(date, 1, 4) == c("2020"))

# Number of cumulative offers
NOffers_19_20 = dim(offer.all_19_20)[2]
# Number of hours
NHours_19_20 = dim(offer.all_19_20)[1]


# Add the pair (0,0) in  cumulative offers and prices datasets
X0 = matrix(0, ncol = 1, nrow = NHours_19_20)
offers_19_20 = cbind(X0, offer.all_19_20[, 4:NOffers_19_20])
prices_19_20 = cbind(X0, price.all_19_20[, 4:NOffers_19_20])
NOffers_19_20 = NOffers_19_20-2

# Equal the repeated prices to NA to reduce data. Keep the highest offer for that repeated price
indexes_prices = which(prices_19_20[, 1:(NOffers_19_20-1)] == prices_19_20[, 2:NOffers_19_20], arr.ind = TRUE)
prices_19_20[indexes_prices] = NA
offers_19_20[indexes_prices] = NA

# For cases with equal cumulative offers and different prices, keep the minimum price
indexes_offers = which(offers_19_20[, 1:(NOffers_19_20-1)] == offers_19_20[, 2:NOffers_19_20], arr.ind = TRUE)
indexes_offers[, 2] = indexes_offers[, 2] + 1 # keep the minimum price. Equivalently, equal to NA the maximum price with the same offer.
prices_19_20[indexes_offers] = NA
offers_19_20[indexes_offers] = NA

# Create a list of prices and a list of cumulative offers where each element of each list is a list with prices/offers of an hour and a day
prices1 = prices_19_20[1, ] %>% as.numeric %>% na.omit
offers1 = offers_19_20[1, ] %>% as.numeric %>% na.omit
prices_19_20_without_NA = list(prices1)
offers_19_20_without_NA = list(offers1)

for (i in 2:NHours_19_20){
  price_19_20_without_NA = prices_19_20[i, ] %>% as.numeric %>% na.omit
  offer_19_20_without_NA = offers_19_20[i, ] %>% as.numeric %>% na.omit
  prices_19_20_without_NA = append(prices_19_20_without_NA, list(price_19_20_without_NA))
  offers_19_20_without_NA = append(offers_19_20_without_NA, list(offer_19_20_without_NA))
}

sd_prices_19_20 = sd(as.matrix(prices_19_20), na.rm = T)
sd_offers_19_20 = sd(as.matrix(offers_19_20), na.rm = T)

for(i in 1:NHours_19_20){
  prices_19_20_without_NA[[i]] = prices_19_20_without_NA[[i]]/sd_prices_19_20
  offers_19_20_without_NA[[i]] = offers_19_20_without_NA[[i]]/sd_offers_19_20
}



#-----------------------------------------------
# Calculate Fréchet distance computation times
#-----------------------------------------------

# Select 1000 pairs of curves randomly
set.seed(1234)
n = 1000
first_curve_indexes = sample(seq(1, length(offers_19_20_without_NA), 1), n)
second_curve_indexes = sample(seq(1, length(offers_19_20_without_NA), 1), n)

times_distFrechet_all_points = numeric(n) # vector where to save the time required to compute the distance with distFrechet function
times_FrechetDistance_all_points = numeric(n) # vector where to save the time required to compute the distance with FrechetDistance function
options(max.print=1) # to avoid prints that increase computational cost

# Computation times
for(i in 1:n){
  first_curve = first_curve_indexes[i]
  second_curve = second_curve_indexes[i]
  
  times_distFrechet_all_points[i] = mean(microbenchmark::microbenchmark(
    distFrechet(offers_19_20_without_NA[[first_curve]], prices_19_20_without_NA[[first_curve]], offers_19_20_without_NA[[second_curve]], prices_19_20_without_NA[[second_curve]]),
    times=10)$time)
  times_distFrechet_all_points[i] = times_distFrechet_all_points[i]/1e6 # convert to milliseconds
  times_FrechetDistance_all_points[i] = mean(microbenchmark::microbenchmark(
    FrechetDistance(prices_19_20_without_NA[[first_curve]], prices_19_20_without_NA[[second_curve]], offers_19_20_without_NA[[first_curve]], offers_19_20_without_NA[[second_curve]]),
    times=10)$time)
  times_FrechetDistance_all_points[i] = times_FrechetDistance_all_points[i]/1e6 # convert to milliseconds
}


# Comparison of the times

boxplot(times_distFrechet_all_points, times_FrechetDistance_all_points, names = c("distFrechet", "FrechetDistance"), ylab = "Time required to compute the distance between curves / ms")
points(1, mean(times_distFrechet_all_points), col = "red", pch = 16)
points(2, mean(times_FrechetDistance_all_points), col = "red", pch = 16)

# Add the means
text(1, mean(times_distFrechet_all_points), round(mean(times_distFrechet_all_points), 2), pos = 3, col = "red", cex = 1.6)
text(2, mean(times_FrechetDistance_all_points), round(mean(times_FrechetDistance_all_points), 2), pos = 3, col = "red", cex = 1.6)



#-------------------------------------------------------------------------------------------------------------------------
# Select a random sample of the prices/offers of each hour and day to reduce computational cost of the Frechet distance
#-------------------------------------------------------------------------------------------------------------------------

prices_19_20_without_NA_samp = list()
offers_19_20_without_NA_samp = list()

# Obtain randomly the points of curves to take into account in the computation of the distances
for (i in 1:NHours_19_20){
  length_i = length(prices_19_20_without_NA[[i]])
  ind_samp_i = sort(sample(seq(2,length_i-1), length_i/3)) # the sample of each hour and day is of length equal to 1/3 of the prices/offers of these hour and day +2
  prices_19_20_without_NA_samp[[i]] = c(prices_19_20_without_NA[[i]][1], prices_19_20_without_NA[[i]][ind_samp_i], prices_19_20_without_NA[[i]][length_i]) # add smallest and largest prices
  offers_19_20_without_NA_samp[[i]] = c(offers_19_20_without_NA[[i]][1], offers_19_20_without_NA[[i]][ind_samp_i], offers_19_20_without_NA[[i]][length_i])
}


# Select 1000 pairs of curves randomly
set.seed(1234)
n = 1000
first_curve_indexes = sample(seq(1, length(offers_19_20_without_NA), 1), n)
second_curve_indexes = sample(seq(1, length(offers_19_20_without_NA), 1), n)

options(max.print=1) # to avoid prints that increase computational cost
frech_dist_random = numeric(n) # vector where to save the approximate Frechet distances
frech_dist_exact = numeric(n) # vector where to save the exact distances
diff_per_distFrechet_random = numeric(n) # vector where to save the difference between the approximate and exact distances


# Compute the difference between the exact Frechet distance and the approximate Frechet distance (in percentage)
for(i in 1:n){
  first_curve = first_curve_indexes[i]
  second_curve = second_curve_indexes[i]
  
  frech_dist_random[i]=distFrechet(offers_19_20_without_NA_samp[[first_curve]], prices_19_20_without_NA_samp[[first_curve]], offers_19_20_without_NA_samp[[second_curve]], prices_19_20_without_NA_samp[[second_curve]])
  frech_dist_exact[i] = distFrechet(offers_19_20_without_NA[[first_curve]], prices_19_20_without_NA[[first_curve]], offers_19_20_without_NA[[second_curve]], prices_19_20_without_NA[[second_curve]])
  diff_per_distFrechet_random[i]=abs(frech_dist_random[i]-frech_dist_exact[i])/frech_dist_exact[i]*100
}

# Histogram of the differences

hist(diff_per_distFrechet_random, breaks = round(sqrt(n)), xlab = "Percentage of difference between exact and approximate Fréchet distances", xlim = c(0,max(diff_per_distFrechet_random)), main ="")


#----------------------------------------------------------------------------------------------------------------------------
# Select a sample taking the offers corresponding to the highest prices to reduce computational cost of the Frechet distance
#----------------------------------------------------------------------------------------------------------------------------

# Compute the differences between consecutive prices in all the curves
prices_19_20_without_NA_jumps = lapply(prices_19_20_without_NA, diff)

prices_19_20_without_NA_samp_jumps = list()
offers_19_20_without_NA_samp_jumps = list()


per = seq(0, 1, 0.05) # percentages of cumulative offers with the highest prices
max_diff_per = numeric(length(per)) # vector where to save the maximum difference between exact and approximate Frechet distance

# Select 1000 pairs of curves randomly
set.seed(1234)
n = 1000
first_curve_indexes = sample(seq(1, length(offers_19_20_without_NA), 1), n)
second_curve_indexes = sample(seq(1, length(offers_19_20_without_NA), 1), n)

frech_dist_distFrechet_jumps = matrix(0, nrow = n, ncol = length(per)) # matrix where to save the approximate Frechet distances
frech_dist_exact_jumps = matrix(0, nrow = n, ncol = length(per)) # matrix where to save the exact Fréchet distances
diff_per_distFrechet_jumps = matrix(0, nrow = n, ncol = length(per)) # matrix where to save the difference between the approximate and exact distances

for (j in 1:length(per)){
  for (i in 1:NHours_19_20){
    length_i = length(prices_19_20_without_NA[[i]])
    # Indexes corresponding to offers with highest prices
    ind_samp_big_jumps_i = sort(order(prices_19_20_without_NA_jumps[[i]], decreasing = TRUE)[1:round(per[j]*length_i/6)]) 
    ind_samp_big_jumps_i = sort(c(ind_samp_big_jumps_i, ind_samp_big_jumps_i+1))
    
    # Indexes corresponding to the remaining offers
    ind_samp_not_big_jumps_i = seq(1,length_i)[-c(1, ind_samp_big_jumps_i, length_i)]
    # (1-per[j])*100% of the offers  will be equidistant points selected among the remaining offers
    ind_samp_eq_i = round(seq(from = ind_samp_not_big_jumps_i[1], to = ind_samp_not_big_jumps_i[length(ind_samp_not_big_jumps_i)], length.out = round((1-per[j])*length_i/3)))
    # Join the two type of offers
    ind_samp_i = sort(unique(c(ind_samp_big_jumps_i, ind_samp_eq_i)))
    # Obtain the selected offers and prices 
    prices_19_20_without_NA_samp_jumps[[i]] = c(prices_19_20_without_NA[[i]][1], prices_19_20_without_NA[[i]][ind_samp_i], prices_19_20_without_NA[[i]][length_i])
    offers_19_20_without_NA_samp_jumps[[i]] = c(offers_19_20_without_NA[[i]][1], offers_19_20_without_NA[[i]][ind_samp_i], offers_19_20_without_NA[[i]][length_i])
  }
  options(max.print=1) # to avoid prints that increase computational cost
  
  for(i in 1:n){
    first_curve = first_curve_indexes[i]
    second_curve = second_curve_indexes[i]
    
    frech_dist_distFrechet_jumps[i,j] = distFrechet(offers_19_20_without_NA_samp_jumps[[first_curve]], prices_19_20_without_NA_samp_jumps[[first_curve]], offers_19_20_without_NA_samp_jumps[[second_curve]], prices_19_20_without_NA_samp_jumps[[second_curve]])
    frech_dist_exact_jumps[i,j] = distFrechet(offers_19_20_without_NA[[first_curve]], prices_19_20_without_NA[[first_curve]], offers_19_20_without_NA[[second_curve]], prices_19_20_without_NA[[second_curve]])
    diff_per_distFrechet_jumps[i,j]=abs((frech_dist_distFrechet_jumps[i,j])-frech_dist_exact_jumps[i,j])/frech_dist_exact_jumps[i,j]*100
  }
}


boxplot(diff_per_distFrechet_jumps, xlab = "Selected percentage of offers with highest prices", ylab = "Percentage of difference between exact and approximate distances", xaxt = "n", cex.lab = 1.6)
axis(1, at = 1:length(per), labels = per)



