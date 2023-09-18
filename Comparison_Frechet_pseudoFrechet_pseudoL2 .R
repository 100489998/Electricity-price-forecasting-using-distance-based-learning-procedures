##################################################################################
# Comparison between discrete Fréchet and pseudo-Fréchet and pseudo-L2 distances
################################################################################## 


# Required libraries
library(longitudinalData)
library(ggplot2)
library(dplyr)
library(foreach)
library(doParallel)


# Read the MercadoSecundarioSubir data
load("MercadoSecundarioSubir.RData")

par(cex.axis = 1.8, cex.lab = 1.8, cex.main = 2)
par(mar = c(4, 4, 4, 1) + 2)  
par(mgp = c(4.5, 1.2, 0))  


#-------------------------------------
# Select years 19-20 for the train set
#-------------------------------------

offer.all_19_20 = subset(offer.all_19_21, substr(date, 1, 4) == c("2019") | substr(date, 1, 4) == c("2020"))
price.all_19_20 = subset(price.all_19_21, substr(date, 1, 4) == c("2019") | substr(date, 1, 4) == c("2020"))

# Number of cumulative offers
NOffers_19_20 = dim(offer.all_19_20)[2]
# Number of hours
NHours_19_20 = dim(offer.all_19_20)[1]


# Add the pair (0,0) in  cumulative offers and prices datasets
X0 = matrix(0, ncol = 1, nrow = NHours_19_20)
offers_19_20 = cbind(X0, offer.all_19_20[, 4:NOffers_19_20])
prices_19_20 = cbind(X0, price.all_19_20[, 4:NOffers_19_20])
NOffers_19_20 = NOffers_19_20-2

# Equal the repeated prices to NA to reduce data. Keep the highest cumulative offer for that repeated price
indexes_prices = which(prices_19_20[, 1:(NOffers_19_20-1)] == prices_19_20[, 2:NOffers_19_20], arr.ind = TRUE)
prices_19_20[indexes_prices] = NA
offers_19_20[indexes_prices] = NA

# For cases with equal cumulative offers and different prices, keep the minimum price
indexes_offers = which(offers_19_20[, 1:(NOffers_19_20-1)] == offers_19_20[, 2:NOffers_19_20], arr.ind = TRUE)
indexes_offers[, 2] = indexes_offers[, 2] + 1 # keep the minimum price. Equivalently, equal to NA the maximum price with the same offer.
prices_19_20[indexes_offers] = NA
offers_19_20[indexes_offers] = NA

# Create a list of prices and a list of cumulative offers where each element of each list is a list with prices/cumulative offers of an hour and a day
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
  prices_19_20_without_NA[[i]] = offers_19_20_without_NA[[i]]/sd_offers_19_20
}




# Select 1000 pairs of curves randomly to calculate the distance between them
set.seed(1234)
n = 10000
first_curve_indexes = sample(seq(1, length(offers_19_20_without_NA), 1), n)
second_curve_indexes = sample(seq(1, length(offers_19_20_without_NA), 1), n)


#----------------------------------
# Discrete Fréchet distance matrix
#----------------------------------

options(max.print = 1)

D_distFrechet_comp = numeric(length = n) # vector where to save the distances between pairs of curves

# Set the number of cores to use
num_cores = detectCores()  

# Register the parallel backend
cl = makeCluster(num_cores)
registerDoParallel(cl)

# Parallel computation using foreach and %dopar%
results = foreach(i = 1:n, .packages = "longitudinalData", .combine='c') %dopar% {
  first_curve = first_curve_indexes[i]
  second_curve = second_curve_indexes[i]
  longitudinalData::distFrechet(offers_19_20_without_NA[[first_curve]], prices_19_20_without_NA[[first_curve]], offers_19_20_without_NA[[second_curve]], prices_19_20_without_NA[[second_curve]])
}

# Stop the parallel backend
stopCluster(cl)

D_distFrechet = unlist(results)



#--------------------------------------------
# Pseudo-L2 distance matrix using 100 points
#--------------------------------------------

N_100 = 100 # number of xticks 
Xmin = 0 # minimum xtick

prices_19_20_100 = array(data = NA, dim = c(NHours_19_20, N_100)) # array where to save the prices corresponding to each xtick
Xvals_100 = array(data = NA, dim = c(NHours_19_20, N_100)) # array where to save the xticks of each curve

for (i in 1:NHours_19_20){
  Xmax = max(offers_19_20_without_NA[[i]]) # the maximum xtick is the maximum cumulative offer of each curve
  Xvals_100[i, ] = seq(Xmin, Xmax, length.out = N_100)
  step_fun = approxfun(offers_19_20_without_NA[[i]], prices_19_20_without_NA[[i]], method = "constant",
                        f = 1, rule = 2, ties = base::max) # define the function of the supply curve
  prices_19_20_100[i, ] = step_fun(Xvals_100[i, ]) # save the prices corresponding to each xtick and curve
}

D_19_20_pL2_100 = numeric(length = n) # vector where to save the distances 

for(i in 1:n){
  first_curve = first_curve_indexes[i]
  second_curve = second_curve_indexes[i]
  D_19_20_pL2_100[i] = sum(sqrt((Xvals_100[first_curve, ]-Xvals_100[second_curve, ])^2+(prices_19_20_100[first_curve, ]-prices_19_20_100[second_curve, ])^2))
}



#-------------------------------------------------
# Pseudo-Frechet distance matrix using 100 points
#-------------------------------------------------

D_19_20_pf_100 = numeric(length = n)


for(i in 1:n){
  first_curve = first_curve_indexes[i]
  second_curve = second_curve_indexes[i]
  D_19_20_pf_100[i] = max(sqrt((Xvals_100[first_curve, ]-Xvals_100[second_curve, ])^2+(prices_19_20_100[first_curve, ]-prices_19_20_100[second_curve, ])^2))
}


#--------------------------------------------
# Pseudo-L2 distance matrix using 200 points
#--------------------------------------------

N_200 = 200 # number of xticks 
Xmin = 0 # minimum xtick

prices_19_20_200 = array(data = NA, dim = c(NHours_19_20, N_200)) # array where to save the prices corresponding to each xtick
Xvals_200 = array(data = NA, dim = c(NHours_19_20, N_200)) # array where to save the xticks of each curve

for (i in 1:NHours_19_20){
  Xmax = max(offers_19_20_without_NA[[i]]) # the maximum xtick is the maximum offer of each curve
  Xvals_200[i, ] = seq(Xmin, Xmax, length.out = N_200)
  step_fun = approxfun(offers_19_20_without_NA[[i]], prices_19_20_without_NA[[i]], method = "constant",
                        f = 1, rule = 2, ties = base::max) # compute an step function of each curve
  prices_19_20_200[i, ] = step_fun(Xvals_200[i, ]) # save the prices corresponding to each xtick and curve
}


D_19_20_pL2_200 = numeric(length = n) # vector where to save the distances 

for(i in 1:n){
  first_curve = first_curve_indexes[i]
  second_curve = second_curve_indexes[i]
  D_19_20_pL2_200[i] = sum(sqrt((Xvals_200[first_curve, ]-Xvals_200[second_curve, ])^2+(prices_19_20_200[first_curve, ]-prices_19_20_200[second_curve, ])^2))
}



#-------------------------------------------------
# Pseudo-Frechet distance matrix using 200 points
#-------------------------------------------------

D_19_20_pf_200 = numeric(length = n)


for(i in 1:n){
  first_curve = first_curve_indexes[i]
  second_curve = second_curve_indexes[i]
  D_19_20_pf_200[i] = max(sqrt((Xvals_200[first_curve, ]-Xvals_200[second_curve, ])^2+(prices_19_20_200[first_curve, ]-prices_19_20_200[second_curve, ])^2))
}


##############
# Comparison
##############

df_comp_pf = data.frame(
  "Discrete Fréchet distance"= D_distFrechet,
  "Pseudo-Fréchet distance with 100 points" = D_19_20_pf_100,
  "Pseudo-Fréchet distance with 200 points" = D_19_20_pf_200
)

pairs(df_comp_pf, labels = c("Discrete Fréchet distance", "Pseudo-Fréchet distance (100 offers)", "Pseudo-Fréchet distance (200 offers)"), cex.labels=2.1, cex.axis = 2.1)

(cor(D_distFrechet, D_19_20_pf_100))
(cor(D_distFrechet, D_19_20_pf_200))
(cor(D_19_20_pf_100, D_19_20_pf_200))


df_comp_pL2 = data.frame(
  "Discrete Fréchet distance"= D_distFrechet,
  "Pseudo-L2 distance with 100 points" = D_19_20_pL2_100, 
  "Pseudo-L2 distance with 200 points" = D_19_20_pL2_200
)

pairs(df_comp_pL2, labels = c("Discrete Fréchet distance", "Pseudo-L2 distance (100 offers)", "Pseudo-L2 distance (200 offers)"), cex.labels=2.1, cex.axis = 2.1)

(cor(D_distFrechet, D_19_20_pL2_100))
(cor(D_distFrechet, D_19_20_pL2_200))
(cor(D_19_20_pL2_100, D_19_20_pL2_200))
