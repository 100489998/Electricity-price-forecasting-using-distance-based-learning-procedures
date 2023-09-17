
###################################################
# Preprocessing and obtaining distance matrices of 
# the upward secondary market
###################################################

# Required libraries
library(readxl)
library(dplyr)
library(distances)

# Read the MercadoSecundarioSubir data
load("MercadoSecundarioSubir.RData")

# Number of cumulative offers
NOffers_19_21 = dim(offer.all_19_21)[2]
# Number of hours
NHours_19_21 = dim(offer.all_19_21)[1]

# Add the pair (0,0) in cumulative offers and prices datasets
X0 = matrix(0, ncol = 1, nrow = NHours_19_21)
offers_19_21 = cbind(X0, offer.all_19_21[ , 4:NOffers_19_21])
prices_19_21 = cbind(X0, price.all_19_21[ , 4:NOffers_19_21])
NOffers_19_21 = NOffers_19_21-2 
rm(X0)

# Remove to save memory
rm(offer.all_19_21) 
rm(price.all_19_21) 

# Equal the repeated prices to NA to reduce data. Keep the highest cumulative offer for that repeated price
indexes_prices = which(prices_19_21[, 1:(NOffers_19_21-1)] == prices_19_21[, 2:NOffers_19_21], arr.ind = TRUE)
prices_19_21[indexes_prices] = NA
offers_19_21[indexes_prices] = NA
rm(indexes_prices)

# For cases with equal cumulative offers and different prices, keep the minimum price
indexes_offers = which(offers_19_21[, 1:(NOffers_19_21-1)] == offers_19_21[, 2:NOffers_19_21], arr.ind = TRUE)
indexes_offers[, 2] = indexes_offers[, 2] + 1 # keep the minimum price. Equivalently, equal to NA the maximum price with the same offer.
prices_19_21[indexes_offers] = NA
offers_19_21[indexes_offers] = NA
rm(indexes_offers)

# Create a list of prices and a list of offers where each element contains prices/cumulative offers of an hour and a day
prices1 = prices_19_21[1,] %>% as.numeric %>% na.omit
offers1 = offers_19_21[1,] %>% as.numeric %>% na.omit
prices_19_21_without_NA = list(prices1)
offers_19_21_without_NA = list(offers1)

for (i in 2:NHours_19_21){
  price_19_21_without_NA = prices_19_21[i,] %>% as.numeric %>% na.omit
  offer_19_21_without_NA = offers_19_21[i,] %>% as.numeric %>% na.omit
  prices_19_21_without_NA = append(prices_19_21_without_NA, list(price_19_21_without_NA))
  offers_19_21_without_NA = append(offers_19_21_without_NA, list(offer_19_21_without_NA))
}

save(offers_19_21_without_NA, file = "offers_19_21_without_NA_goup.RData")
save(prices_19_21_without_NA, file = "prices_19_21_without_NA_goup.RData")


# Compute the standard deviation of the previous lists
sd_prices_19_21 = sd(as.matrix(prices_19_21), na.rm = T)
sd_offers_19_21 = sd(as.matrix(offers_19_21), na.rm = T)

save(sd_prices_19_21, file = "sd_prices_19_21_goup.RData")
save(sd_offers_19_21, file = "sd_offers_19_21_goup.RData")

# Remove to save memory
rm(prices_19_21) 
rm(offers_19_21) 
rm(prices1)
rm(offers1)



# Obtain the distance matrix using the pseudo-Fréchet distance

N_100 = 100 # number of xticks 
Xmin = 0 # minimum xtick

prices_19_21_100 = array(data = NA, dim = c(NHours_19_21, N_100)) # array where to save the prices corresponding to the xticks
Xvals_100 = array(data = NA, dim = c(NHours_19_21, N_100)) # array where to save the xticks of each curve

for (i in 1:NHours_19_21){
  Xmax = max(offers_19_21_without_NA[[i]]) # the maximum xtick is the maximum cumulative offer of each curve
  Xvals_100[i,] = seq(Xmin, Xmax, length.out = N_100) # grid of the offers
  step_fun = approxfun(offers_19_21_without_NA[[i]], prices_19_21_without_NA[[i]], method = "constant",
                        f = 1, rule = 2, ties = base::max) # define the supply curve
  prices_19_21_100[i,] = step_fun(Xvals_100[i,]) # save the prices corresponding to the xticks of each curve
}

# Divide cumulative offers and prices by the standard deviation
Xvals_100 = Xvals_100/sd_offers_19_21
prices_19_21_100 = prices_19_21_100/sd_prices_19_21

# Create an array where to save distances
D = array(0, dim = c(NHours_19_21, NHours_19_21)) 

# Compute the distance matrix 
for(i in 1:(NHours_19_21-1)){
  for(j in (i+1):NHours_19_21){
    D[i,j] = max(sqrt((Xvals_100[i,]-Xvals_100[j,])^2+(prices_19_21_100[i,]-prices_19_21_100[j,])^2)) 
  }
}

# Make the matrix symmetrical
for(i in 1:(NHours_19_21-1)){
  for(j in (i+1):NHours_19_21){
    D[j,i] = D[i,j]
  }
}

save(D, file = "D_19_21_goup_pf.RData")


# Compute the distance matrix using the pseudo-L2 distance

# Create an array where to save distances
D = array(0, dim = c(NHours_19_21, NHours_19_21)) 

# Compute the distances 
for(i in 1:(NHours_19_21-1)){
  for(j in (i+1):NHours_19_21){
    D[i,j] = sum(sqrt((Xvals_100[i,]-Xvals_100[j,])^2+(prices_19_21_100[i,]-prices_19_21_100[j,])^2)) 
  }
}

# Make the matrix symmetrical
for(i in 1:(NHours_19_21-1)){
  for(j in (i+1):NHours_19_21){
    D[j,i] = D[i,j]
  }
}

save(D, file = "D_19_21_goup_pL2.RData")


# Compute distance matrix using L2 distance

# Divide prices and cumulative offers by the standard deviation
for(i in 1:NHours_19_21){
  prices_19_21_without_NA[[i]] = prices_19_21_without_NA[[i]]/sd_prices_19_21
  offers_19_21_without_NA[[i]] = offers_19_21_without_NA[[i]]/sd_offers_19_21
}

# Obtain the maximum cumulative offer of each day
MaxOffer = sapply(offers_19_21_without_NA, max)

N = 100 # number of xticks 
Xmin = 0 # minimum xtick
Xmax = min(MaxOffer) # Minimum maximum cumulative offer among all the curves
Xvals = Xmin + (Xmax - Xmin)*(0:(N-1))/(N-1)

prices_19_21_L2 = array(data = NA, dim = c(NHours_19_21, N)) # array where to save the prices corresponding to the xticks

for (i in 1:NHours_19_21){
  step_fun = approxfun(offers_19_21_without_NA[[i]], prices_19_21_without_NA[[i]], method = "constant",
                        f = 1, rule = 2, ties = base::max) # define the supply curve
  prices_19_21_L2[i,] = step_fun(Xvals) # save the prices corresponding to the xticks of each curve
}


D = sqrt(Xvals[2])*as.matrix(distances(prices_19_21_L2))

save(D, file = "D_19_21_goup_L2.RData")


