####################################
# Descriptive analysis of the data
####################################


# Required libraries
library(readxl)
library(dplyr)
library(ggplot2)

par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2)


##################################################
# Years 19-20 of the upward secondary market 
##################################################

# Read the MercadoSecundarioSubir data
load("MercadoSecundarioSubir.RData")


# Read the requirements
RequerimientosSecundariaASubir = read_excel("RequerimientosSecundariaASubir.xlsx")

# Select years 19-20
offer.all_19_20 = subset(offer.all_19_21, substr(date, 1, 4) == c("2019")  | substr(date, 1, 4) == c("2020"))
price.all_19_20 = subset(price.all_19_21, substr(date, 1, 4) == c("2019")  | substr(date, 1, 4) == c("2020"))
req_19_20 = subset(RequerimientosSecundariaASubir, substr(datetime, 1, 4) == c("2019")  | substr(datetime, 1, 4) == c("2020"))

# Number of cumulative offers
NOffers_19_20 = dim(offer.all_19_20)[2]
# Number of hours
NHours_19_20 = dim(offer.all_19_20)[1]


# Add the pair (0,0) in cumulative offers and prices datasets
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

save(offers_19_20_without_NA, file = "offers_19_20_without_NA.RData")
save(prices_19_20_without_NA, file = "prices_19_20_without_NA.RData")


sd_prices_19_20 = sd(as.matrix(prices_19_20), na.rm = T)
sd_offers_19_20 = sd(as.matrix(offers_19_20), na.rm = T)

save(sd_prices_19_20, file = "sd_prices_19_20.RData")
save(sd_offers_19_20, file = "sd_offers_19_20.RData")

#-----------------------------------------------------------------
# Represent the supply curves and requirements of January 1, 2019
#-----------------------------------------------------------------

# Plot the supply curves

# Obtain the maximum cumulative offer and price to use them in the limits of the plots
max_offer = numeric()
max_price = numeric()

for (i in 1:24) {
  max_offer[i] = max(offers_19_20_without_NA[[i]])
  max_price[i] = max(prices_19_20_without_NA[[i]])
}

max_offer = max(max_offer)
max_price = max(max_price)

cols = rainbow(24, start = 0, end = 0.9) # color scale

# Create a data frame to store the data of January 1, 2019
data = data.frame()

for (i in 1:24) {
  df = data.frame(offers = offers_19_20_without_NA[[i]], prices = prices_19_20_without_NA[[i]], curve = i)
  data = rbind(data, df)
}

p = ggplot(data, aes(x = offers, y = prices, color = factor(curve))) +
  geom_step(direction = "vh") +
  xlim(0, max_offer) +
  ylim(0, max_price) +
  xlab("Power / MW") +
  ylab("Price / €/MW") +
  scale_color_manual(values = cols) +
  ggtitle("") +
  theme_bw() +
  theme(panel.grid = element_blank(),          
        strip.text = element_text(size = 18),   
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18),  
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        legend.position = "right") +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250), labels = c(0, 50, 100, 150, 200, 250)) +
  guides(colour = guide_legend(title = "Hour", ncol = 1))

print(p)


# Plot the requirements of January 1, 2019

data = data.frame(hour = 1:24, requirement = req_19_20$value[1:24])

p = ggplot(data, aes(x = hour, y = requirement)) +
  geom_point(aes(color = factor(hour)), shape = 16, size = 4) +
  xlab("Hour") +
  ylab("Requirement / MW") +
  xlim(1, 24) +
  scale_color_manual(values = cols) +
  ggtitle("") +
  theme_bw() +
  theme(panel.grid = element_blank(),          
        strip.text = element_text(size = 18),   
        axis.title = element_text(size = 18),  
        axis.text = element_text(size = 18),  
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        legend.position = "none") + 
  scale_x_continuous(breaks = 1:24, labels = 1:24)

print(p)



# Plot the curves and requirements of January 1, 2019 and crosses them

cols = rainbow(24, start = 0, end = 0.9)

data = data.frame()

for (i in c(1, 15, 18, 24)) {
  f = approxfun(offers_19_20_without_NA[[i]], prices_19_20_without_NA[[i]], method = "constant",
                 f = 1, rule = 2, ties = base::max) # function of the supply curve
  df = data.frame(
    offers = offers_19_20_without_NA[[i]],
    prices = prices_19_20_without_NA[[i]],
    curve = i,
    req = req_19_20$value[i],
    price_req = f(req_19_20$value[i])
  )
  data = rbind(data, df)
}


p = ggplot(data, aes(x = offers, y = prices, color = factor(curve))) +
  geom_step(direction = "vh") +
  geom_vline(aes(xintercept = req, color = factor(curve)), linetype = "dashed") +
  geom_point(
    aes(x = req, y = price_req),
    shape = 4,
    size = 4
  ) +
  xlab("Power / MW") +
  ylab("Prices / €/MW") +
  scale_color_manual(values = cols[c(1, 15, 18, 24)]) +
  ggtitle("") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    legend.position = "right") +
  guides(
    colour = guide_legend(
      title = "Hour",
      ncol = 1))+
  coord_cartesian(xlim = c(400, 1000), ylim = c(0, 18))  # Restrict visible range


print(p)

#----------------------------------------------------------------
# Represent the time series of prices at which we can buy 400 MW
#----------------------------------------------------------------

# Create a dataframe with the date in the first column and the values 
# of the curves for the offers_acf values

offers_acf = seq(from = 40, to = 800, by = 40)

prices_vals = data.frame(
  date = as.Date(offer.all_19_20$date, format = "%Y%m%d"),
  matrix(NA, nrow = NHours_19_20, ncol = length(offers_acf))
)

for(i in 1:NHours_19_20){
  f = approxfun(offers_19_20_without_NA[[i]], prices_19_20_without_NA[[i]], method = "constant",
                 f = 1, rule = 2, ties = base::max)
  for(j in 1:length(offers_acf)){
    prices_vals[i,j+1] = f(offers_acf[j])
  }
}

# Plot the time series of prices at which we can buy 400 MW
plot(prices_vals$date, prices_vals[, which(offers_acf == 400)+1], type = "l", xlab = "Date", ylab = "Price / €/MW", main = "")


# Plot the acf of the times series of prices at which we can buy the values of  offers_acf

cols = rainbow(length(offers_acf), start=0, end = 0.9)

# Calculate autocorrelation functions
acf_data = lapply(1:length(offers_acf), function(i) {
  time_series = ts(prices_vals[, i+1])
  acf_result = acf(time_series, lag.max = 240)
  data.frame(lag = acf_result$lag, acf = acf_result$acf, MW = offers_acf[i])
})

# Combine the acf data into a single dataframe
acf_data = do.call(rbind, acf_data)

# Create the plot
acf_plot = ggplot(acf_data, aes(x = lag, y = acf, color = factor(MW))) +
  geom_line() +
  labs(x = "Lag", y = "ACF", color = "MW") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(0, 240, by = 24)) +
  scale_y_continuous(breaks = seq(-0.1, 1, by = 0.1), labels = seq(-0.1, 1, by = 0.1)) +
  scale_color_manual(values = cols) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    legend.position = "right") 

# Adding vertical lines to the ACF plot
for (i in seq(0, 240, by = 24)) {
  acf_plot = acf_plot + geom_vline(xintercept = i, color = "grey", linetype = "dashed")
}

# Display the plot
acf_plot




##################################################
# Years 19-20 of the downward secondary market 
##################################################

# Read the MercadoSecundarioSubir data
load("MercadoSecundarioBajar.RData")


# Read the requirements
RequerimientosSecundariaABajar = read_excel("RequerimientosSecundariaABajar.xlsx")

offer.all_19_20 = subset(offer.all_19_21, substr(date, 1, 4) == c("2019")  | substr(date, 1, 4) == c("2020"))
price.all_19_20 = subset(price.all_19_21, substr(date, 1, 4) == c("2019")  | substr(date, 1, 4) == c("2020"))
req_19_20 = subset(RequerimientosSecundariaABajar, substr(datetime, 1, 4) == c("2019")  | substr(datetime, 1, 4) == c("2020"))

# Number of cumulative offers
NOffers_19_20 = dim(offer.all_19_20)[2]
# Number of hours
NHours_19_20 = dim(offer.all_19_20)[1]


# Add the pair (0,0) in cumulative offers and prices datasets
X0 = matrix(0, ncol = 1, nrow = NHours_19_20)
offers_19_20 = cbind(X0, offer.all_19_20[ , 4:NOffers_19_20])
prices_19_20 = cbind(X0, price.all_19_20[ , 4:NOffers_19_20])
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

# Create a list of prices and a list of offers where each element of each list is a list with prices/offers of an hour and a day
prices1 = prices_19_20[1,] %>% as.numeric %>% na.omit
offers1 = offers_19_20[1,] %>% as.numeric %>% na.omit
prices_19_20_without_NA = list(prices1)
offers_19_20_without_NA = list(offers1)

for (i in 2:NHours_19_20){
  price_19_20_without_NA = prices_19_20[i,] %>% as.numeric %>% na.omit
  offer_19_20_without_NA = offers_19_20[i,] %>% as.numeric %>% na.omit
  prices_19_20_without_NA = append(prices_19_20_without_NA, list(price_19_20_without_NA))
  offers_19_20_without_NA = append(offers_19_20_without_NA, list(offer_19_20_without_NA))
}



#------------------------------------------------------------------
# Represent the supply curves and requirements of January 1, 2019
#------------------------------------------------------------------

# Obtain the maximum offer and price to use them in the limits of the plots
max_offer = numeric()
max_price = numeric()

for (i in 1:24) {
  max_offer[i] = max(offers_19_20_without_NA[[i]])
  max_price[i] = max(prices_19_20_without_NA[[i]])
}

max_offer = max(max_offer)
max_price = max(max_price)

cols = rainbow(24, start = 0, end = 0.9)

# Create a data frame to store all the data
data = data.frame()

for (i in 1:24) {
  df = data.frame(offers = offers_19_20_without_NA[[i]], prices = prices_19_20_without_NA[[i]], curve = i)
  data = rbind(data, df)
}

p = ggplot(data, aes(x = offers, y = prices, color = factor(curve))) +
  geom_step(direction = "vh") +
  xlim(0, max_offer) +
  ylim(0, max_price) +
  xlab("Power / MW") +
  ylab("Price / €/MW") +
  scale_color_manual(values = cols) +
  ggtitle("") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 18),  
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18),  
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        legend.position = "right") +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250), labels = c(0, 50, 100, 150, 200, 250)) +
  guides(colour = guide_legend(title = "Hour", ncol = 1))

print(p)


# Plot the requirements of January 1, 2019

data = data.frame(hour = 1:24, requirement = req_19_20$value[1:24])

p = ggplot(data, aes(x = hour, y = requirement)) +
  geom_point(aes(color = factor(hour)), shape = 16, size = 4) +
  xlab("Hour") +
  ylab("Requirement / MW") +
  xlim(1, 24) +
  scale_color_manual(values = cols) +
  ggtitle("") +
  theme_bw() +
  theme(panel.grid = element_blank(),          
        strip.text = element_text(size = 18),   
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18),  
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        legend.position = "none") + 
  scale_x_continuous(breaks = 1:24, labels = 1:24)

print(p)


# Plot the curves and requirements of January 1, 2019 and crosses them

cols = rainbow(24, start = 0, end = 0.9)

data = data.frame()

for (i in c(1, 6, 18)) {
  f = approxfun(offers_19_20_without_NA[[i]], prices_19_20_without_NA[[i]], method = "constant",
                 f = 1, rule = 2, ties = base::max)
  df = data.frame(
    offers = offers_19_20_without_NA[[i]],
    prices = prices_19_20_without_NA[[i]],
    curve = i,
    req = req_19_20$value[i],
    price_req = f(req_19_20$value[i])
  )
  data = rbind(data, df)
}


p = ggplot(data, aes(x = offers, y = prices, color = factor(curve))) +
  geom_step(direction = "vh") +
  geom_vline(aes(xintercept = req, color = factor(curve)), linetype = "dashed") +
  geom_point(
    aes(x = req, y = price_req),
    shape = 4,
    size = 4
  ) +
  xlab("Power / MW") +
  ylab("Price / €/MW") +
  scale_color_manual(values = cols[c(1, 6, 18)]) +
  ggtitle("") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    legend.position = "right") +
  guides(
    colour = guide_legend(
      title = "Hour",
      ncol = 1))+
  coord_cartesian(xlim = c(300, 700), ylim = c(0, 18))  # Restrict visible range


print(p)


#------------------------------------------------------------------
# Represent the time series of prices at which we can buy 400 MW
#------------------------------------------------------------------

# Create a dataframe with the date in the first column and the values 
# of the curves for the offers_acf values

offers_acf = seq(from = 40, to = 800, by = 40)

prices_vals = data.frame(
  date = as.Date(offer.all_19_20$date, format = "%Y%m%d"),
  matrix(NA, nrow = NHours_19_20, ncol = length(offers_acf))
)


for(i in 1:NHours_19_20){
  f = approxfun(offers_19_20_without_NA[[i]], prices_19_20_without_NA[[i]], method = "constant",
                 f = 1, rule = 2, ties = base::max)
  for(j in 1:length(offers_acf)){
    prices_vals[i,j+1] = f(offers_acf[j])
  }
}

# Plot the time series of prices at which we can buy 400 MW
plot(prices_vals$date, prices_vals[, which(offers_acf == 400)+1], type = "l", xlab = "Date", ylab = "Price / €/MW", main = "")

# Plot the acf of the times series of prices at which we can buy the values of offers_acf

cols = rainbow(length(offers_acf), start=0, end = 0.9)

# Calculate autocorrelation functions
acf_data = lapply(1:length(offers_acf), function(i) {
  time_series = ts(prices_vals[, i+1])
  acf_result = acf(time_series, lag.max = 240)
  data.frame(lag = acf_result$lag, acf = acf_result$acf, MW = offers_acf[i])
})

# Combine the acf data into a single dataframe
acf_data = do.call(rbind, acf_data)

# Create a plot using ggplot2
acf_plot = ggplot(acf_data, aes(x = lag, y = acf, color = factor(MW))) +
  geom_line() +
  labs(x = "Lag", y = "ACF", color = "MW") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(0, 240, by = 24)) +
  scale_y_continuous(breaks = seq(-0.1, 1, by = 0.1), labels = seq(-0.1, 1, by = 0.1)) +
  scale_color_manual(values = cols) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    legend.position = "right") 

# Adding vertical lines to the ACF plot
for (i in seq(0, 240, by = 24)) {
  acf_plot = acf_plot + geom_vline(xintercept = i, color = "grey", linetype = "dashed")
}

# Display the plot
acf_plot



