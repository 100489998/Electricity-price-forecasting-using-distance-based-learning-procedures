#############################################################################################
# Training and testing the methods (Pseudo-L2 distance and downward secondary market)
#############################################################################################

# Required libraries
library(parsnip)
library(randomForest)
library(ranger)
library(dplyr)
library(readxl)
library(ggplot2)
library(distances)

par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2)

NHours = 26304 # sum of the hours of years 2019, 2020 and 2021
TestSize = 365 # number of days of year 2021, test set
TrainSize = NHours - TestSize*24 # number of hours of the train set (2019 and 2020)

# Load matrix
load("D_19_21_godown_pL2.RData")

# Load cumulative offers and prices for price forecasting 

load("offers_19_21_without_NA_godown.Rdata")
load("prices_19_21_without_NA_godown.Rdata")

load("sd_offers_19_21_godown.RData")
load("sd_prices_19_21_godown.RData")

# Load requirements

RequerimientosSecundariaABajar = read_excel("RequerimientosSecundariaABajar.xlsx")
req_19_21 = subset(RequerimientosSecundariaABajar, substr(datetime, 1, 4) == c("2019")  | substr(datetime, 1, 4) == c("2020") | substr(datetime, 1, 4) == c("2021"))

# -------------------------------------------------------------------
# Function to train the random forest model of the RF + 1-NN method 
# -------------------------------------------------------------------

# Input of RFmodelingDA function:
# - D: distance matrix
# - Indexes1, Indexes 2: First hour of the days between which the distances 
#                        between supply curves are to be calculated
# - Indexes: logical vector indicating the indexes where Indexes2<Indexes1

RFmodelingDA = function(D, Indexes1, Indexes2, Indexes)
{
  # matrix where to save the distances between the curves
  X = matrix(NA, nrow = length(Indexes1)*length(Indexes2), ncol = 24*2)
  
  # Compute distances between curves of the same hour in different days
  # d(C_t,C_s), d(C_{t-1},C_{s-1}), ..., d(C_{t-23}, C_{s-23})
  for (l in 1:24){
    Xtmp = D[Indexes1-l, Indexes2-l]
    X[,l] = c(Xtmp)
  }
  
  # Compute the distances between curves of the same hour in consecutive days
  # d(C_t,C_{s+24}), d(C_{t-1}, C_{s+23}), ..., d(C_{t-23}, C_{s+1})
  for (l in 1:24){
    Xtmp = D[Indexes1-l, Indexes2+24-l]
    X[,l+24] = c(Xtmp)
  }
  
  # Compute the distances to be predicted
  # d(C_{t+24},C_{s+24}), d(C_{t+23}, C_{s+23}), ..., d(C_{t+1},C_{s+1})
  Y = matrix(NA, nrow = length(Indexes1)*length(Indexes2), ncol = 24)
  for (Lag in 1:24){
    Ytmp = D[Indexes1+24-Lag, Indexes2+24-Lag]
    Y[,Lag] = c(Ytmp)
  }
  
  # Keep only distances Indexes2<Indexes1
  X = X[Indexes, ]
  Y = Y[Indexes, ]
  
  # Obtain the mean of the distances of each day
  Y = apply(Y, 1, mean)
  Y = c(Y)
  O = order(Y) # indexes to order the Y vector
  
  # Select randomly 20000 hours to save computational cost
  set.seed(1234)
  II = sample(O[1:(0.99*length(Y))], 20000) 
  training_data = data.frame(X[II,], Y = Y[II])
  
  # Train the random forest 
  RFfit = rand_forest(trees = 500) %>%
    set_engine("ranger", importance = "impurity", seed = 1234) %>%
    set_mode("regression") %>% fit(Y ~ . , data = training_data)
  return(RFfit)
}

# ------------------------------------------------------------------
# Function to train the random forest models of RF(h) + 1-NN method
# ------------------------------------------------------------------

# Input of RFmodeling function:
# - D: distance matrix
# - Indexes1, Indexes 2: First hour of the days between which the distances 
#                        between supply curves are to be calculated
# - Indexes: logical vector indicating the indexes where Indexes2<Indexes1
# - Lag=25-hour where hour is the hour of the curves to be predicted

RFmodeling = function(D, Indexes1, Indexes2, Indexes, Lag)
{
  # matrix where save the distances between the curves
  X = matrix(NA, nrow = length(Indexes1)*length(Indexes2), ncol = 24*2)
  
  # Compute distances between curves of the same hour in different days
  # d(C_t,C_s), d(C_{t-1},C_{s-1}), ..., d(C_{t-23}, C_{s-23})  
  for (l in 1:24){
    Xtmp = D[Indexes1-l, Indexes2-l]
    X[,l] = c(Xtmp)
  }
  
  # Compute the distances between curves of the same hour in consecutive days
  #d(C_{t}, C_{s+h}), d(C_{t-1}, C_{s+h}),..., d(C_{t-23}, C_{s+h})
  for (l in 1:24){
    Xtmp = D[Indexes1-l, Indexes2+24-Lag] # Lag en Indexes2
    X[, l+24] = c(Xtmp)
  }
  
  # Predicted distances d(C_t+h, C_s+h).
  Y = D[Indexes1+24-Lag, Indexes2+24-Lag]
  Y = c(Y)
  
  # Keep only distances when Indexes2<Indexes1
  X = X[Indexes, ]
  Y = Y[Indexes]
  O = order(Y)
  
  # Select 20000 hours to save computational cost
  set.seed(1234)
  II = sample(O[1:(0.99*length(Y))], 20000) 
  training_data = data.frame(X[II, ], Y = Y[II])
  
  # Train the random forest
  RFfit = rand_forest(trees = 500) %>%
    set_engine("ranger", importance = "impurity", seed = 1234) %>%
    set_mode("regression") %>% fit(Y ~ . , data = training_data)
  return(RFfit)
}

# ---------------------
# Indexes for training.
# ---------------------

Indexes1 = seq(25, (TrainSize-23), 24) # First hour of the days of the train set
Indexes2 = seq(25, (TrainSize-23), 24) # First hour of the days of the train set

# Indexes ensures that Indexes1 (t) > Indexes2 (s)
Indexes = vector(mode = "logical", length = length(Indexes1)*length(Indexes2)) 
k = 0 
for (i in Indexes1){
  for (j in Indexes2){
    k = k + 1
    if (i <= j){
      Indexes[k] = FALSE
    }
    else{
      Indexes[k] = TRUE
    }
  }
}


# -----------------
# Training models
# -----------------

rfFit1DA = RFmodelingDA(D, Indexes1, Indexes2, Indexes)
R2DA = rfFit1DA$fit$r.squared # R^2

# Save model and R^2
save(rfFit1DA, file = "rfFit1DA.RData")
save(R2DA, file = "R2DA_pL2_godown.RData")

for(Lag in 1:24){
  RFfit = RFmodeling(D, Indexes1, Indexes2, Indexes, Lag)
  assign(paste0("RFfit", Lag), RFfit)
  cat(Lag, "\r")
}

# Obtain R^2 values
model_names = paste0("RFfit", 1:24)
R2 = numeric(length(model_names))

for (i in 1:length(model_names)) {
  model = get(model_names[i])
  R2[i] = model$fit$r.squared
}

save(R2, file = "R2_pL2_godown.RData")

# Save models
save(RFfit1, file = "RFfit1.RData")
save(RFfit2, file = "RFfit2.RData")
save(RFfit3, file = "RFfit3.RData")
save(RFfit4, file = "RFfit4.RData")
save(RFfit5, file = "RFfit5.RData")
save(RFfit6, file = "RFfit6.RData")
save(RFfit7, file = "RFfit7.RData")
save(RFfit8, file = "RFfit8.RData")
save(RFfit9, file = "RFfit9.RData")
save(RFfit10, file = "RFfit10.RData")
save(RFfit11, file = "RFfit11.RData")
save(RFfit12, file = "RFfit12.RData")
save(RFfit13, file = "RFfit13.RData")
save(RFfit14, file = "RFfit14.RData")
save(RFfit15, file = "RFfit15.RData")
save(RFfit16, file = "RFfit16.RData")
save(RFfit17, file = "RFfit17.RData")
save(RFfit18, file = "RFfit18.RData")
save(RFfit19, file = "RFfit19.RData")
save(RFfit20, file = "RFfit20.RData")
save(RFfit21, file = "RFfit21.RData")
save(RFfit22, file = "RFfit22.RData")
save(RFfit23, file = "RFfit23.RData")
save(RFfit24, file = "RFfit24.RData")

# Remove models for memory resons. They will subsequently be loaded when necessary
rm(RFfit1, RFfit2, RFfit3, RFfit4, RFfit5, RFfit6, RFfit7, RFfit8, RFfit9, RFfit10, 
   RFfit11, RFfit12, RFfit13, RFfit14, RFfit15, RFfit16, RFfit17, RFfit18, RFfit19, 
   RFfit20, RFfit21, RFfit22, RFfit23, RFfit24)

# -----------------------------------------------------------------------
# Analysis of the prediction errors of the supply curves in the test set
# ---------------------------------------------- ------------------------

# RF + 1-NN method testing

errorDA = array(NA, c(TestSize, 24)) # array where to save errors of curve predictions
knnIndexesDA = array(NA, c(TestSize, 1)) # array where to save indexes of predictions 
# index corresponding to the first hour of the day
for (j in 1:TestSize){
  i = TrainSize+(j-1)*24+1 # first hour of the days whose curves are to be predicted
  
  Indexes1 = i # equal Indexes1 to this hour
  Indexes3 = seq(25, (i-24), 24) # first hour of all the days before day corresponding to hour i. length(Indexes3)=number of days before hour i
  
  DD = numeric(length(Indexes3)) # vector where to save predictions of the distances between curves of day corresponding to i and all the previous days
  
  X = matrix(NA, nrow = length(Indexes3), ncol = 24*2) # matrix where to save the input of the random forest 
  
  # Compute distances between curves of the same hour of the previous day to predict (D-1) and all the previous days to this one
  for (l in 1:24){
    Xtmp = D[Indexes1-l, Indexes3-l] 
    X[, l] = c(Xtmp)
  }
  
  # Compute distances between curves of the same hour of the previous day to predict (D-1) and all the previous days to this one +1
  for (l in 1:24){
    Xtmp = D[Indexes1-l, Indexes3+24-l] 
    X[, l+24] = c(Xtmp)
  }
  
  X = data.frame(X)
  
  
  DD[1:length(Indexes3)] = predict(rfFit1DA, X)$.pred # Predictions of the distances between day i (D) and all the previous days
  knnIndex = Indexes3[which.min(DD[1:length(Indexes3)])] # First hour indexes of curves at shortest distances
  knnIndexesDA[j] = knnIndex 
  
  # Prediction errors
  for (h in 1:24){
    errorDA[j, h] = D[i+h-1, knnIndex+h-1] # Distance between predicted and real curves
  }
  
  cat(j, "\r")
}


save(errorDA, file = "errorDA.RData")
save(knnIndexesDA, file = "knnIndexesDA.RData")


# RF(h) + 1-NN method testing

for(Lag in 1:24){
  
  load(paste0("RFfit", Lag, ".RData")) # load the model
  
  error = array(NA, c(TestSize)) # array where to save the curve prediction errors
  knnIndexes = array(NA, c(TestSize)) # array where to save the prediction indexes
  
  for (j in 1:TestSize){
    i = TrainSize+(j-1)*24+1 # first hour of the days whose curves are to be predicted
    Indexes1 = i # equal Indexes1 to this hour 
    Indexes3 = seq(25, (i-Lag), 24) # first hour of all the days before day corresponding to hour i. length(Indexes3)=number of days before hour i 
    
    # Distances predictions
    DD = numeric(length(Indexes3)) # vector where to save the distance predictions
    
    X = matrix(NA, nrow = length(Indexes3), ncol = 24*2)
    
    # Compute distances between curves of the same hour of the previous day to predict (D-1) and all the previous days to this one
    for (l in 1:24){
      Xtmp = D[Indexes1-l, Indexes3-l] 
      X[, l] = c(Xtmp)
    }
    
    # Compute distances between curves of the previous day to predict (D-1) and curve of hour 25-Lag of all the previous days to this one +1
    for (l in 1:24){
      Xtmp = D[Indexes1-l, Indexes3+24-Lag] 
      X[, l+24] = c(Xtmp)
    }
    X = data.frame(X)
    
    DD[1:length(Indexes3)] = predict(get(paste0("RFfit", Lag)), X)$.pred # Distance predictions
    knnIndexes[j] = Indexes3[which.min(DD[1:length(Indexes3)])] # First hour indexes of curves at shortest distances
    
    # Prediction errors
    h = 25-Lag
    error[j] = D[i+h-1, knnIndexes[j]+h-1] # Distances between predicted and real curves
    
    cat(j, "\r")
  }
  
  save(error, file = paste0("error_rf", Lag, ".RData"))
  save(knnIndexes, file = paste0("knnIndexes_rf", Lag, ".RData"))
  
  
  rm(list=paste0("RFfit", Lag)) # Delete the model in the environment for memory reasons
}


# Benchmark methods testing

errorBenchmark.Sum = array(NA, c(TestSize, 24)) # Array where to save curve prediction errors using the benchmark method with sum
errorBenchmark.Max = array(NA, c(TestSize, 24)) # Array where to save curve prediction errors using the benchmark method with maximum
knnIndexSum = numeric(length = TestSize) # Vector where to save indexes of the predictions with the benchmark method with sum
knnIndexMax = numeric(length = TestSize) # Vector where to save indexes of the predictions with the benchmark method with maximum


for (j in 1:TestSize){
  d0 = TrainSize/24+j-1 # previous day (D-1) to the day to predict 
  
  DailyDistanceSum = array(0, c(d0-1,1)) # Vector where to save the sum of the distances between curves of the same hour of day d0-1 and the previous days
  DailyDistanceMax = array(0, c(d0-1,1)) # vector where to save the maximum of the distances between curves of the same hour of day d0-1 and the previous days
  
  # For loop in the days prior to the day before the day to predict (d0)
  for (d in 1:(d0-1)){
    # For loop in the hours of the day
    for (h in 1:24){
      # DailyDistanceSum for 1-NN (sum) 
      DailyDistanceSum[d] = DailyDistanceSum[d]+D[(d0-1)*24+h,(d-1)*24+h] # Sum of the distances between curves of the same hour of days d0 and d
      # DailyDistanceMax for 1-NN (max)
      DailyDistanceMax[d] = max(DailyDistanceMax[d], D[(d0-1)*24+h,(d-1)*24+h]) # Maximum of the distances between curves of the same hour of days d0 and d
    }
  }
  knnIndexSum[j] = which.min(DailyDistanceSum) # day d where the minimum sum of distances is achieved
  knnIndexMax[j] = which.min(DailyDistanceMax) # day d where the minimum of the maximum of the distances is achieved
  
  
  # Errors between the predicted and real curves
  for (h in 1:24){
    errorBenchmark.Sum[j,h] = D[d0*24+h,(knnIndexSum[j]+1)*24+h] # matrix where to save the prediction errors: errorBenchmark.Sum[j,h]: prediction error of the h-th curve of the j-th day in the test set
    errorBenchmark.Max[j,h] = D[d0*24+h,(knnIndexMax[j]+1)*24+h] # matrix where to save the prediction errors: errorBenchmark.Sum[j,h]: prediction error of the h-th curve of the j-th day in the test set
  }
}

save(errorBenchmark.Sum, file = "errorBenchmark.Sum.RData")
save(errorBenchmark.Max, file = "errorBenchmark.Max.RData")
save(knnIndexSum, file = "knnIndexSum.RData")
save(knnIndexMax, file = "knnIndexMax.RData")



# Comparison of all the methods 

# Means of the hourly errors of the RF + 1-NN method

error_DA_means = numeric()

for (hour in 1:24) {
  error_DA_means[hour] = mean(errorDA[, hour])
}

# Means of the hourly errors of the RF(h) + 1-NN method

error_rf_means = numeric()

# Model with input Lag is used to predict curves of hour 25-Lag
lags = rev(1:24)

for (j in 1:24) {
  load(paste0("error_rf", lags[j], ".RData"))
  error_rf_means[j] = mean(error)
}


# Means of the hourly errors of benchmark methods

error_knn_sum_means = numeric()
error_knn_max_means = numeric()

for (hour in 1:24) {
  error_knn_sum_means[hour] = mean(errorBenchmark.Sum[, hour])
  error_knn_max_means[hour] = mean(errorBenchmark.Max[, hour])
}


# Plot the results with ggplot

# Create a data frame
data = data.frame(
  Hour = 1:24,
  error_rf_means,
  error_DA_means,
  error_knn_max_means,
  error_knn_sum_means
)

data = data.frame(
  Hour = seq(1, 24),
  L2error_rf_means,
  L2error_DA_means,
  L2error_knn_max_means,
  L2error_knn_sum_means
)

method_colors = c("RF(h) + 1-NN" = "black", "RF + 1-NN" = "red", "1-NN (max)" = "blue", "1-NN (sum)" = "green")
min = round(min(L2error_DA_means, L2error_rf_means, L2error_knn_max_means, L2error_knn_sum_means))
max = round(max(L2error_DA_means, L2error_rf_means, L2error_knn_max_means, L2error_knn_sum_means))
min = 25
max = 225
breaks = seq(min, max, by = 25)

# Create the plot using ggplot2
ggplot(data, aes(x = Hour)) +
  geom_point(aes(y = L2error_rf_means, color = "RF(h) + 1-NN"), size = 4, shape = 16) +
  geom_point(aes(y = L2error_DA_means, color = "RF + 1-NN"), size = 4, shape = 16) +
  geom_point(aes(y = L2error_knn_max_means, color = "1-NN (max)"), size = 4, shape = 16) +
  geom_point(aes(y = L2error_knn_sum_means, color = "1-NN (sum)"), size = 4, shape = 16) +
  geom_hline(yintercept = mean(data$L2error_rf_means), linetype = "solid", color = "black") +
  geom_hline(yintercept = mean(data$L2error_DA_means), linetype = "solid", color = "red") +
  geom_hline(yintercept = mean(data$L2error_knn_max_means), linetype = "solid", color = "blue") +
  geom_hline(yintercept = mean(data$L2error_knn_sum_means), linetype = "solid", color = "green") +
  scale_y_continuous(trans = "log", breaks = breaks, labels = breaks) +
  scale_x_continuous(breaks = seq(1,24), labels = seq(1,24))+
  labs(x = "Hour", y = "Mean error", title = "") +
  scale_color_manual(values = method_colors) +  # Set method colors
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
  guides(color = guide_legend(title = "Method"))+
  coord_cartesian(ylim = c(25,225))

  
  
# -------------------------------------------------------------------------
# Analysis of the prediction errors of the supply curves in the test set 
# around the requirements
# ---------------------------------------------- --------------------------

# RF + 1-NN method

errors_res_DA = matrix(NA, nrow = TestSize, ncol = 24) 
load("knnIndexesDA.RData")

for (h in 1:24){
  for(j in 1:TestSize){
    i = TrainSize+(j-1)*24+h
    Xvals = seq(req_19_21$value[i]-10, req_19_21$value[i]+10, length.out = 100)/sd_offers_19_21  # sequence of 100 equidistant points around the requirement
    
    step_fun_real = approxfun(offers_19_21_without_NA[[i]]/sd_offers_19_21
                               , prices_19_21_without_NA[[i]]/sd_prices_19_21, method = "constant",
                               f = 1, rule = 2, ties = base::max) # function of the real supply curve
    prices_real = step_fun_real(Xvals) # Obtain real prices
    
    step_fun_pred = approxfun(offers_19_21_without_NA[[knnIndexesDA[j]+(h-1)]]/sd_offers_19_21,
                               prices_19_21_without_NA[[knnIndexesDA[j]+(h-1)]]/sd_prices_19_21, method = "constant",
                               f = 1, rule = 2, ties = base::max) # function of the predicted supply curve
    prices_pred = step_fun_pred(Xvals) # Obtain predicted prices
    
    errors_res_DA[j,h]= sum(sqrt((prices_real-prices_pred)^2))  # Distance between predicted and real curves
  }
  cat(h)
}


# RF(h) + 1-NN method
errors_res_rf = matrix(NA, nrow = TestSize, ncol = 24) 

for (lag in 24:1){
  h = 25-lag
  load(paste0("knnIndexes_rf",lag, ".RData"))
  
  for(j in 1:TestSize){
    i = TrainSize+(j-1)*24+h
    Xvals = seq(req_19_21$value[i]-10, req_19_21$value[i]+10, length.out = 100)/sd_offers_19_21 # sequence of 100 equidistant points around the requirement
    step_fun_real = approxfun(offers_19_21_without_NA[[i]]/sd_offers_19_21, 
                               prices_19_21_without_NA[[i]]/sd_prices_19_21, method = "constant",
                               f = 1, rule = 2, ties = 'max') # function of real the supply curve 
    prices_real = step_fun_real(Xvals) # Obtain real prices
    
    step_fun_pred = approxfun(offers_19_21_without_NA[[knnIndexes[j]+(h-1)]]/sd_offers_19_21,
                               prices_19_21_without_NA[[knnIndexes[j]+(h-1)]]/sd_prices_19_21, method = "constant",
                               f = 1, rule = 2, ties = base::max) # function of the predicted supply curve
    prices_pred = step_fun_pred(Xvals) # Obtain predicted prices
    
    errors_res_rf[j,h]= sum(sqrt((prices_real-prices_pred)^2))  # Distance between predicted and real curves
  }
  cat(lag)
}


# Benchmark methods

errors_res_bsum = matrix(NA, nrow = TestSize, ncol = 24) # in each column, the prices of the different days but the same hour
errors_res_bmax = matrix(NA, nrow = TestSize, ncol = 24) # in each column, the prices of the different days but the same hour

load("knnIndexSum.RData")
load("knnIndexMax.RData")

# 1-NN (max)

for (h in 1:24){
  for(j in 1:TestSize){
    i = TrainSize+(j-1)*24+h
    Xvals = seq(req_19_21$value[i]-10, req_19_21$value[i]+10, length.out = 100)/sd_offers_19_21
    
    step_fun_real = approxfun(offers_19_21_without_NA[[i]]/sd_offers_19_21
                               , prices_19_21_without_NA[[i]]/sd_prices_19_21, method = "constant",
                               f = 1, rule = 2, ties = base::max) # function of the real supply curve
    prices_real = step_fun_real(Xvals) # Obtain real prices
    
    step_fun_pred = approxfun(offers_19_21_without_NA[[knnIndexMax[j]+(h-1)]]/sd_offers_19_21, 
                               prices_19_21_without_NA[[knnIndexMax[j]+(h-1)]]/sd_prices_19_21, method = "constant",
                               f = 1, rule = 2, ties = base::max) # function of the predicted supply curve
    prices_pred = step_fun_pred(Xvals) # Obtain predicted prices
    
    errors_res_bmax[j,h]= sum(sqrt((prices_real-prices_pred)^2))  # Distance between predicted and real curves
  }
  cat(h)
}

# 1-NN (sum)

for (h in 1:24){
  for(j in 1:TestSize){
    i = TrainSize+(j-1)*24+h
    Xvals = seq(req_19_21$value[i]-10, req_19_21$value[i]+10, length.out = 100)/sd_offers_19_21
    
    step_fun_real = approxfun(offers_19_21_without_NA[[i]]/sd_offers_19_21
                               , prices_19_21_without_NA[[i]]/sd_prices_19_21, method = "constant",
                               f = 1, rule = 2, ties = base::max) # function of the real supply curve
    prices_real = step_fun_real(Xvals) # Obtain real prices
    
    step_fun_pred = approxfun(offers_19_21_without_NA[[knnIndexSum[j]+(h-1)]]/sd_offers_19_21, 
                               prices_19_21_without_NA[[knnIndexSum[j]+(h-1)]]/sd_prices_19_21, method = "constant",
                               f = 1, rule = 2, ties = base::max) # function of the predicted supply curve
    prices_pred = step_fun_pred(Xvals) # Obtain predicted prices
    
    errors_res_bsum[j,h]= sum(sqrt((prices_real-prices_pred)^2))  # Distance between predicted and real curve
  }
  cat(h)
}


# Mean errors

# RF + 1-NN
errors_res_DA_means = colMeans(errors_res_DA)

# RF(h) + 1-NN
errors_res_rf_means = colMeans(errors_res_rf)

# Benchmark methods
errors_res_bmax_means = colMeans(errors_res_bmax)
errors_res_bsum_means = colMeans(errors_res_bsum)

# Create a data frame
data = data.frame(
  Hour = 1:24,
  errors_res_rf_means,
  errors_res_DA_means,
  errors_res_bmax_means,
  errors_res_bsum_means
)

method_colors = c("RF(h) + 1-NN" = "black", "RF + 1-NN" = "red", "1-NN (max)" = "blue", "1-NN (sum)" = "green")
(min_errors = min(errors_res_rf_means, errors_res_DA_means, errors_res_bmax_means, errors_res_bsum_means))
(max_errors = max(errors_res_rf_means, errors_res_DA_means, errors_res_bmax_means, errors_res_bsum_means))

min = 14
max = 40
breaks = seq(min, max, by = 5)

# Create the plot using ggplot2
ggplot(data, aes(x = Hour)) +
  geom_point(aes(y = errors_res_rf_means, color = "RF(h) + 1-NN"), size = 4, shape = 16) +
  geom_point(aes(y = errors_res_DA_means, color = "RF + 1-NN"), size = 4, shape = 16) +
  geom_point(aes(y = errors_res_bmax_means, color = "1-NN (max)"), size = 4, shape = 16) +
  geom_point(aes(y = errors_res_bsum_means, color = "1-NN (sum)"), size = 4, shape = 16) +
  geom_hline(yintercept = mean(data$errors_res_rf_means), linetype = "solid", color = "black") +
  geom_hline(yintercept = mean(data$errors_res_DA_means), linetype = "solid", color = "red") +
  geom_hline(yintercept = mean(data$errors_res_bmax_means), linetype = "solid", color = "blue") +
  geom_hline(yintercept = mean(data$errors_res_bsum_means), linetype = "solid", color = "green") +
  scale_y_continuous(trans = "log", breaks = breaks, labels = breaks) +
  scale_x_continuous(breaks = 1:24, labels = 1:24)+
  labs(x = "Hour", y = "Mean error", title = "") +
  scale_color_manual(values = method_colors) +  # Set method colors
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
  guides(color = guide_legend(title = "Method"))


#####################
# Price forecasting
#####################

# Using RF + 1-NN method

prices_DA = matrix(NA, nrow = TestSize, ncol = 24) # in each column, the prices of the different days but the same hour
load("knnIndexesDA.RData")

for(h in 1:24){
  for(j in 1:TestSize){
    step_fun = approxfun(offers_19_21_without_NA[[knnIndexesDA[j]+(h-1)]], prices_19_21_without_NA[[knnIndexesDA[j]+(h-1)]], method = "constant",
                          f = 1, rule = 2, ties = base::max) # function of the supply curve
    i = TrainSize+(j-1)*24+h
    prices_DA[j, h] = step_fun(req_19_21$value[i]) # evaluate in the corresponding requirement
  }
}


# Using RF(h) + 1-NN method

# Obtain the price by evaluating the corresponding curve
prices_rf = matrix(NA, nrow = TestSize, ncol = 24) # in each column, the prices of the different days but the same hour

for (lag in 24:1){
  h = 25-lag # Model using Lag is used to predict curves of hour 25-Lag
  for(j in 1:TestSize){
    load(paste0("knnIndexes_rf", lag, ".RData")) # load the indexes of the predictions
    step_fun = approxfun(offers_19_21_without_NA[[knnIndexes[j]+(h-1)]], prices_19_21_without_NA[[knnIndexes[j]+(h-1)]], method = "constant",
                          f = 1, rule = 2, ties = base::max) # function of the supply curve
    i = TrainSize+(j-1)*24+h 
    prices_rf[j, h]= step_fun(req_19_21$value[i]) # evaluate the step function in the corresponding requirement
  }
  cat(lag)
}


# Using benchmark methods

prices_benchmark_sum = matrix(NA, nrow = TestSize, ncol = 24) # in each column, the prices of the different days but the same hour
prices_benchmark_max = matrix(NA, nrow = TestSize, ncol = 24) # in each column, the prices of the different days but the same hour

load("knnIndexSum.RData")
load("knnIndexMax.RData")


for(h in 1:24){
  for(j in 1:TestSize){
    step_fun = approxfun(offers_19_21_without_NA[[knnIndexSum[j]+(h-1)]], prices_19_21_without_NA[[knnIndexSum[j]+(h-1)]], method = "constant",
                          f = 1, rule = 2, ties = base::max) # function of the supply curve 
    i = TrainSize+(j-1)*24+h
    prices_benchmark_sum[j, h] = step_fun(req_19_21$value[i]) # evaluate in the corresponding requirement
  }
}

for(h in 1:24){
  for(j in 1:TestSize){
    step_fun = approxfun(offers_19_21_without_NA[[knnIndexMax[j]+(h-1)]], prices_19_21_without_NA[[knnIndexMax[j]+(h-1)]], method = "constant",
                          f = 1, rule = 2, ties = base::max) # function of the supply curve  }
    i = TrainSize+(j-1)*24+h
    prices_benchmark_max[j, h] = step_fun(req_19_21$value[i]) # evaluate in the corresponding requirement
  }
}



# Real prices

real_prices = matrix(NA, nrow = TestSize, ncol =24)
for (h in 1:24){
  for(j in 1:TestSize){
    i = TrainSize+(j-1)*24+h
    step_fun = approxfun(offers_19_21_without_NA[[i]], prices_19_21_without_NA[[i]], method = "constant",
                          f = 1, rule = 2, ties = base::max) # function of the supply curve
    real_prices[j,h]= step_fun(req_19_21$value[i])
  }
  cat(h)
}

# Errors of the electricity price predictions

# RF + 1-NN
(rmse_DA = sqrt(1/(365*24)*sum((real_prices-prices_DA)^2))) 
(mae_DA = mean(abs(real_prices - prices_DA))) 


# RF(h) + 1-NN
(rmse_rf = sqrt(1/(365*24)*sum((real_prices-prices_rf)^2))) 
(mae_rf = mean(abs(real_prices - prices_rf))) 

# Benchmark methods

(rmse_bmax = sqrt(1/(365*24)*sum((real_prices-prices_benchmark_max)^2))) 
(mae_bmax = mean(abs(real_prices - prices_benchmark_max))) 


(rmse_bsum = sqrt(1/(365*24)*sum((real_prices-prices_benchmark_sum)^2))) 
(mae_bsum = mean(abs(real_prices - prices_benchmark_sum))) 


# Save results

rmse_godown_pL2 = c(rmse_bmax, rmse_bsum, rmse_DA, rmse_rf)
save(rmse_godown_pL2, file = "rmse_godown_pL2.RData")
mae_godown_pL2 = c(mae_bmax, mae_bsum, mae_DA, mae_rf)
save(mae_godown_pL2, file = "mae_godown_pL2.RData")
