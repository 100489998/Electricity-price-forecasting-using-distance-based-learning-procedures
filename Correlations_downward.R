###################################################################################
# Correlations between the 48 independent variables and the 24 dependent variables
# Downward secondary market
###################################################################################

# Required libraries 
library(corrplot)
library(GGally)
library(ggplot2)


NHours = 26304 # sum of the hours of years 2019, 2020 and 2021
TestSize = 365 # days of year 2021, test set
TrainSize = NHours - TestSize*24 # hours in the train set 

Indexes1 <- seq(25, (TrainSize-23), 24) # first hour of the days of the train set
Indexes2 <- seq(25, (TrainSize-23), 24) # first hour of the days of the train set

Indexes = vector(mode = "logical", length = length(Indexes1)*length(Indexes2)) 
k = 0 # Indexes ensures that Indexes1 (t) > Indexes2 (s)
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

# L2 distance

load("D_19_21_godown_L2.RData")

X = matrix(NA, nrow = length(Indexes1)*length(Indexes2), ncol = 24*2)

for (l in 24:1){
  Xtmp <- D[Indexes1-l,Indexes2-l] 
  X[,l] = c(Xtmp)
}

for (l in 24:1){
  Xtmp <- D[Indexes1-l,Indexes2+24-l] # First column: hour 24, second column: hour 23,..., 24-th column: hour 
  X[,l+24] = c(Xtmp)
}


Y = matrix(NA, nrow = length(Indexes1)*length(Indexes2), ncol = 24)
for (Lag in 24:1){
  Ytmp <- D[Indexes1+24-Lag,Indexes2+24-Lag] 
  Y[,Lag] = c(Ytmp)
}

# Keep only the distances when Indexes2<Indexes1
X = X[Indexes,]
Y = Y[Indexes,]

Y_mean = apply(Y, 1, mean)
Y_mean = c(Y_mean)
O = order(Y_mean) # indexes to order the Y vector

set.seed(1234)
II = sample(O[1:(0.99*length(Y_mean))], 20000) # select 20000 indexes of the Y vector for computationally reasons. 

X = X[II,]
Y = Y[II,]


# Correlation matrix 
cor_L2 = cor(X,Y, method = "spearman")
corrplot(cor_L2, tl.cex = 1, cl.cex = 1, cl.align.text = 'l', col = COL1('YlGn', 200))


#-----------------------
#Pseudo-Frechet distance
#-----------------------

load("D_19_21_godown_pf.RData")

X = matrix(NA, nrow = length(Indexes1)*length(Indexes2), ncol = 24*2)

for (l in 1:24){
  Xtmp <- D[Indexes1-l,Indexes2-l] 
  
  X[,l] = c(Xtmp)
}

for (l in 1:24){
  Xtmp <- D[Indexes1-l,Indexes2+24-l] # First column: hour 24, second column: hour 23,..., 24-th column: hour 
  X[,l+24] = c(Xtmp)
}

Y = matrix(NA, nrow = length(Indexes1)*length(Indexes2), ncol = 24)
for (Lag in 1:24){
  Ytmp <- D[Indexes1+24-Lag,Indexes2+24-Lag] 
  Y[,Lag] = c(Ytmp)
}

# Keep only the distances when Indexes2<Indexes1
X = X[Indexes,]
Y = Y[Indexes,]

Y_mean = apply(Y, 1, mean)
Y_mean = c(Y_mean)
O = order(Y_mean) # indexes to order the Y vector

set.seed(1234)
II = sample(O[1:(0.99*length(Y_mean))], 20000) # select 20000 indexes of the Y vector for computationally reasons. 0.99 para que valores atípicos no estropeen la prediccion porque quieres predecicr las distancias de abajo

X = X[II,]
Y = Y[II,]

cor_pf = cor(X,Y, method = "spearman")
corrplot(cor_pf, tl.cex = 1, cl.cex = 1, cl.align.text = 'l', col = COL1('YlGn', 200))


#--------------------------
# Pseudo-L2 distance
#--------------------------

load("D_19_21_godown_pL2.RData")

X = matrix(NA, nrow = length(Indexes1)*length(Indexes2), ncol = 24*2)

for (l in 1:24){
  Xtmp <- D[Indexes1-l,Indexes2-l] 
  
  X[,l] = c(Xtmp)
}

for (l in 1:24){
  Xtmp <- D[Indexes1-l,Indexes2+24-l] 
  X[,l+24] = c(Xtmp)
}

Y = matrix(NA, nrow = length(Indexes1)*length(Indexes2), ncol = 24)
for (Lag in 1:24){
  Ytmp <- D[Indexes1+24-Lag,Indexes2+24-Lag] 
  Y[,Lag] = c(Ytmp)
}

# Keep only the distances when Indexes2<Indexes1
X = X[Indexes,]
Y = Y[Indexes,]

Y_mean = apply(Y, 1, mean)
Y_mean = c(Y_mean)
O = order(Y_mean) # indexes to order the Y vector

set.seed(1234)
II = sample(O[1:(0.99*length(Y_mean))], 20000) # select 20000 indexes of the Y vector for computationally reasons. 0.99 para que valores atípicos no estropeen la prediccion porque quieres predecicr las distancias de abajo

X = X[II,]
Y = Y[II,]



cor_pL2 = cor(X,Y, method = "spearman")
corrplot(cor_pL2, tl.cex = 1, cl.cex = 1, cl.align.text = 'l', col = COL1('YlGn', 200))