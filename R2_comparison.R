###############################
# Comparison of the R^2 values
###############################

# Load R^2 values

# Upward secondary market
load("R2DA_L2_goup.RData")
R2DA_L2_goup = R2DA
load("R2DA_pf_goup.RData")
R2DA_pf_goup = R2DA
load("R2DA_pL2_goup.RData")
R2DA_pL2_goup = R2DA

load("R2_L2_goup.RData")
R2_L2_goup = R2
load("R2_pf_goup.RData")
R2_pf_goup = R2
load("R2_pL2_goup.RData")
R2_pL2_goup = R2

# Downward secondary market 
load("R2DA_L2_godown.RData")
R2DA_L2_godown = R2DA
load("R2DA_pf_godown.RData")
R2DA_pf_godown = R2DA
load("R2DA_pL2_godown.RData")
R2DA_pL2_godown = R2DA

load("R2_L2_godown.RData")
R2_L2_godown = R2
load("R2_pf_godown.RData")
R2_pf_godown = R2
load("R2_pL2_godown.RData")
R2_pL2_godown = R2

library(ggplot2)

# RF(h) + 1-NN method

dists = c("L²", "Pseudo-Fréchet", "Pseudo-L²")
market = c("Upward", "Downward")
methods = seq(1,24)

data = expand.grid(Method = methods, Distance = dists, Market = market)
data$R2 = c(rev(R2_L2_goup), rev(R2_pf_goup), rev(R2_pL2_goup), rev(R2_L2_godown), rev(R2_pf_godown), rev(R2_pL2_godown))
colors = c("black", "red", "blue")

ggplot(data, aes(x = Method, y = R2, color = Distance)) +
  geom_point(size = 4) +
  facet_grid(Market ~ ., scales = "free_y") +
  labs(x = "Hour", y = "R²") +
  scale_color_manual(values = colors) +  # Asignar colores a los métodos
  theme_bw() +
  scale_x_continuous(breaks = methods, labels = methods) +  
  theme(panel.grid = element_blank(),          
        strip.text = element_text(size = 18),   
        axis.title = element_text(size = 18),  
        axis.text = element_text(size = 18),  
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black")) 


# RF + 1-NN method 

df_2 = data.frame(
  Market = rep(c("Upward", "Downward"), each = 3),
  Distance = rep(c("L²", "Pseudo-Fréchet", "Pseudo-L²")),
  R2 = c(R2DA_L2_goup, R2DA_pf_goup, R2DA_pL2_godown, R2DA_L2_godown, R2DA_pf_godown, R2DA_pL2_godown)
)
min_2 = min(R2DA_L2_goup, R2DA_L2_godown, R2DA_pf_goup, R2DA_pf_godown, R2DA_pL2_goup, R2DA_pL2_godown)
max_2 =  max(R2DA_L2_goup, R2DA_L2_godown, R2DA_pf_goup, R2DA_pf_godown, R2DA_pL2_goup, R2DA_pL2_godown)

ggplot(df_2, mapping = aes(x = Market, y = R2, color = Distance)) +
  geom_point(size = 4) +
  labs(x = "Market", y = "R²") +
  scale_color_manual(values = colors) + 
  theme_bw() +
  theme(panel.grid = element_blank(),          
        strip.text = element_text(size = 18),   
        axis.title = element_text(size = 18),  
        axis.text = element_text(size = 18),  
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"))


