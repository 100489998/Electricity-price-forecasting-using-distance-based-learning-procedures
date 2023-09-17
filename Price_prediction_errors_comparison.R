#########################################
# Comparison of price prediction errors
#########################################

library(ggplot2)
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2)

# Load all the price prediction errors
load("rmse_goup_L2.RData")
load("rmse_goup_pL2.RData")
load("rmse_goup_pf.RData")
load("rmse_godown_L2.RData")
load("rmse_godown_pL2.RData")
load("rmse_godown_pf.RData")

load("mae_goup_L2.RData")
load("mae_goup_pL2.RData")
load("mae_goup_pf.RData")
load("mae_godown_L2.RData")
load("mae_godown_pL2.RData")
load("mae_godown_pf.RData")



# Plot errors of the upward secondary market

dists = c("L²", "Pseudo-Fréchet", "Pseudo-L²")
error_type = c("RMSE", "MAE")
methods = c("1-NN (max)", "1-NN (sum)", "RF+1-NN", "RF(h)+1-NN")

data = expand.grid(Method = methods, Distance = dists, ErrorType = error_type)
data$Error = c(rmse_goup_L2, rmse_goup_pf, rmse_goup_pL2, mae_goup_L2, mae_goup_pf, mae_goup_pL2)
colors = c("blue", "green", "red", "black")

p = ggplot(data, aes(x = Method, y = Error, color = Method)) +
  geom_point(size = 4) +
  facet_grid(ErrorType ~ Distance, scales = "free_y") +
  labs(x = NULL, y = "Error") +
  scale_color_manual(values = colors) +  
  theme_bw() +
  theme(panel.grid = element_blank(),          
        strip.text = element_text(size = 18),   
        axis.title = element_text(size = 18),  
        axis.text = element_text(size = 18),  
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(color = "black")) 


print(p)

(rmse_goup_L2 < rmse_goup_pf)
(rmse_goup_L2 < rmse_goup_pL2)
(mae_goup_L2 < mae_goup_pf)
(mae_goup_L2 < mae_goup_pL2)

# Plot errors of the downward secondary market 

dists = c("L²", "Pseudo-Fréchet", "Pseudo-L²")
error_type = c("RMSE", "MAE")
methods = c("1-NN (max)", "1-NN (sum)", "RF+1-NN", "RF(h)+1-NN")

data = expand.grid(Method = methods, Distance = dists, ErrorType = error_type)
data$Error = c(rmse_godown_L2, rmse_godown_pf, rmse_godown_pL2, mae_godown_L2, mae_godown_pf, mae_godown_pL2)
colors = c("blue", "green", "red", "black")


p = ggplot(data, aes(x = Method, y = Error, color = Method)) +
  geom_point(size = 4) +
  facet_grid(ErrorType ~ Distance, scales = "free_y") +
  labs(x = NULL, y = "Error") +
  scale_color_manual(values = colors) +  
  theme_bw() +
  theme(panel.grid = element_blank(),          
        strip.text = element_text(size = 18),   
        axis.title = element_text(size = 18),  
        axis.text = element_text(size = 18),  
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(color = "black")) 


print(p)

(rmse_godown_L2 < rmse_godown_pf)
(rmse_godown_L2 < rmse_godown_pL2)
(mae_godown_L2 < mae_godown_pf)
(mae_godown_L2 < mae_godown_pL2)

