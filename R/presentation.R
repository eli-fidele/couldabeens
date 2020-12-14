#======================================#
#       Couldabeen Density Plots       #
#======================================#
# Set colors
col1 <- "salmon"   # Retirees
col2 <- "skyblue3" # Rookies
# Set color parameters
colors_density <- c("Retirees" = col1, "Rookies" = col2)
# Density plot of rookie and retired pitchers 
dens_pit <- ggplot() +
  geom_density(data = pit_ret, aes(x = WAR, color = "Retirees")) +
  geom_density(data = pit_rkes, aes(x = WAR, color = "Rookies")) +
  geom_vline(xintercept = mean(pit_rkes$WAR), color = col2) +
  labs(title = "Pitchers") +
  theme(legend.position = 'right') +
  scale_color_manual("Player",values = colors_density) 
# Density plot of rookie position players and retired position players 
dens_pos <- ggplot() +
  geom_density(data = pos_ret, aes(x = WAR, color = "Retirees")) +
  geom_density(data = pos_rkes, aes(x = WAR, color = "Rookies")) +
  geom_vline(xintercept = mean(pos_rkes$WAR), color = col2) +
  labs(title = "Position") +
  theme(legend.position = 'right') +
  scale_color_manual("Player",values = colors_density) 