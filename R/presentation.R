#======================================#
#       Couldabeen Density Plots       #
#======================================#
# Set colors
col1 <- "salmon"   # Retirees
col2 <- "skyblue3" # Rookies
col3 <- "violet"
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

#================================================
#         Color Settings for Linear Models 
#================================================

col <- "mediumvioletred"
col1 <- "salmon"
col2 <- "forestgreen"

#================================================
#         Coefficients for Linear Models 
#================================================

# Obtain linear model for all years
coefs_comp <- lm(formula = prop ~ I(Year), data = couldabeens_t)$coefficients
coefs_post <- lm(formula = prop ~ I(Year), data = couldabeens_post)$coefficients
coefs_pre <- lm(formula = prop ~ I(Year), data = couldabeens_pre)$coefficients

#===========================================
#       Proportion-Year Linear Model
#===========================================

# Plot proportion of couldabeens in post-rule era ()
plot_lm1 <- scatter_props(couldabeens_t) +
  geom_abline(slope = coefs_comp[2], intercept = coefs_comp[1], color = col) + 
  geom_point(data = couldabeens_pre, mapping = aes(x = Year, y = prop), color = col) +
  geom_point(data = couldabeens_post, mapping = aes(x = Year, y = prop),color = col)

#=============================================
#          Year Linear Model (Pre)
#=============================================

plot_lm2 <- ggplot(data = couldabeens_pre, mapping = aes(y = prop, x = Year)) + 
  geom_point(color = col1) +
  geom_smooth(color = col1, se = F, method = "lm")


#=============================================
#          Year Linear Model (Post)
#=============================================

plot_lm3 <- ggplot(data = couldabeens_post, mapping = aes(y = prop, x = Year)) + 
  geom_point(color = col2) +
  geom_smooth(color = col2, se = F, method = "lm")

#=============================================
#       Labor Share Linear Model (Post)
#=============================================

plot_lm4 <- ggplot(data = couldabeens_post, mapping = aes(y = prop, x = labShare)) + 
  geom_point(color = col1) +
  geom_smooth(color = col1, se = F, method = "lm")

#=====================================
#       Simpson's Paradox Plot
#=====================================

# Plot proportion of couldabeens in post-rule era ()
plot_simpsons <- scatter_props(couldabeens, title = "") +
  geom_abline(slope = coefs_comp[2], intercept = coefs_comp[1], color = col) + 
  geom_point(data = couldabeens_pre, mapping = aes(x = Year, y = prop), color = col1) +
  geom_abline(slope = coefs_pre[2], intercept = coefs_pre[1], color = col1) +
  geom_point(data = couldabeens_post, mapping = aes(x = Year, y = prop),color = col2) +
  geom_abline(slope = coefs_post[2], intercept = coefs_post[1], color = col2) + 
  labs(y = "Proportion")

#=====================================
#       Labor Share Year Plot
#=====================================

scatter_labShare <- ggplot(data = payroll, mapping = aes(x = Year, y = labShare)) + 
  geom_point(color = col3) +
  geom_smooth(se = F, method = "lm", color = col3)

#=======================================
#       Labor Share Proportion Plot
#=======================================

col2 <- "skyblue3"
plot_labShare <- ggplot(data = couldabeens_post, mapping = aes(y = prop, x = labShare)) + 
  geom_point(color = col2) +
  geom_smooth(color = col2, se = F, method = "lm")
