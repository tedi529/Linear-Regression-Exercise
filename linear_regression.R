# Teodora Latinska
# Linear Regression Exercise
# Foundations of Data Science

library(dplyr)
library(tidyr)
library(ggplot2)

# Set WD 
setwd("C:/Users/Tedi/Desktop/Data Science/Linear Regression")

#Download data from zipped file
states <- readRDS("states.rds")

# Examine structure of data and clean data appropriately
summary(str)
str(states)
is.na(states)

#Plot data
ggplot(states, aes(x = metro, y = energy)) + geom_point()

#fit regression model to data predicting energy exp from metro pop
energy_mod <- lm(energy ~ metro, data = states)

#summarize and print results
summary(energy_mod)

#plot model
plot(energy_mod)
par(ask=F)

#selecting income and waste as additional predictor of energy usage 
energy_mod2 <- lm(energy ~ metro + income + waste + green + toxic
                  , data = states)
summary(energy_mod2)

#income and waste not statistically significant but green and toxic
#very statistically significant

#remove income and waste from model
energy_mod3 <- lm(energy ~ metro + green + toxic
                  , data = states)
summary(energy_mod3)
plot(energy_mod3)
par(ask=F)

#adding green and toxic renders metro not statistically significant

#add interaction of density to model
energy_mod4 <- lm(energy ~ metro*density + green*density + toxic*density, data = states)
summary(energy_mod4)
coef(summary(energy_mod4))
plot(energy_mod4)
par(ask=F)

#add region to the model
#let R know region is categorical
states$region <- factor(states$region)
energy_mod5 <- lm(energy ~ metro + green + toxic + region, data = states)
coef(summary(energy_mod5))
plot(energy_mod5)
par(ask=F)

#there do seem to be significant differences between the regions

