

### --------------------------------------------------------------------- ###

### GLM (poisson regression) ####

## Jacob Zeldin, Chicago Botanic Garden ##
## Winter/Spring 2020 ##

### --------------------------------------------------------------------- ###

### You will need the following packages to follow along with the module.
### Use install.packages() with the name of the package in quotes to install.
### ex. install.packages("tidyverse")

library(tidyverse)
library(modelr)
library(broom)
library(ggthemes)
library(car)
library(emmeans)
library(ggpubr)
# You may have data that doesn't conform to the assumptions of a linear regression
# For example, you may be taking 'count' data (ex. leaf numbers, seed germination etc.)
# For this type of data, you may want to build a generlized linear model (GLM) using
# a posisson distribution (as opposed to a normal or binomial dist).

# This type of analysis can get a bit more complicated as we have to check for
# things like over-dispersion... more on that later

# To illustrate this type of analysis, we can use the warpbreaks data built into R

warpbreaks

# This gives us data with two independant (predictor) variables: Wool type and Tension.
# and one dependant variable (response); breaks

# First let's plot our variables a few different ways

ggplot(warpbreaks, aes(wool,breaks)) +
  geom_boxplot()
ggplot(warpbreaks, aes(tension,breaks)) +
  geom_boxplot()
ggplot(warpbreaks, aes(tension,breaks, color = wool)) +
  geom_boxplot()

# looks like there could be an effect of tension and possibly an interaction
# with wool type.

# The setup for a GLM is similar to a linear regression but with a few key differences. We use
# the function glm() and now we need to include the distribution type. Here, we use the poisson
# distrubtion because we are working with count data.

g_mod <- glm(breaks ~ wool + tension + wool:tension, data = warpbreaks, 
             family = poisson)

# Now, maybe we may want to go through model simplification to find the most parsimonious
# model. To do this, we build our simplified model without an interaction for comparison

g_mod2 <- glm(breaks ~ wool + tension, data = warpbreaks, 
              family = poisson)

# To compare GLMs, we use the anova function and specify the test as a Chi square test.

anova(g_mod2,g_mod,test = "Chi")

# The low p-value indicates we should keep the interaction term

# let's look at our model summary
summary(g_mod)


# Here we see that the residual deviance is much higher than our degrees of freedom
# this indiciates over-dispersion in our data. That's not good for our model
# assumptions. Basically it means that there is a large portion of variation in our
# data that is not accounted for in our model. 

# one way to address this problem is to use a quasi-poisson distribution and see if
# that improves our situation. This estimates the disperion parameter beyond 1 which helps
# to account for the extra variance

g_mod_q <- glm(breaks ~ wool + tension + wool:tension, data = warpbreaks, 
               family = quasipoisson)

# We will go through and check if a simplified model is better

g_mod2_q <- glm(breaks ~ wool + tension, data = warpbreaks, 
                family = quasipoisson)

anova(g_mod2_q,g_mod_q, test = "Chi") # we should still keep our interaction


summary(g_mod_q)

# Now if we compare this output to our orignal model, we see that the dispersion parameter
# has been increased to 3.76. We see that the estimates are the
# same, but the std errors are much larger. This accounts for our over-dispersion by
# increasing our uncertainty.

# We can demonstrate this by predicting the results of both models and plotting them

preds1 <- data.frame(unique(warpbreaks[,c("wool","tension")]))
p <- predict(object = g_mod,
             newdata = preds1,type = "response",
             se.fit = T)
preds1$fit <- p$fit
preds1$se <- p$se.fit

preds2 <- data.frame(unique(warpbreaks[,c("wool","tension")]))
p2 <- predict(object = g_mod_q,
              newdata = preds2,type = "response",
              se.fit = T)
preds2$fit <- p2$fit
preds2$se <- p2$se.fit

mod1 <- ggplot(preds1,aes(tension,fit,color = wool))+
  geom_point()+
  geom_errorbar(aes(ymin = fit - se, ymax = fit + se),width = 0.25)+
  ylab("Breaks")+
  xlab("Tension")+
  theme_bw()

mod1

mod2 <- ggplot(preds2,aes(tension,fit,color = wool))+
  geom_point()+
  geom_errorbar(aes(ymin = fit - se, ymax = fit + se),width = 0.25)+
  ylab("Breaks")+
  xlab("Tension")+
  theme_bw()

mod2

# We can use ggarrange() to arrange the plots side by side
ggarrange(mod1+ggtitle("Poisson"),mod2+ggtitle("Quasipoisson"),
          common.legend = T,legend = "bottom")

# We see that our quasi-poisson model has larger error bars. This is important
# because it changes our interpretation of the data. In the first (over-dispersed) model, we might conclude that wool types have different
# breaks (responses) in all tension categories....

# However, in our more conservative second model, we would conclude that this is only
# the case in the low tension category. In the medium and high categories, the error bars
# overlap, meaning we dont have the evidence to say that wool types differ in those
# categories!


