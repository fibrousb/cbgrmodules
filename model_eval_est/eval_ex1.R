#### Model Evaluation Excericse 1 ####
                         
library(tidyverse)
library(ggthemes)
library(broom)
library(car)
library(modelr)
## Below, we import a simulated dataset of a plant competition experiment.

plant_comp <- read.csv("model_eval_est/plant_comp.csv")
head(plant_comp)                  

## There are three columns: comp = the height of the competitor plant, height = the height of
## the focal plant, and group = the treatment group where "A" is a nutrient poor treatment
## and "B" is a nutrient rich treatment.

## Build and evaluate a model to test the following hypotheses: 
  ##    - There is a negative relationship between competitor height and focal plant height.
  ##    - The plants in group A and group B differ in their overall height.
  ##    - The relationship between competitor height and focal plant height is the same between
  ##      nutrient treatments.

## Evaluate the predictors and the overall model. Provide graphical and statistical evidence
## to support your findings, including model estimates and uncertainty. Wherever possible,
## plot both raw data and model estimates.


## Explore data 

plant_comp %>% 
  group_by(group) %>% 
  summarize(mean_height = mean(height),
            sd_height = sd(height))

ggplot(plant_comp,aes(group,height))+
  geom_jitter(width = 0.1)

ggplot(plant_comp,aes(height, fill = group))+
  geom_density(alpha = 0.5)+
  theme_bw()

ggplot(plant_comp,aes(comp,height, color = group))+
  geom_point(alpha = 0.9)+
  theme_bw()

## Build model
mod1 <- lm(height ~ comp*group, plant_comp)

## Model summary
summary(mod1)

## Evaluate predictors w/ type-3 ANOVA
Anova(mod1, type = 3)

## Model selection - option
mod2 <- update(mod1, ~. -comp:group)
summary(mod2)
anova(mod2,mod1)

## Assess fit
rmse(mod1,plant_comp)
mae(mod1,plant_comp)
rmse(mod2,plant_comp)
mae(mod2,plant_comp)

## Diagnostics
plot(mod1)

## Group means across comp heights

emmeans(mod1, spec = "group")

## Generate model predictions and plot

mod1 %>% 
  augment(newdata = expand_grid(group = unique(plant_comp$group),
                                comp = seq(from = min(plant_comp$comp),
                                          to = max(plant_comp$comp),
                                          by = 0.1))) %>%
  ggplot(aes(comp,.fitted, color = group))+
  geom_point(data = plant_comp, aes(comp, height, color = group),
             alpha = 0.65, size = 2, inherit.aes = F)+
  geom_line()+
  geom_ribbon(aes(ymin = .fitted - .se.fit, ymax = .fitted + .se.fit,
                  fill = group, color = NULL), alpha = 0.25)+
  ylab("Focal plant height (cm)")+
  xlab("Competitor plant height (cm)")+
  scale_color_discrete(name = "Treatment", labels = c("Nutrient poor", "Nutrient rich"))+
  scale_fill_discrete(name = "Treatment", labels = c("Nutrient poor", "Nutrient rich"))+
  theme_bw()


## Model coefficients and CI
summary(mod1)

## Get the estimate abnd SE of the slope in group B using the variance-covariance 
## matrix of the model coefficients. 
## The SE is the square root of Var(a+b) where Var(a+b) = Var(a) + Var(b) + 2*cov(a,b)
slope2 <- coef(mod1)["comp"]+coef(mod1)["comp:groupB"] 
se_slope2<-sqrt(vcov(mod1)["comp","comp"]+
           vcov(mod1)["comp:groupB","comp:groupB"]+
           2*vcov(mod1)["comp","comp:groupB"])
slope2
se_slope2

#  2-sided t-test... to test is slope is different from 0
pt(slope2/se_slope2, df = nrow(plant_comp)-length(coef(mod1)))*2

## Stats:
    ## Overall model F-test:  F = 114.8, d.f = 3, 96, p < 0.001

    ## ANOVA: Comp - F = 71.38 , p <  0.001
    ##        Group - F = 8.27, p = 0.005  
    ##        Comp:Group - F = 15.79, p < 0.001

    ## Estimated group means across competitor heights: A = 71.1 cm, B = 80.6 cm
    ## Competitor height slope estimates: A = -0.47 (+/- 0.06), B = -0.16 (+/- 0.05, p = 0.003)
  






