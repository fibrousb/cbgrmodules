### --------------------------------------------------------------------- ###

            ### Model evaluation, estimation, and uncertainty ###
           ### useful tools and workflows with the tidyverse ###

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



                #### Evaluating models with summary metrics ####

## After fitting a model to data, we may want to evaluate the fit
## of the model. There are many ways to go about model evaluation, including many
## error or precision metrics and various visual tools. We will briefly go over some 
## evaluation methods using simple linear regression models as examples.

## We will start with the classic mtcars dataset built into R.

head(mtcars) # the data frame
pairs(select(mtcars,mpg,disp,hp,drat,wt,qsec)) # pairs plot of a few variables

## There's a strong visual relationship between weight (wt) and fuel efficiency (mpg)
## so we'll build a simple model of mpg by wt

mod1 <- lm(mpg ~ wt, data = mtcars)

## If we look at the model summary, we see that R already gives us a few ways to
## evaluate the model's fit

summary(mod1)

## The model summary provides us with a MULTIPLE R2 and an ADJUSTED R2.
## The former is a measure of the amount of variations in the response variable
## that is "explained" by the predictor variable(s). The metric is easily interpreted
## as it is on a scale of 0 (no variance explained by the predictor(s)) to 1 (all variance
## explained). One major issue with the R2 metric is that as you add more predictor variables, 
# the R2 will always increase, potentially leading the analyst to assume adding more variables
## will always improve the model. However, the adjusted R2 incorporates penalities
## as the number of predictor variables increases. This can defend against overfitting
## models.

## If we add additional predictor variables to the model, the model will explain more of
## the variation in the response but the discrepancy between the multiple R2 and 
## adjusted R2 will grow.

mod2 <- lm(mpg ~ wt+vs+cyl+hp, data = mtcars)
summary(mod2)

## Another evaluation metrics is Root Mean Square Error or RMSE. RMSE is essentially the
## square root of the residual variance,. This metric takes the residuals,
## or how far the observed data is form the model estimates (in this case, the regrssion line)
## and measures their spread. RMSE tells you how well the observed data clusters about our
## model estimates. Moreover, RMSE gives extra weight to very large errors (residuals far from
## the estimate). While R2 provides a relative measure of model fit, RMSE is an absolute measure
## of the model fit. Lower values indicate a better fit and RMSE can be thought of as
## "the standard deviation of the unexplained variance" (theanalysisfactor.com) and thus,
## is in the same units as the response variable.

## RMSE can be calculated easily with the modelr package

rmse(mod1, data = mtcars)

## The mean absolute error (MAE) is similar to RMSE, however, is weights all errors equally.

mae(mod1, data = mtcars)


## We can compare all these metrics with a model of mpg that has a poorer fit, for example
## a model of mpg with qsec as the predictor variable

mod3 <- lm(mpg ~ qsec, data = mtcars)
summary(mod3)

# Creat table of model evaluations
eval_table <- as_tibble(rbind(c("qsec", summary(mod3)$r.squared, summary(mod3)$adj.r.squared,
                                rmse(mod3,mtcars),mae(mod3,mtcars)),
                              c("wt",summary(mod1)$r.squared,summary(mod1)$adj.r.squared,
                                rmse(mod1,mtcars),mae(mod1,mtcars))))
# Rename columns
colnames(eval_table) <- c("predictor","R2","adjR2","RMSE","MAE")

eval_table

## Looking at the evaluation table, it's clear that the fit is much better in the wt model.
## Both R2 values are much higher and both RMSE and MAE values are much lower in the wt model.

                  
          
                    #### Evaluating models visually ####


## The metrics above tell us a lot about the explained variance and the quality of the fit, but 
## to really get a feel for the model performance, we can visualize the model fit.

## To start, we can make a simple plot of the raw data.

ggplot(mtcars,aes(wt,mpg))+
  geom_point(size = 3, alpha = 0.75)+
  theme_bw()

## We can then plot our model estimate. There are many ways of doing this, here I'll use
## augment() from the broom package.

newdat <- tibble(wt = seq(min(mtcars$wt),max(mtcars$wt),0.001)) # new dataframe to predict response

augment(mod1,newdata = newdat) %>%  # estimates based on model
  ggplot(aes(wt,.fitted))+ # aes for fit line
  geom_point(data = mtcars,aes(wt,mpg), # seperate data and aes for raw data
             size = 3, alpha = 0.75)+ 
  ylab("mpg")+
  geom_line()+ # geom for model fit line
  theme_bw()

## Now we can see how far the observed data is from our fitted model regression.
## We can further visualize spread of the residuals by connecting them to our fit line.

mtcars$mod1_pred <- augment(mod1)$.fitted # attach predictions to dataframe

augment(mod1,newdata = newdat) %>%
  ggplot(aes(wt,.fitted))+
  geom_segment(data = mtcars, aes(x = wt, xend = wt, # geom for line segments
                                  y = mpg, yend = mod1_pred))+
  geom_point(data = mtcars,aes(wt,mpg), 
             size = 3, alpha = 0.75)+ 
  ylab("mpg")+
  geom_line()+ 
  theme_bw()

## We can also remove the points
augment(mod1,newdata = newdat) %>%
  ggplot(aes(wt,.fitted))+
  geom_segment(data = mtcars, aes(x = wt, xend = wt, # geom for line segments
                                  y = mpg, yend = mod1_pred))+
  geom_line()+ 
  ylab("mpg")+
  theme_bw()

## Now compare this fit to the qsec model

mtcars$mod3_pred <- augment(mod3)$.fitted # attach predictions to dataframe

newdat2 <- tibble(qsec = seq(min(mtcars$qsec),max(mtcars$qsec),0.001))

augment(mod3,newdata = newdat2) %>%
  ggplot(aes(qsec,.fitted))+
  geom_segment(data = mtcars, aes(x = qsec, xend = qsec, # geom for line segments
                                  y = mpg, yend = mod3_pred))+
  geom_point(data = mtcars,aes(qsec,mpg), 
             size = 3, alpha = 0.75)+ 
  ylab("mpg")+
  geom_line()+ 
  theme_bw()

## Clearly, a poorer fit! This also sends up red flags about homegeneity of the variance. 
## the residual variance appears to be larger in center of the qsec range than at the ends.


## To wrap up, let's go back to original model with wt. Becuase there is a single continuous
## predictor, evaluating the effect of the predictor on the response is straightforward. The 
## summary output for the model will give us the same result as a one-way ANOVA or an F-test
## or a LRT against the null... it's all the same thing!!

summary(mod1)
anova(mod1)
null <- lm(mpg ~1, mtcars)
anova(null,mod1)

## So, our predictor is significant below 0.05. From the summary we see the slope is
## estimated at -5.3, indicating a considerble negative effect of weight on fuel efficiency.

## We can also get the confidence interval around the intercept and slope estimates
ci <- confint(mod1,c("wt","(Intercept)"));ci


#### Model comparison and visual evaluation of fit with different types of models ####

## Let's move beyond a one term linear regression. In the next model, we will look at 
## a classic two-way ANOVA set up with two categorical predictor variables and a continuous
## response. Here we will use the ToothGrowth dataset which tracks tooth length in guinea
## pigs fed two different supplements at different doesages. Dose could potentiaklly be
## coded as categorical or continuous, but we will stick with categorical for this example.

tooth_data <- ToothGrowth # rename dataframe
tooth_data$dose <- factor(tooth_data$dose) # factor dose to make categorical

## Quickly plot the raw data
ggplot(tooth_data,aes(dose,len))+
  geom_point(alpha = 0.75)+
  facet_grid(~supp)+
  theme_bw()

## Now we will model the repsonse, length, by supp, dose, and their interaction
tooth_model <- lm(len ~ supp*dose, tooth_data)
summary(tooth_model)

## We can test this against a simpler model with a likelihood ratio test (LRT)
tooth_model2 <- update(tooth_model, ~.-supp:dose)
anova(tooth_model2,tooth_model) # LRT suggests keeping the interaction

## Another test of fit is the overall F test against the null. The summary function
## will give us this test by default at the bottom of the output

summary(tooth_model) # the summary tells us that the model is a better fit than the null model

## we can extract the test statistic and d.f with this call (in case this needs to reported)
summary(tooth_model)$fstatistic

## We also get the usual R2 values, and we can calculate RMSE and MAE as above
rmse(tooth_model,tooth_data)
mae(tooth_model,tooth_data)

## Now lets visualize the model fit. This is a bit different than the linear regression
## as we only need to get model estimates for each level of the predictor instead of a range
## of values like we saw above. 

## Our new data frame to predict over should contain all unique combination of factor
## levels in our predictor variables. The easiest way to accomplosh this is with the 
## expand_grid function

newdat_tooth <- expand_grid(supp = unique(tooth_data$supp), dose = unique(tooth_data$dose))

## Now we can use augment as before to get our estimates. This time, however, we will also
## generate a meaure of uncertainty to accompany our estimate. We'll use the standard error.

tooth_model %>% 
  augment(newdata = newdat_tooth)

## Now let's plot the estimates and uncertainty

tooth_model %>% 
  augment(newdata = newdat_tooth) %>% 
  ggplot(aes(dose,.fitted))+
  geom_point(size = 3)+
  #below we set the upper and lower bounds of the errorbars with the estimate and the se
  geom_errorbar(aes(ymin = .fitted - .se.fit, ymax = .fitted + .se.fit),
                width = 0.1)+
  facet_grid(~supp)+
  theme_bw()

## Great, now we can add back in the raw data to check the fit

tooth_model %>% 
  augment(newdata = newdat_tooth,) %>% 
  ggplot(aes(dose,.fitted))+
  geom_point(data = tooth_data, aes(dose, len), alpha = 0.75,
             color = "cyan4")+
  geom_point(size = 3, alpha = 0.75)+
  geom_errorbar(aes(ymin = .fitted - .se.fit, ymax = .fitted + .se.fit),
                width = 0.1)+
  facet_grid(~supp)+
  theme_bw()

## The fit looks pretty good, and the relationship tracks. We can get a more conservative 
## idea of our uncertainty with 95% confidence intervals. You can get approx. 95% CI
## by multiplying the SE by 1.96

tooth_model %>% 
  augment(newdata = newdat_tooth,) %>% 
  ggplot(aes(dose,.fitted))+
  geom_point(data = tooth_data, aes(dose, len), alpha = 0.75,
             color = "cyan4")+
  geom_point(size = 3, alpha = 0.75)+
  geom_errorbar(aes(ymin = .fitted - 1.96*.se.fit, ymax = .fitted + 1.96*.se.fit),
                width = 0.1)+
  facet_grid(~supp)+
  theme_bw()

## We can make a few little tweaks to make the figure better:

supp.labs <- c("Orange juice", "Absorbic acid")
names(supp.labs) <- c("OJ", "VC")

tooth_model %>% 
  augment(newdata = newdat_tooth,) %>% 
  ggplot(aes(dose,.fitted))+
  geom_jitter(data = tooth_data, aes(dose, len), alpha = 0.3,
             color = "cyan4",width = 0.1)+
  geom_point(size = 3, alpha = 0.75)+
  geom_errorbar(aes(ymin = .fitted - 1.96*.se.fit, ymax = .fitted + 1.96*.se.fit),
                width = 0.1)+
  scale_y_continuous(name = "Tooth length (mm)", limits = c(0,35))+
  xlab("Dose (mg/day)")+
  facet_grid(~supp,labeller = labeller(supp = supp.labs))+
  theme_few()

## Now that we have evaluated the fit of our model in a number of different ways, we should
## evaluate the influence of the individual predictors. When there is an interaction present
## Type III ANOVA is often the most appropriate choice.

Anova(tooth_model, type = 3)

## The ANOVA output suggests that all of our predictor variables are 
## statistically significant below 0.05. This coupled with the visual and qunatitative
## evidence above tells us our model does a good job of describing the data. Now it's up
## to you as the analyst to interpret these results!


## Finally, after all the above - and to provide context and evidence for your interpretations
## of the data - you may want to report estimated marginal means. These are the mean values
## as estimated by the model. This is often most useful after running an ANOVA where the 
## indepenant variable(s) are categorical. This allows you to directly compare estimated 
## means of the groups you are likely investiagting differences among.

## We can use the emmeans package for this. Provide the function with the model and the 
## predictors to estimate over.

emmeans(tooth_model,specs = c("dose","supp"))

## The output will provide means, st. errors, and confidence levels. You can also use the
## resulting data frame for plotting etc.


