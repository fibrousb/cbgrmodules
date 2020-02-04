### --------------------------------------------------------------------- ###

            ### Tools for data wrangling and manipulation ###
                ## Jacob Zeldin, Chicago Botanic Garden ##
                        ## Winter/Spring 2020 ##
### --------------------------------------------------------------------- ###

library(tidyverse)
library(modelr)
library(broom)
library(purrr)



                        #### Homebrew Functions ####

## Homebrew functions are an important tool for data manipulation, summary statistics,
## calculations, and endless other tasks in R. If you find yourself writing the same few lines
## of code to accomnpliosh the same task over and over again with minor tweaks, chances are
## you can write a function to do that task. This saves a lot of time and can help minimize
## errors.

## Base R and the essentital packages in the tidyverse have tons of built-in functions that
## are often enough to take care of the common task you'll be performing in R.

## For example, you've almost certainly used the mean() function to find the mean of a vector
## or other data object.

mean(mtcars$mpg)

## Or sd() to find the standard deviation

sd(mtcars$mpg)

## An example of a slightly complex function is a linear model

lm(mpg ~ hp, mtcars)


## But what if base R or your installed packages don't have a built-in function for a 
## task you'd like to perform? Just build your own!

## For example, base R does not have a standard error function. We can easily build one.

st.error <- function(x) {
  se <- sd(x, na.rm = T)/sqrt(length(x[!is.na(x)]))
  print(se)
}

st.error(mtcars$mpg)

## The stanard method for composing a function is to supply function() with one or more
## arguments (here we supply it with "x"). Then, inside curly brackets we tell the function
## what actions to perform, generalizing the instructions to apply to the argument. So here
## we tell the function to find the st. deviation of "x", removing all NAs, and divide that
## value by the square root of the number of observations (here, the length of "x" after
## removing any NAs). Finally, we included the print() line so that the value would be 
## printed in the terminal inb addition to being stored in an object should we choose to
## assign an object.
