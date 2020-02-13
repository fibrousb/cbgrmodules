### --------------------------------------------------------------------- ###

            ### Tools for data wrangling and manipulation ###
                ## Jacob Zeldin, Chicago Botanic Garden ##
                        ## Winter/Spring 2020 ##
### --------------------------------------------------------------------- ###

library(tidyverse)
library(ggthemes)
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

## Now  we have a function that will help us accomplish a common task. Let's say we want to
## apply this function to many different elements, such as those stored in a list There
## a few ways to accomplish this.

## Let's start by building a list each element will have 50 values with mean 5. The 
## standard deviation of each will vary

a <- rnorm(50,5,1)
b <- rnorm(50,5,2)
c <- rnorm(50,5,3)
d <- rnorm(50,5,5)

our_list <- list(a,b,c,d)
our_list

## Now, if we wanted to find the standard error of a list element, one way is to manually
## direct our function to apply to a list element

st.error(our_list[[2]])

## If we chose that approach, we'd have to write 5 seperate lines of code to get the 
## standard error of each list element. Imagine the list contained hundreds of elements,
## that would be extremely inefficient.

## One classic way to iterate our function over each list element is a for loop.

for (i in 1:length(our_list)){
  st.error(our_list[[i]])
}

## An even simpler way to accomplish this is with the apply() family of functions. In this
## case we will use lapply() because we are working with a list.

se_list <- lapply(our_list,st.error)

## And an even better way is to use the map() function from the purrr package. This method
## is best because it is less tempramental than the apply() functions, accepts most object
## types, while still reducing the amount of code that needs to be written (less than a for loop)

se_list2 <- map(our_list,st.error)


## This was obviously a simple example, but map(), apply(), and for loops also work for
## much more complicated tasks on larger data sets. Loops are also great for nesting functions
## when multiple tasks need to be performed in specific orders.


         #### Data wrangling and summarizing with the tidyverse (mostly dplyr!) ####


## It is rarely the case that data will be ready to explore and analayze as soon as you 
## import it into R. Most of us are entering data in Excel and even after it has been proofed
## and provisionally formatted, it might not be quite ready for primetime. Therefore, there is
## often some data wrangling and summarizing that needs to be done.

## This type of data prep can include calculations across columns (for example, calculating
## density as a function of mass and volume), transposing rows to columns, grouping data,
## gathering multiple responses into a key and value column pair, transforming response
## variables, etc. etc.

## The tidyverse family of packages/functions make these type of tasks a whole lot easier and
## are intuituve to use, often more so than alternatives in base R.


## Let's start with a common problem: you want to produce an exploratory figure illustrating
## multiple response variables at the same time. You don't want them in seperate facets, but
## instead would like them to appear as seperate colors in the same panel. This is a great
## use case for the gather() function from dplyr.

## To illustrate the solution, we will use the built in iris data set. There are four response
## variables, the width and length of petals and sepals.

head(iris)

## We may want to inspect the distributions of these responses simultaneously with ggplot.
## Because we want to code the different responses by color in our aes() call, we will need to
## collapse the four responses into two columns: a key column and a value column.

iris %>% 
  gather(-Species,key = "variable", value = "value") 
           # there is only one column that is not a response, so we will tell gather
           # to simply exlude the Species column

iris %>% 
  gather(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,key = "variable", value = "value") 
          # Alternatively, we can provide gather() with the names of the variable we would like
          # it to collapse

## Now we can use these collapsed columns to build our plot
  
iris %>% 
  gather(-Species,key = "variable", value = "value") %>% 
  ggplot(aes(value, fill = variable))+
  geom_density(alpha = 0.5, color = "black")+
  scale_fill_discrete(name = "Response")+
  scale_y_continuous(name = NULL, limits = c(0,1.1),expand = c(0,0))+
  scale_x_continuous(name = NULL, expand = c(0,0))+
  theme_bw()

## Great! Now we can see that the two petal responses appear to be biomdal while the sepal
## responses are not.


## Now let's move on to calculations and variable creation. We'll use the same iris dataset.
## suppose we want to create a new variable, sepal area, that multiplies the sepal length and
## width. We can do this easily ebough in base R by assigning a new column with the $ symbol.

iris$Sepal.Area <- iris$Sepal.Length * iris$Sepal.Width
iris$Sepal.Area

## But if we also wanted to calculate petal area, we'd have to do that all over again with
## more lines of code. Instead, we can use the mutate() function to accomplish both calculations
## in the same line of code. The other advantage of this method is we can go straight on to
## other functions, like gathering and plotting, using piping notation!!

iris %>% 
  mutate(Sepal.Area = Sepal.Length*Sepal.Width, Petal.Area = Petal.Length*Petal.Width) %>%
  gather(Sepal.Area,Petal.Area, key = "var", value = "value") %>% 
  ggplot(aes(Species, value, color = var))+
  geom_jitter(size = 2,width = 0.1, alpha = 0.5)+
  ylab("Area")+
  scale_color_discrete(name = NULL, labels = c("Petal","Sepal"))+
  theme_bw()


## The plot above would take more lines of code and potentially assigning new objects to
## generate without using mutate and piping. This approach is often easier, cleaner, 
## and more readable than alternative methods with base R.


## Next we will move on to summary functions. The summarise() function applies summaries to
## columns, generating a new table. Inside the summary function, you can include pretty
## much any other function. Let's see an example of summarise().

mtcars %>% 
  summarise(avg_mpg = mean(mpg), # here we find a mean
            sd_mpg = sd(mpg), # and the std. deviation
            obs = n(), # with n() we can get the sample size
            mpg_by_wt = mean(mpg)/mean(wt)) # dividing avg mpg by avg wt

## To extend the use of summarise(), we can first group the data by category. This is
## quite useful for quickly generating group means/variances from raw data in preparation
## for further analysis (ex. ANOVA)

mtcars %>% 
  group_by(cyl) %>% 
  summarise(avg_mpg = mean(mpg))

## We can also group by multiple independant variables

mtcars %>% 
  group_by(cyl, gear) %>% 
  summarise(avg_mpg = mean(mpg))

mtcars %>% 
  group_by(cyl, gear, am) %>% 
  summarise(avg_mpg = mean(mpg))

## And as usual with tidyverse workflows, we can use piping and go right into plotting!
## We will get NA errors for a few errorbars as we cant get a variance estimate when n < 2

mtcars %>% 
  group_by(cyl, gear,) %>% 
  summarise(avg_mpg = mean(mpg),
            se_mpg = st.error(mpg),
            n = n()) %>% 
  ggplot(aes(as.factor(cyl),avg_mpg, color = as.factor(gear)))+
    geom_point(size = 3,position = position_dodge(1))+
    geom_errorbar(aes(ymin = avg_mpg - se_mpg,
                      ymax = avg_mpg + se_mpg),
                  width = 0.1, position = position_dodge(1))+
  xlab("Cylinders")+
  ylab("MPG")+
  scale_color_brewer(palette = "Dark2",name = "Gears")+
  theme_bw()

# We could also fill out the figure with the observed data

mtcars %>% 
  group_by(cyl, gear,) %>% 
  summarise(avg_mpg = mean(mpg),
            se_mpg = st.error(mpg),
            n = n()) %>% 
  ggplot(aes(as.factor(cyl),avg_mpg, color = as.factor(gear)))+
  geom_point(data = mtcars, aes(as.factor(cyl),mpg, color = as.factor(gear)),
             position = position_dodge(1),inherit.aes = F, alpha = 0.5)+
  geom_point(size = 3,position = position_dodge(1),alpha = 0.75)+
  geom_errorbar(aes(ymin = avg_mpg - se_mpg,
                    ymax = avg_mpg + se_mpg),
                width = 0.1, position = position_dodge(1))+
  xlab("Cylinders")+
  ylab("MPG")+
  scale_color_brewer(palette = "Dark2",name = "Gears")+
  theme_bw()



## Now let's move on to another case of data wrangling. Let's say you have an experiment where
## you took data a few different sites. You've entered the data in seperate Excel spreadsheets
## and imported them into R and want to analyze them together (i.e run an ANOVA for differences
## between sites). You could go back into excel and try to copy paste the spreadsheets into a 
## single sheet... but this is time consuming and could lead to errors or omission of data. A
## better solution would be to combine them in R.

## Let's prepare an example while at the same time demonstrating another tool using
## the txhousing data set built into ggplot. A very common task in data analysis is subsetting.
## Let's say we want to extract only a single year of data from txhousing. We can do this
## using the filter() function.

tx_2004 <- filter(txhousing, year == 2004) # We provide the data and use conditional statements
                                           # to filter (subset) the data

tx_2005 <- filter(txhousing, year == 2005)  # now 2005

## Now if we want all years of data OTHER THAN 2004 and 2005, we can still use filter, but this 
## time we will use the conditional statement "year != 2004 & year != 2005" which translates to 
# year NOT equal to 2004 and NOT qual to 2005

tx_other_yrs <- filter(txhousing, year != 2004 & year != 2005 )

## Great. Now back to combing tables. Let's say that we recently entered all this housing
## data into seperate excel spreadsheets for each year, imported them into R, and wanted to 
## combine the years 2004 and 2005 into a single data frame for analysis. We could do this with
## rbind() in base R or bind_rows() in dplyr

two_years <- bind_rows(tx_2004,tx_2005)

## What if you have one data frame the data from an experiment such as the CO2 data set
## which measures CO2 uptake in plants of didfferent origins under different reatments.

head(CO2)

## And you have a second data frame with temperature associated with the two 
## chilling treatments.

chill <- tibble(Treatment = c("chilled","nonchilled"),
                Temperature = c(4,15))
chill

## And you'd like to combine the information from these two data frames. Dplyr has a series of 
## functions for just that! Using the left_join() function we can join matching values from
## one table to another.

left_join(CO2,chill, by = "Treatment")

## There is also right_join and full_join for other use cases, for example when
## you want to retain all rows from table Y, even when there is not a corresponding value
## in table X.






