Tidy Data
================
J. Zeldin
September 14, 2018

## Teaching Modules

### I. Working with Tidy data

This module is an introduction to working with “tidy” data. This is a
specific way of organizing data in R and leverages the tidyverse family
of packages (Wickham et. al.) which include ‘ggplot2’, ‘dplyr’, ‘tidyr’,
and many others. This is just an introduction, read Wickham’s “R for
Data Science” for more in-depth details of what tidy data is and the
associated work-flow.

**The three rules that make data tidy as are as follows:**  
1\. Each variable must have its own column.  
2\. Each observation must have its own row.  
3\. Each value must have its own cell.

Working with data in this format facilitates all of the tidyverse
functions and manipulations you will need for exploration and analysis.

Often, the data you would like to work with is not in a tidy format by
default. There are a number of functions in the tidyr package that help
to conform your data to the tidy format. Here’s an example of an un-tidy
dataset which we will make tidy with tidyr
functions:

``` r
library(tidyverse)
```

``` r
# This is our un-tidy dataset. We will import it using read_csv() (the tidyverse verison of read.csv)

teachers <- read_csv("teachers.csv")
head(teachers,15)
```

    ## # A tibble: 15 x 4
    ##    state    county school count
    ##    <chr>    <chr>  <chr>  <dbl>
    ##  1 Oregon   a      rural    139
    ##  2 Oregon   a      urban    221
    ##  3 Oregon   b      rural    137
    ##  4 Oregon   b      urban    187
    ##  5 Oregon   c      rural    131
    ##  6 Oregon   c      urban    165
    ##  7 Oregon   d      rural     47
    ##  8 Oregon   d      urban    216
    ##  9 Oregon   e      rural    121
    ## 10 Oregon   e      urban    184
    ## 11 Michigan a      rural     88
    ## 12 Michigan a      urban    299
    ## 13 Michigan b      rural    126
    ## 14 Michigan b      urban    246
    ## 15 Michigan c      rural    136

The dataset above is NOT tidy\! We know this because it breaks our 2nd
rule: Each observation must have its own row. In this case, each county
is an observation and in our dataset there are two rows per county. How
do we fix this to make our data
tidy?

``` r
# To make our dataset tidy, we will use the spread() function to spread the school column into two seperate variables with values from the count column.

# The spread function has two arguments:
# 1) the "key" argument, which takes the column with the variable names that are to be spread. In our case, this is the school column.
# 2) the "value", which takes the column containing the values associated with the variables that are to be spread. In our case, this is the count column.

teachers %>% 
  spread(key = school, value = count)
```

    ## # A tibble: 10 x 4
    ##    state    county rural urban
    ##    <chr>    <chr>  <dbl> <dbl>
    ##  1 Michigan a         88   299
    ##  2 Michigan b        126   246
    ##  3 Michigan c        136   293
    ##  4 Michigan d        112   158
    ##  5 Michigan e        110   238
    ##  6 Oregon   a        139   221
    ##  7 Oregon   b        137   187
    ##  8 Oregon   c        131   165
    ##  9 Oregon   d         47   216
    ## 10 Oregon   e        121   184

What if we had a dataset the looks like the one below? It is even more
un-tidy\! This time, in addition to haveing multiple rows for each
observation, we have a situation where each variable does not have its
own column. That is, county and state are combine in a single column. We
can tidy this data with the separate() function

``` r
teachers2 <-  read.csv("teachers2.csv")
head(teachers2,15)
```

    ##    state_county school count
    ## 1      Oregon-a  rural   139
    ## 2      Oregon-a  urban   221
    ## 3      Oregon-b  rural   137
    ## 4      Oregon-b  urban   187
    ## 5      Oregon-c  rural   131
    ## 6      Oregon-c  urban   165
    ## 7      Oregon-d  rural    47
    ## 8      Oregon-d  urban   216
    ## 9      Oregon-e  rural   121
    ## 10     Oregon-e  urban   184
    ## 11   Michigan-a  rural    88
    ## 12   Michigan-a  urban   299
    ## 13   Michigan-b  rural   126
    ## 14   Michigan-b  urban   246
    ## 15   Michigan-c  rural   136

``` r
teachers2 %>% 
  spread(key = school, value = count) %>% # take care of multiple rows per
                                          # observatuion, as above
  separate(state_county,into = c("state","county"),sep = "-")
```

    ##       state county rural urban
    ## 1  Michigan      a    88   299
    ## 2  Michigan      b   126   246
    ## 3  Michigan      c   136   293
    ## 4  Michigan      d   112   158
    ## 5  Michigan      e   110   238
    ## 6    Oregon      a   139   221
    ## 7    Oregon      b   137   187
    ## 8    Oregon      c   131   165
    ## 9    Oregon      d    47   216
    ## 10   Oregon      e   121   184

``` r
                                          # here we used separate to split
                                          # state and county, using the 
                                          # separator "-" to tell the                                               # function where to split
```

There are many more functions for data wrangling in the tidyverse.
Explore them to more efficiently handle and explore your data.
