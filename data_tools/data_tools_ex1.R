######### Data tools excercize ########## 
                  
library(tidyverse)
library(ggplot2)

## For this excercize we will use the built-in diamonds data set.

## We will start with an example of a for loop. Here, we want to loop through the data and
## attribute a "quality" cetegory to each observation. To begin, we will assign observations
## that have the best clarity ("IF") OR the best color ("J") to "High" quality. All other
## observations will be assigned a quality of "Other".


for (i in 1:nrow(diamonds)){
  if (diamonds[i,"clarity"] == "IF" | diamonds[i,"color"] == "J"){
    diamonds[i,"quality"] <- "High"
  }
  else {
    diamonds[i,"quality"] <- "Other"
  }
}

## Now we can look at the breakdown of qualities across the data
table(diamonds$quality)

## Now, for the excercize, do the following:
    # 1) Subset the diamonds data set to only "High" quality diamonds
    # 2) Find the price per carat for each obervation
    # 3) Use a for loop to categorize observations by weight - 
        # all diamonds with carat > 1 should be assigned "Heavy"
        # diamonds between 0.5 and 1 carats should be assigned "Medium" 
            # this includes diamonds exactly 0.5 or 1 carat
        # diamonds under 0.5 carats should be assigned "Light"
    # 4) Find the mean price per carat of each weight category 
    # 5) Generate a plot of price per carat by weight category. Plot each observation
        # jittering the points and making them semi-transparent (alpha = 0.2). Make the 
        # y-axis on a log10 scale. Also plot the category means with red triangles, making 
        # the points larger than the raw data. Include appropriate axes titles and a plot title.

d2 <- filter(diamonds, quality == "High")

d2$price_carat <- d2$price/d2$carat

for (i in 1:nrow(d2)) {
  if (d2[i, "carat"] > 1) {
    d2[i,"weight"] <- "Heavy"
  } else if (d2[i,"carat"] <= 1 & d2[i,"carat"] >= 0.5){
    d2[i,"weight"] <- "Medium"
  } else if (d2[i,"carat"] < 0.5){
    d2[i,"weight"] <- "Light"
  }
}

d2 %>% 
  group_by(weight) %>% 
  summarise(mean = mean(price_carat,na.rm = T)) %>% 
  ggplot(aes(weight,mean))+
  geom_jitter(data = d2, aes(weight,price_carat), width = 0.05, alpha = 0.2)+
  geom_point(shape = 17, size = 3, color = "red")+
  scale_y_log10(name = "Price per carat")+
  xlab("Weight category")+
  ggtitle("Price per carat by weight class of high quality diamonds")+
  theme_bw()
  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  