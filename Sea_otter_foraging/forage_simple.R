####### ANOVA for foraging data #####

library(ggplot2)

###  DATA  ###
s.prop <- read.csv("s_prop.csv") # import a file with the male/female proportions

age.prop <- read.csv("age_prop.csv")
all.prop <- read.csv("all_prop.csv")
year.prop <- read.csv("year_prop.csv")

# stack the "all" data
all <- stack(all.prop)
names(all) <- c("prop", "species")


# checking the data frame
dim(s.prop)
names(s.prop)
str(s.prop)
str(age.prop)
str(year.prop)


# ALL #
ggplot(data = all) +
  geom_col( 
    mapping= aes(x = species, y = prop)
  )
  
# AGE #
ggplot(data = age.prop) +
  geom_col( 
    mapping= aes(x = species, y = prop, fill = age),
    position = "dodge"
  )

# SEX #
ggplot(data = s.prop) +
  geom_col( 
    mapping= aes(x = species, y = prop, fill = sex),
    position = "dodge"
  )

# YEAR # The area list isn't working because it is a number...
ggplot(data = year.prop) +
  geom_col( 
    mapping= aes(x = species, y = prop, fill = area),
    position = "dodge"
  )

