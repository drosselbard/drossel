# mario random forest
# First things first, you can always input arrays for every user input
# formatted like this: c(first_value,second_value,...,n-th_value)
# for the year column it should be numeric values
# for the second custom column it should be logical values (TRUE/FALSE)

# if the needed libraries are not yet installed, un-comment this ####
#install.packages("tidyverse")
#install.packages("randomForest")

# libraries ####
library(tidyverse) # it's probably overkill to load tidyverse because it loads several packages, but oh well :^)
library(randomForest)

# setup ####
rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# read csv ####
file <- read.csv2(file.path(getwd(),'mario_random_forest.csv')) %>% select(!location)

# prediction only using the year as a predictor ####
## user inputs ####
new.game.years <- c(2025) # INPUT THE WANTED YEAR HERE
new.game <- as_tibble(matrix(nrow = length(new.game.years), ncol = ncol(file))) %>% `colnames<-`(colnames(file))
new.game$year <- new.game.years

for (i in 2:ncol(file)) {
  print(colnames(file)[i])
  
  rf <- randomForest::randomForest(as.formula(paste0(colnames(file)[i],'~.')), data = file %>% select(c('year', colnames(file)[i])))
  pd <- predict(object = rf, newdata = new.game %>% select(year))
  
  new.game[[colnames(file)[i]]] <- pd
}

## view results ####
View(new.game)

# prediction using two columns as a predictor ####
## user inputs ####
new.game.col2.name <- 'peach.kidnapped' # INPUT THE NAME OF THE SECOND DEFINED COLUMN HERE
new.game.col2 <- c(FALSE) # INPUT THE VALUE OF THE SECOND DEFINED COLUMN HERE

file <- file %>% select(c('year', new.game.col2.name), everything())
new.game <- as_tibble(matrix(nrow = length(new.game.years), ncol = ncol(file))) %>% `colnames<-`(colnames(file))
new.game$year <- new.game.years
new.game[[new.game.col2.name]] <- new.game.col2


for (i in 3:ncol(file)) {
  print(colnames(file)[i])
  
  rf <- randomForest::randomForest(as.formula(paste0(colnames(file)[i],'~.')), data = file %>% select(c('year', new.game.col2.name, colnames(file)[i])))
  pd <- predict(object = rf, newdata = new.game %>% select(c('year', new.game.col2.name)))
  
  new.game[[colnames(file)[i]]] <- pd
}

## view results ####
View(new.game)
