#packages
install.packages("dplyr")
install.packages("Amelia")

#loading dataset
adult <- read.csv('adult_sal.csv')

summary(adult)
str(adult)

head(adult)

#removing duplicate index
library(dplyr)
adult <- select(adult, -X)

#checking for missing data
library(Amelia)
missmap(adult, legend = FALSE, col = c('yellow', 'black'))

table(adult$type_employer)

# combining never worked with without pay
adult$type_employer <- ifelse(adult$type_employer == 'Never-worked' | adult$type_employer == 'Without-pay', 'Unemployed', adult$type_employer)


table(adult$type_employer)
