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



table(adult$type_employer)

# combining employer types
adult$type_employer <- ifelse(adult$type_employer == 'Never-worked' | adult$type_employer == 'Without-pay', 'Unemployed', adult$type_employer)
adult$type_employer <- ifelse(adult$type_employer == 'Federal-gov' | adult$type_employer == 'Local-gov', 'SL-gov', adult$type_employer)
adult$type_employer <- ifelse(adult$type_employer == 'Self-emp-inc' | adult$type_employer == 'Self-emp-not-inc', 'self-emp', adult$type_employer)

#combining marital column into 3 values
adult$marital <- trimws(adult$marital)
adult$marital <- ifelse(adult$marital == 'Married-AF-spouse' | adult$marital == 'Married-civ-spouse' | adult$marital == 'Married-spouse-absent', 'Married', adult$marital)

adult$marital <- ifelse(adult$marital == 'Divorced' | adult$marital == 'Separated' | adult$marital == 'Widowed', 'Not Married', adult$marital)

table(adult$marital)

table(adult$country)
unique(adult$country)

#combining countries
america <- c('Canada', 'Mexico', 'United-States', 'Outlying-US(Guam-USVI-etc)', 'Puerto-Rico')
asia <- c('China', 'Cambodia', 'Hong', 'India', 'Iran', 'Japan', 'Laos', 'Philippines', 'Taiwan', 'Thailand', 'Vietnam')
latin_central <- c('Columbia', 'Cuba', 'Dominican-Republic', 'Ecuador', 'El-Salvador', 'Guatemala', 'Haiti', 'Honduras', 'Jamaica', 'Nicaragua', 'Peru', 'Trinadad&Tobago')
europe <- c('England', 'France', 'Germany', 'Greece', 'Holand-Netherlands', 'Hungary', 'Ireland', 'Italy', 'Poland', 'Portugal', 'Scotland', 'Yugoslavia')

adult$continent <- ifelse(adult$country %in% america, 'North.America', 
                          ifelse(adult$country %in% asia, 'Asia', 
                                 ifelse(adult$country %in% latin_central, 'Latin.Central.America', 
                                        ifelse(adult$country %in% europe, 'Europe', 'Other'))))

table(adult$continent)

#fix education column
adult$education <- ifelse(adult$education_num < 9, 'HS-incomplete', adult$education)

#deal with ? values
adult[adult == '?'] <- NA

#checking for missing data
library(Amelia)
missmap(adult, legend = FALSE, col = c('yellow', 'black'))

#text to factor
adult <- adult %>% mutate_if(is.character, as.factor)

str(adult)
summary(adult)
