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
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

adult <- na.omit(adult)
  
#text to factor
adult <- adult %>% mutate_if(is.character, as.factor)

str(adult)
summary(adult)

### EDA
library(ggplot2)

#Histogram of age colored by salaries
ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()

#histogram of hours worked per week
ggplot(adult,aes(hr_per_week)) + geom_histogram(aes(fill=income), binwidth=3) + theme_bw()

# barplot of continent with the fill color defined by income class.
ggplot(adult, aes(continent)) + geom_bar(aes(fill=income), color='black') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

adult <- select(adult, -country)
## Creating the model

# Splitting into train and test using catools
install.packages("caTools")
library(caTools)

# Set a random see so your "random" results are the same as this notebook
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)

model <- glm(income ~. , family = binomial(logit), data = train)

summary(model)

new.model <- step(model)

summary(new.model)

### Predicting on test data
fitted.values <- predict(new.model, test, type = 'response')
fitted.results <- ifelse(fitted.values>0.5, 1, 0)

test$predicted.income = predict(model, newdata=test, type="response")

table(test$income, test$predicted.income > 0.5)

#model accuracy
(6284+1348)/(6284+1348+512+904)

#recall
6284/(6284+512)

#precision
6732/(6372+904)
