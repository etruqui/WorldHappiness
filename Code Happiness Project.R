title: "happiness_project"
output: html_document
---
  
  
  ```{r happiness, echo=FALSE}
#LOADING DATA AND DOWNLOADING PACKAGES
data = read.csv('https://raw.githubusercontent.com/arunagossai/happiness_project_R/master/happiness_data.csv')
head(data)
summary(data)
dim(data)
install.packages('fastDummies')
library(fastDummies)
```


```{r happiness, echo=FALSE}
#VARIABLE CREATION AND PREPROCESSING
years <- dummy_cols(data$year,NULL)
#droping the countries that have no data for depedent variables
df_countries  = subset(data, country != "Kosovo"  & country !="Taiwan" )
paste( dim(data)[1] - dim(df_countries)[1], "observations lost")
#droping NAs
df_NAs = subset(df_countries, democracy != 'NA' & refugee!= 'NA' & gini != 'NA' & child_mortality != 'NA')
paste( dim(df_countries)[1] - dim(df_NAs)[1], "observations lost")
#identifying outliers
plot(happiness~democracy, data)
plot(happiness~gini, data)
plot(happiness~child_mortality, data)
plot(happiness~sanitation, data)
plot(happiness~women_edu, data)
plot(happiness~men_edu, data)
plot(happiness~elder_child, data)
plot(happiness~pop_den, data)
plot(happiness~labour, data)
#droping outliers (maybe replace with an average?)
df <- subset(df_NAs, pop_den < 5000)
paste( dim(df_NAs)[1] - dim(df)[1], "observations lost")
```


```{r happiness, echo=FALSE}
#DESCRIPTIVE STATS
M1 <- lm(happiness~democracy, data)
plot(happiness~democracy, data)
abline(M1$coefficients[1],M1$coefficients[2],col = "blue")
M2<- lm(happiness~gini, data)
plot(happiness~gini, data)
abline(M2$coefficients[1],M2$coefficients[2],col = "blue")

#correlation matrix
#descriptive stats

