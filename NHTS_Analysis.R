library(tidyverse)
library(car) 
library(ggplot2)
library(gridExtra)
library(plotly)
library(RColorBrewer)
library(vcd)
library(readxl)
library(gmodels)
library(mapdata)

getwd()
#Reading the Household Dataset
hhdata1 <- read_csv("Csv/hhpub.csv")

#Replicating the data and preserving a copy
hhdata <-hhdata1
glimpse(hhdata)

#Exploring the dataset
View(hhdata)
dim(hhdata)
#The dataset consists of 58 columns and 129696 datarows

#Check if the dataset is loaded with correct datatypes. 
glimpse(hhdata)
#Considerable amount of recoding and datatype conversion to be performed on a large number of columns
#Before proceeding to that we will ensure that the data is clean


#Loading the Trip Dataset
tripdata1 <- read_csv("Csv/trippub.csv")

tripdata <- tripdata1 

View(tripdata)
glimpse(tripdata) 

#Extracting the HouseHoldID and Trip Miles to create the Dependent Variabe of Trip Miles 
trip <- tripdata %>% dplyr::select(HOUSEID,TRPMILES) %>% filter(TRPMILES>=0)

summary(trip$TRPMILES) #Observing a negative value for the trips

#Aggregating the trip miles per household 
tripmiles <- aggregate(trip$TRPMILES, by=list(Category=trip$HOUSEID), FUN=sum)

options(scipen = 99)
str(tripmiles)
names(tripmiles)[names(tripmiles)=="Category"] <- "HOUSEID"
names(tripmiles)[names(tripmiles)=="x"] <- "TRPMILES"

str(tripmiles)
hhdata_final <- merge(hhdata,tripmiles,by="HOUSEID")

#Additional column TRPMILES for each household is added to the dataset
hhdata <- hhdata_final
glimpse(hhdata)
dim(hhdata)

#Sampling the dataset
set.seed(462)
hhdata_final <- sample_frac(hhdata, 0.05)
dim(hhdata_final)
#Sampling the dataset and getting 5860 rows and 59 columns
#The Target Variable will be the TripMiles of the entire household for the travel day obtained 
#from the trip file

#Checking for normal distribution of the Target Variable TRPMILES
hist(hhdata_final$TRPMILES, main="Distr of TRPMILES", freq=F, col="grey") 
lines(density(hhdata_final$TRPMILES), main="Distribution of TRPMILES", col="yellow", lwd=2)
boxplot(hhdata_final$TRPMILES, main="Boxplot of TRPMILES", col="coral")

# Checking for transformations on the Dependent Variable (Trip Miles) 
summary(hhdata_final$TRPMILES)
plot(density(log(hhdata_final$TRPMILES)))
boxplot(log(hhdata_final$TRPMILES), main="Boxplot of TRPMILES", col="coral")

plot(density(sqrt(hhdata_final$TRPMILES)))
plot(density(1/(hhdata_final$TRPMILES)))
summary(powerTransform(hhdata_final$TRPMILES))

#Removing the outliers from TRPMILES
lower_outliers <- fivenum(hhdata_final$TRPMILES)[2]- IQR(hhdata_final$TRPMILES)*1.5
lower_outliers
which(hhdata_final$TRPMILES<=lower_outliers) #We find that there are 6 Lower Outliers
higher_outliers <- fivenum(hhdata_final$TRPMILES)[4]+IQR(hhdata_final$TRPMILES)*1.5
higher_outliers
which(hhdata_final$TRPMILES>=higher_outliers)

hhdata1 <- hhdata_final %>% filter(TRPMILES < higher_outliers) #3.177 percent of data is lost
dim(hhdata1)
dim(hhdata_final) # 477 rows from 5654 rows removed
# 8% of the data lost


hist(hhdata1$TRPMILES, main="Distr of TRPMILES", freq=F, col="grey") 
lines(density(hhdata1$TRPMILES), main="Distribution of TRPMILES", col="yellow", lwd=2)
boxplot(hhdata1$TRPMILES, main="Boxplot of TRPMILES", col="coral")


# Performing transformations on the Dependent variable (TRPMILES) 
summary(hhdata1$TRPMILES)
plot(density(log(hhdata1$TRPMILES)))
plot(density(sqrt(hhdata1$TRPMILES)))
plot(density(1/(hhdata1$TRPMILES)))

par(mfrow=c(1,2))
hist(sqrt(hhdata1$TRPMILES), main="Distr of TRPMILES", freq=F, col="grey") 
lines(density(sqrt(hhdata1$TRPMILES)), main="Distribution of TRPMILES", col="yellow", lwd=2)
boxplot(sqrt(hhdata1$TRPMILES), main="Boxplot of TRPMILES", col="coral")
dev.off()
#Square Root transformation looks good!
#The outliers are also removed when using the sqrt transformation

# Thus the final Dependent variable to be used is Square Root of Trip Miles.




#############################Factor Analysis on Variables PLACE,WALK2SAVE, BIKE2SAVE, PTRANS
which(colnames(hhdata1)=="PLACE")
which(colnames(hhdata1)=="WALK2SAVE")
which(colnames(hhdata1)=="BIKE2SAVE")
which(colnames(hhdata1)=="PTRANS")

hhdata_new <- hhdata1
hhdata_new$PLACE <- as.numeric(hhdata_new$PLACE)
hhdata_new$WALK2SAVE <- as.numeric(hhdata_new$WALK2SAVE)
hhdata_new$BIKE2SAVE <- as.numeric(hhdata_new$BIKE2SAVE)
hhdata_new$PTRANS <- as.numeric(hhdata_new$PTRANS)

dev.off()
hhdata4 <- data.frame(scale(hhdata_new[, c(19,20,21,22)]))
corrplot(cor(hhdata4), method="circle", addCoef.col="grey", type="upper")

fa.parallel(hhdata4, fa="fa", n.iter=100, show.legend=T)
hhdatafa3 <- factanal(hhdata4, 1) 
hhdatafa3$loadings


################################## Subseting data for Analysis ########################### 

# Subseting only the columns that we're planning to use.
# Can add additional columns or subset another dataset for analysis of other columns.
house1 <- dplyr::select(hhdata1, HH_RACE, HOUSEID, TRPMILES, TRAVDAY, HHSIZE, HHVEHCNT, HHFAMINC, PRICE, PLACE, DRVRCNT, WRKCOUNT, MSASIZE, URBRUR)
dim(house1)
#5344 rows and 13 columns



################################# Analysis of HHFAMINC ###################################################

house1$HHFAMINC <- as.factor(house1$HHFAMINC)

house1$HHFAMINC <- dplyr::recode(house1$HHFAMINC, "01"="Low", "02"="Low","03"="Low","04"="Low","05"="Middle","06"="Middle","07"="Middle","08"="Middle","09"="Upper","10"="Upper","11"="Upper","-7"="Other","-8"="Other","-9"="Other") 
#Lower, Upper and Middle is much better

#Changing to correct datatype
house1$HHFAMINC <- factor(house1$HHFAMINC,  levels = c("Low", "Middle", "Upper","Other"))
table(house1$HHFAMINC)
prop.table(table(house1$HHFAMINC))
#54% are in Middle class, 29% in Low income leve and 16% in Upper income level


###### HHFAMINC Uni-variate ##############

barplot(table(house1$HHFAMINC), xlab = "Frequency of Family Income", ylab = "Counts", ylim = c(0, 3000))
barplot(round((table(house1$HHFAMINC)/nrow(house1)*100), digits = 2), main = "Distribution of Household Income", xlab = "Household Income", ylab = "Percent Frequency", ylim = c(0, 60), col = grey.colors(3))


###### HHFAMINC Bivariate ##############

boxplot(sqrt(house1$TRPMILES)~house1$HHFAMINC, data=house1, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby Household Income",ylab = "Square root of Trip miles", xlab = "Household Size") # \n is a new line


###### HHFAMINC ANOVA ##############
aov_inc <- aov(sqrt(TRPMILES)~HHFAMINC, house1)
summary(aov_inc) #Statistically significant


######## HHFAMINC Linear Model ########

lm_HHFAMINC <- lm(sqrt(TRPMILES)~HHFAMINC, house1)
summary(lm_HHFAMINC) #Other is no longer statistically significant


################# Plotting HHFAMINC with ggplot ########################################
# 1. HHSize Univariate 
ggplot(data=house1, aes(HHFAMINC)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(title='Distribution of Household Income', x = "Household Income", y = "Frequency")

#Plotting the percent data
data <- house1 %>% count(HHFAMINC) %>%  mutate(perc = (n / nrow(house1)) *100)


HHFAMINC_bar <-  ggplot(data = data, aes(x=HHFAMINC, y=perc )) + 
  geom_bar(fill="steelblue", stat = "identity") + theme_minimal()+
  labs(title='Distribution by Household Income')
HHFAMINC_bar + ylim(0,60) +  xlab("Household Income") + ylab("Percentage")

# 2. HHSize Bivariate
ggplot(house1, aes(x=HHFAMINC, y=(sqrt(TRPMILES)), color=HHFAMINC)) + 
  geom_boxplot()+
  labs(title='Distribution of Trip Miles by Household Income', x = "Household Income", y = "Square root of Trip Miles")
#Households with higher income cover more Trip Miles compared to Low and Middle income households as expected


################################# Analysis of TRAVDAY ##################################################

summary(house1$TRAVDAY)
house1$TRAVDAY<- as.factor(house1$TRAVDAY)

#Recoding the variable to Weekday and Weekend
house1$TRAVDAY <- dplyr::recode(house1$TRAVDAY, "01"="Weekend", "02"="Weekday","03"="Weekday","04"="Weekday","05"="Weekday","06"="Weekday","07"="Weekend") 
table(house1$TRAVDAY)
prop.table(table(house1$TRAVDAY))
#79% of household travels on Weekdays and 20% on Weekends



###### TRAVDAY Uni-variate ##############

barplot(table(house1$TRAVDAY), xlab = "Frequency of Travel Day", ylab = "Counts", ylim = c(0, 5000))
barplot(round((table(house1$TRAVDAY)/nrow(house1)*100), digits = 2), main = "Distribution of Travel Day", xlab = "Travel Day", ylab = "Percent Frequency", ylim = c(0, 80), col = grey.colors(3))


###### TRAVDAY Bivariate ##############

boxplot(sqrt(house1$TRPMILES)~house1$TRAVDAY, data=house1, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby Travel Day",ylab = "Square root of Trip miles", xlab = "Travel Day") # \n is a new line



###### TRAVDAY ANOVA ##############
aov_travday <- aov(sqrt(TRPMILES)~TRAVDAY, house1)
summary(aov_travday) #Statistically significant


######## TRAVDAY Linear Model ########

lm_TRAVDAY <- lm(sqrt(TRPMILES)~TRAVDAY, house1)
summary(lm_TRAVDAY) 


################# Plotting TRAVDAY with ggplot ########################################
# 1. TRAVDAY Univariate 
ggplot(data=house1, aes(TRAVDAY)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(title='Distribution of Travel Day', x = "Travel Day", y = "Frequency")

#Plotting the percent data
data <- house1 %>% count(TRAVDAY) %>%  mutate(perc = (n / nrow(house1)) *100)


HHFAMINC_bar <-  ggplot(data = data, aes(x=TRAVDAY, y=perc )) + 
  geom_bar(fill="steelblue", stat = "identity") + theme_minimal()+
  labs(title='Distribution by Travel Day')
HHFAMINC_bar + ylim(0,80) +  xlab("Travel Day") + ylab("Percentage")

# 2. TRAVDAY Bivariate
ggplot(house1, aes(x=TRAVDAY, y=(sqrt(TRPMILES)), color=TRAVDAY)) + 
  geom_boxplot()+
  labs(title='Distribution of Trip Miles by Travel Day', x = "Travel Day", y = "Square root of Trip Miles")

#More TripMiles travelled compared to Weekend, confirmed from the median values of boxplots


############################# Analysis of HHSIZE ####################################################

table(house1$HHSIZE)
str(house1$HHSIZE)

# Recoding HHSIZE

house1$HHSIZE[house1$HHSIZE >= 3] <- "3 & Above"
table(house1$HHSIZE)
house1$HHSIZE <- as.factor(house1$HHSIZE)

round((table(house1$HHSIZE)/nrow(house1)*100), digits = 0)
#45% of households has 2 members, 30% has a single member and 25% has 3 or more members


###### HHSIZE Uni-variate ##############

barplot(table(house1$HHSIZE), xlab = "Frequency of Household Size", ylab = "Counts", ylim = c(0, 3000))
barplot(round((table(house1$HHSIZE)/nrow(house1)*100), digits = 2), main = "Distribution of Household Size", xlab = "Household Size", ylab = "Percent Frequency", ylim = c(0, 50), col = grey.colors(3))

###### HHSIZE Bivariate ##############

boxplot(sqrt(house1$TRPMILES)~house1$HHSIZE, data=house1, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby Household size",ylab = "Square root of Trip miles", xlab = "Household Size") # \n is a new line

######## HHSIZE Anova ################
options(scipen = 99)

aov_hhsize <- aov(sqrt(TRPMILES)~HHSIZE, house1)
summary(aov_hhsize) #Statistically significant

######## HHSIZE Linear Model ########

lm_hhsize <- lm(sqrt(TRPMILES)~HHSIZE, house1)
summary(lm_hhsize)

################# Plotting HHsize with ggplot ########################################
# 1. HHSize Univariate 
ggplot(data=house1, aes(HHSIZE)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(title='Distribution of Household Size', x = "Household Size", y = "Frequency")

#Plotting the percent data
data <- house1 %>% count(HHSIZE) %>%  mutate(perc = (n / nrow(house1)) *100)

hhsize_bar <-  ggplot(data = data, aes(x=HHSIZE, y=perc )) + 
  geom_bar(fill="steelblue", stat = "identity") + theme_minimal()+
  labs(title='Distribution by Household Size')
hhsize_bar + ylim(0,50) +  xlab("Household Size") + ylab("Percentage")

# 2. HHSize Bivariate
ggplot(house1, aes(x=HHSIZE, y=(sqrt(TRPMILES)), color=HHSIZE)) + 
  geom_boxplot()+
  labs(title='Distribution of Trip Miles by Household Size', x = "Household size", y = "Square root of Trip Miles")
#Households with 3 or more members cover more trip miles than single member and two member households


############################ Analysis for Vehicle count ############################
dev.off()
str(hhdata1)

### Recoding HHVEHCNT

house1$HHVEHCNT[house1$HHVEHCNT<= 1] <- "1 or Less"
house1$HHVEHCNT[house1$HHVEHCNT>= 3] <- "3 & Above"
table(house1$HHVEHCNT)

house1$HHVEHCNT <- as.factor(house1$HHVEHCNT)
house1$HHVEHCNT <- dplyr::recode(house1$HHVEHCNT, "1 or Less" = "1 or Less", "2" = "2","10" = "3 & Above", "11" = "3 & Above", "3 & Above" = "3 & Above")

house1$HHVEHCNT <- factor(house1$HHVEHCNT, levels = c("1 or Less", "2", "3 & Above"))

table(house1$HHVEHCNT)/nrow(house1)

round((table(house1$HHVEHCNT)/nrow(house1)*100), digits = 2)
round((table(house1$HHVEHCNT)/nrow(house1)*100), digits = 0)
#40% of households own 2 vehicles and only 24% of households owns 3 or more vehicles while rest own 1 vehicle or no vehicle


###### HHVEHCNT Univariate ##############
barplot(table(house1$HHVEHCNT), xlab = "Number of vehicles", ylab = "Counts", ylim = c(0, 2000))
barplot(round((table(house1$HHVEHCNT)/nrow(house1)*100), digits = 2), main = "Vehicles per household",  xlab = "Number of vehicles", ylab = "Percent frequency", ylim = c(0, 50), col = grey.colors(3))

###### HHVEHCNT Bivariate ##############
plot(house1$HHVEHCNT, sqrt(house1$TRPMILES), ylab = "Square root of Trip miles", xlab = "Number of vehicles")

boxplot(sqrt(house1$TRPMILES)~house1$HHVEHCNT, data=house1, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby Vehicle count",ylab = "Square root of Trip miles", xlab = "Number of vehicles/household") # \n is a new line 

################################ Plotting Vehicle count in ggplot ##########################

#Plotting the percent data
data1 <- house1 %>% count(HHVEHCNT) %>%  mutate(perc = (n / nrow(house1)) *100)

# 1. HHVEHCNT Univariate
veh_bar <-  ggplot(data=data1, aes(x=HHVEHCNT, y=perc )) + 
  geom_bar(fill="steelblue", stat = "identity") + theme_minimal()+
  labs(title='Distribution by Vehicle count')
veh_bar + ylim(0,50) +  xlab("Vehicle Count") + ylab("Percentage")

# 2. HHVEHCNT Bivariate
ggplot(house1, aes(x=HHVEHCNT, y=(sqrt(TRPMILES)), color=HHVEHCNT)) + 
  geom_boxplot()+
  labs(title='Distribution of Trip Miles by Vehicle Count', x = "Vehicle Count", y = "Square root of Trip Miles")
#More trip miles covered by households with vehicle 3 or above than the others

######## HHVEHCNT Anova ################

table(house1$HHVEHCNT)
aov_vehicle <- aov(sqrt(TRPMILES)~HHVEHCNT, house1)
summary(aov_vehicle) #Statistically significant

# Thus we can observe that as the number of vehicles increases, the trip miles increases.

######## HHVEHCNT Linear Model ########

lm_vehicle <- lm(sqrt(TRPMILES)~HHVEHCNT, house1)
summary(lm_vehicle)

plot(lm_vehicle)

############################ Analysis for Urban/Rural ############################

str(house1)

### Recoding URBRUR

house1$URBRUR <- as.factor(house1$URBRUR)
house1$URBRUR <- dplyr::recode(house1$URBRUR, "01" = "Urban", "02" = "Rural")

summary(house1$URBRUR)

###### URBRUR Univariate ##############
table(house1$URBRUR)
round((table(house1$URBRUR)/nrow(house1)*100), digits = 2)
round((table(house1$URBRUR)/nrow(house1)*100), digits = 0)
#79% are in Urban while 21% are in Rural

barplot(table(house1$URBRUR), xlab = "Urban/Rural", ylab = "Counts", ylim = c(0, 5000))
barplot(round((table(house1$URBRUR)/nrow(house1)*100)), xlab = "Urban/Rural", ylab = "Percent Frequency", ylim = c(0, 100))

###### URBRUR Bivariate ##############
plot(house1$URBRUR, sqrt(house1$TRPMILES), ylab = "Square Root of Trip miles", xlab = "Urban/Rural", main="Distribution of Trip miles\nby locality")

boxplot(sqrt(house1$TRPMILES)~house1$URBRUR, data=house1, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby locality",ylab = "Square Root of Trip miles", xlab = "Urban Vs Rural") # \n is a new line 

####################### Plotting URBRUR in ggplot ########################################

# 1. URBRUR Univariate  
#Plotting the percent data
data3 <- house1 %>% count(URBRUR) %>%  mutate(perc = (n / nrow(house1)) *100)

urban_bar <-  ggplot(data=data3, aes(x=URBRUR, y=perc )) + 
  geom_bar(fill="steelblue", stat = "identity") + theme_minimal()+
  labs(title='Distribution by Urban Vs Rural')
urban_bar + ylim(0,100) +  xlab("Urban Vs Rural") + ylab("Percentage")

# 2. URBRUR Bivariate
ggplot(house1, aes(x=URBRUR, y=(sqrt(TRPMILES)), color=URBRUR)) + 
  geom_boxplot()+
  labs(title='Distribution of Trip Miles by Urban Vs Rural', x = "Urban Vs Rural", y = "Square root of Trip Miles")


######## URBRUR Anova ################

aov_urban <- aov(sqrt(TRPMILES)~URBRUR, house1)
summary(aov_urban) #Statistically significant

# Thus we can observe that the median trip miles in rural areas is higher than the median trip miles in Urban areas.

######## URBRUR Linear Model ########

lm_urban <- lm(sqrt(TRPMILES)~URBRUR, house1)
summary(lm_urban)

################################# Analysis of Driver count ############################

str(house1$DRVRCNT)
table(house1$DRVRCNT)

#### Recoding DRVRCNT

house1$DRVRCNT[house1$DRVRCNT <= 1] <- "1 or Less"
house1$DRVRCNT[house1$DRVRCNT >= 3] <- "3 & Above"
table(house1$DRVRCNT)
round((table(house1$DRVRCNT)/nrow(house1)*100), digits = 2)
round((table(house1$DRVRCNT)/nrow(house1)*100), digits = 0)
#51% of households has 2 drivers, 40% has 1 or less and only 9% has 3 or more

house1$DRVRCNT <- as.factor(house1$DRVRCNT)

###### DRVRCNT Univariate ##############
barplot(table(house1$DRVRCNT), xlab = "Number of Drivers", ylab = "Counts", ylim = c(0, 3000))

barplot(round((table(house1$DRVRCNT)/nrow(house1)*100), digits = 2), xlab = "Number of Drivers", ylab = "Percent Frequency", ylim = c(0, 60))

###### DRVRCNT Bivariate ##############
boxplot(sqrt(house1$TRPMILES)~house1$DRVRCNT, data=house1, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby Driver count",ylab = "Square root of Trip miles", xlab = "Number of Drivers/household") # \n is a new line

############################# Plotting DRVRCNT in ggplot ################################## 

# 1. DRVRCNT Univariate
#Plotting the percent data
data2 <- house1 %>% count(DRVRCNT) %>%  mutate(perc = (n / nrow(house1)) *100)

dri_bar <-  ggplot(data=data2, aes(x=DRVRCNT, y=perc )) + 
  geom_bar(fill="steelblue", stat = "identity") + theme_minimal()+
  labs(title='Distribution by Driver count')
dri_bar + ylim(0,75) +  xlab("Driver count") + ylab("Percentage")

# 2. DRVRCNT Bivariate
ggplot(house1, aes(x=DRVRCNT, y=(sqrt(TRPMILES)), color=DRVRCNT)) + 
  geom_boxplot()+
  labs(title='Distribution of Trip Miles by Driver count', x = "Driver count", y = "Square root of Trip Miles")
#the median trip miles in households with 3 & Above  is higher than the median trip miles of rest

######## DRVRCNT Anova ################
table(house1$DRVRCNT)

aov_driver <- aov(sqrt(TRPMILES)~DRVRCNT, house1)
summary(aov_driver) #Statistically significant

# Thus we can observe that as the number of drivers increases, the trip miles increases.

######## DRVRCNT Linear Model ########

lm_driver <- lm(sqrt(TRPMILES)~DRVRCNT, house1)
summary(lm_driver)

################################# Multiple Linear Regression #################################### 

# Below are the linear models that we generated by including the variables 
# that we found to be significant in the bi-variate analysis.

lm_multi1 <- lm(sqrt(TRPMILES)~WRKCOUNT+HHVEHCNT+URBRUR, house1)
summary(lm_multi1)

lm_multi2 <- lm(sqrt(TRPMILES)~DRVRCNT+HHVEHCNT+URBRUR+HHSIZE, house1)
summary(lm_multi2)

lm_multi3 <- lm(sqrt(TRPMILES)~DRVRCNT+HHVEHCNT+HHSIZE, house1)
summary(lm_multi3)

lm_multi4 <- lm(sqrt(TRPMILES)~WRKCOUNT+HHVEHCNT+HHSIZE, house1)
summary(lm_multi4)

lm_multi5 <- lm(sqrt(TRPMILES)~WRKCOUNT+DRVRCNT+HHVEHCNT+URBRUR+HHSIZE, house1)
summary(lm_multi5)
plot(lm_multi5)

# Model "lm_multi5" was found to be the best model till now with the adjusted R-squared 
# value being 22.18%.

### Model with interaction term WRKCOUNT*HHSIZE
lm_multi6 <- lm(sqrt(TRPMILES)~WRKCOUNT+HHSIZE+WRKCOUNT*HHSIZE, house1)
summary(lm_multi6)
# The interaction term was found to be not significant

lm_multi8 <- lm(sqrt(TRPMILES)~WRKCOUNT+DRVRCNT+HHVEHCNT+HHSIZE+HHFAMINC, house1)
summary(lm_multi8)


################################# Analysis of MSA Size #########################################

table(house1$MSASIZE)
str(house1$MSASIZE)
house1$MSASIZE <- as.factor(house1$MSASIZE)

table(house1$MSASIZE)

#### Recoding MSASIZE

house1$MSASIZE <- dplyr::recode(house1$MSASIZE, "01" = "MSA < 250K", "02" = "250K > MSA < 500K", "03" = "500K >= MSA < 1M", "04" = "1M >= MSA < 3M", "05" = "MSA >= 3M", "06" = "Not MSA")
table(house1$MSASIZE)
round((table(house1$MSASIZE)/nrow(house1)*100), digits = 2)

###### Univariate ##############
barplot(table(house1$MSASIZE), xlab = "Population Category", ylab = "Counts", ylim = c(0, 1500))

barplot(round((table(house1$MSASIZE)/nrow(house1)*100), digits = 2), xlab = "Population Category", ylab = "Percent Frequency", ylim = c(0, 30))

###### Bivariate ##############

boxplot(sqrt(house1$TRPMILES)~house1$MSASIZE, data=house1, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby MSA Size",ylab = "Square root Trip miles", xlab = "MSA Size") # \n is a new line

######## MSASIZE Anova ################

aov_MSAsize <- aov(sqrt(TRPMILES)~MSASIZE, house1)
summary(aov_MSAsize) #Not Statisctically significant

######## Linear Model ########

lm_MSAsize <- lm(sqrt(TRPMILES)~MSASIZE, house1)
summary(lm_MSAsize)


##################### Analysis of PLACE (if travel is a Financial Burden) ##################

str(house1$PLACE)
table(house1$PLACE)
house1$PLACE <- as.factor(house1$PLACE)

### Recoding PLACE

house1$PLACE_coded <- dplyr::recode(house1$PLACE, "01" = "Agree", "02" = "Agree", "03" = "Other", "04" = "Disagree", "05" = "Disagree", "-9" = "Other")
table(house1$PLACE_coded)
prop.table(table(house1$PLACE_coded))
#37% Agree that travel is a burden and 25% disagree that travel is a burden


###### Univariate ##############
barplot(table(house1$PLACE_coded), xlab = "Travel is Financial Burden", ylab = "Counts", ylim = c(0, 2000))

###### Bivariate ##############
plot(house1$PLACE_coded, sqrt(house1$TRPMILES), ylab = "Square Root of Trip miles", xlab = "Travel is Financial Burden")

###################### Plotting PLACE using ggplot #########################################

# 1. PLACE Univariate 
#Plotting the percent data
data5 <- house1 %>% count(PLACE_coded) %>%  mutate(perc = (n / nrow(house1)) *100)

place_bar <-  ggplot(data=data5, aes(x=PLACE_coded, y=perc )) + 
  geom_bar(fill="steelblue", stat = "identity") + theme_minimal()+
  labs(title='Is Travel a Financial Burden?')
place_bar + ylim(0,50) +  xlab("Travel as Financial Burden") + ylab("Percentage")

# 2. PLACE Bivariate
ggplot(house1, aes(x=PLACE_coded, y=(sqrt(TRPMILES)), color=PLACE_coded)) + 
  geom_boxplot()+
  labs(title='Distribution of Trip Miles by Travel as Financial Burden ', x = "Travel as Financial Burden", y = "Square root of Trip Miles")

######## PLACE Anova ################

aov_place2 <- aov(sqrt(TRPMILES)~house1$PLACE_coded, house1)
summary(aov_place2) #Not statistically significant

######## PLACE Linear Model ########

lm_place <- lm(sqrt(TRPMILES)~PLACE_coded, house1)
summary(lm_place)
#Travel is financial burden is not statistically significant

###################################### Analysis of Worker Count ############################ 

table(house1$WRKCOUNT)
str(house1$WRKCOUNT)
house1$WRKCOUNT <- as.factor(house1$WRKCOUNT)
table(house1$WRKCOUNT)

house1$WRKCOUNT <- dplyr::recode(house1$WRKCOUNT, "0" = "0", "1" = "1", "2" = "2 & Above", "3" = "2 & Above", "4" = "2 & Above", "5" = "2 & Above")



################################# Plotting WRKCOUNT in ggplot ###############################

# 1. WRKCOUNT Univariate
#Plotting the percent data
data4 <- house1 %>% count(WRKCOUNT) %>%  mutate(perc = (n / nrow(house1)) *100)

work_bar <-  ggplot(data=data4, aes(x=WRKCOUNT, y=perc )) + 
  geom_bar(fill="steelblue", stat = "identity") + theme_minimal()+
  labs(title='Distribution by Worker Count')
work_bar + ylim(0,50) +  xlab("Worker Count") + ylab("Percentage")



# 2. WRKCOUNT Bivariate

ggplot(house1, aes(x=WRKCOUNT, y=(sqrt(TRPMILES)), color=WRKCOUNT)) + 
  geom_boxplot()+
  labs(title='Distribution of Trip Miles by Worker Count', x = "Worker Count", y = "Square root of Trip Miles")
#the median trip miles in households with 2 & Above Workers is higher than the median trip miles of rest




################ WRKCOUNT Anova ###########################################

aov_worker <- aov(sqrt(TRPMILES)~WRKCOUNT, house1)
summary(aov_worker) #Statistically Significant
str(house1$WRKCOUNT)


################################ Analysis of Location & Travel Behavior Data #####################################

######## Subsetting again to create new Dataset for Analysis ###########################

house2 <- dplyr::select(hhdata1, HOUSEID, HHSTATE, WALK, BIKE, CAR, TAXI, BUS, TRAIN, PARA, WALK2SAVE, BIKE2SAVE, PTRANS, TRPMILES)

house2
str(house2)

house1$HH_RACE <- as.factor(house1$HH_RACE)
house1$race_coded <- dplyr::recode(house1$HH_RACE, "01" = "White", "02" = "African American", "03" = "Asian", "04" = "American Indian", "05" = "Native Hawaiian", "06" = "Mixed Race", "97" = "Other", "-8" = "No Answer", "-7" = "No Answer")

table(house1$race_coded)

house2$bike2save_coded <- dplyr::recode(house2$BIKE2SAVE, "01" = "Agree", "02" = "Agree", "03" = "Other", "04" = "Disagree", "05" = "Disagree", "-9" = "Other", "-8" = "Other", "-7" = "Other")
table(house2$bike2save_coded)

write.xlsx2(house2, file = "house2.xlsx", sheetName = "house2",
            col.names = TRUE, row.names = TRUE, append = FALSE)

table(house2$WALK2SAVE)
str(house2$WALK2SAVE)

#Recoding WALK2SAVE
house2$WALK2SAVE <- as.factor(house2$WALK2SAVE)
str(house2$WALK2SAVE)

house2$walk2save_coded <- dplyr::recode(house2$WALK2SAVE, "01" = "Agree", "02" = "Agree", "03" = "Other", "04" = "Disagree", "05" = "Disagree", "-9" = "Other")

table(house2$BIKE2SAVE)
table(house2$walk2save_coded)

#Recoding BIKE2SAVE
house2$BIKE2SAVE <- as.factor(house2$BIKE2SAVE)

house2$bike2save_coded <- dplyr::recode(house2$BIKE2SAVE, "01" = "Agree", "02" = "Agree", "03" = "Other", "04" = "Disagree", "05" = "Disagree", "-9" = "Other")
table(house2$bike2save_coded)

table(house2$PTRANS)

house2$PTRANS <- as.factor(house2$PTRANS)

house2$ptrans_coded <- dplyr::recode(house2$PTRANS, "01" = "Agree", "02" = "Agree", "03" = "Other", "04" = "Disagree", "05" = "Disagree", "-9" = "Other")
table(house2$ptrans_coded)

xtab_place_walk <- xtabs(~house1$PLACE_coded + house2$walk2save_coded)
xtab_place_walk

chisq.test(house1$PLACE_coded, house2$walk2save_coded)
CrossTable(house1$PLACE_coded, house2$walk2save_coded, prop.c = F, prop.r = F,
           dnn = c('Travel is burden', 'Walk2Save'), format=c("SPSS"), chisq = T)

xtab_place_bike <- xtabs(~house1$PLACE_coded + house2$bike2save_coded)
xtab_place_bike

chisq.test(house1$PLACE_coded, house2$bike2save_coded)
CrossTable(house1$PLACE_coded, house2$bike2save_coded, prop.c = F, prop.r = F,
           dnn = c('Travel is burden', 'Bike2Save'), format=c("SPSS"), chisq = T)

xtab_place_ptrans <- xtabs(~house1$PLACE_coded + house2$ptrans_coded)
xtab_place_ptrans

chisq.test(house1$PLACE_coded, house2$ptrans_coded)
CrossTable(house1$PLACE_coded, house2$ptrans_coded, prop.c = F, prop.r = F,
           dnn = c('Travel is burden', 'Use Public Transportation'), format=c("SPSS"), chisq = T)


################################# Analysis of PTRANS ##############################################

str(house2$PTRANS)
house2$ptrans_coded <- as.factor(house2$PTRANS)

house2$ptrans_coded <- dplyr::recode(house2$PTRANS, "01" = "Agree", "02" = "Agree", "03" = "Other", "04" = "Disagree", "05" = "Disagree", "-9" = "Other")
table(house2$ptrans_coded)

round((table(house2$ptrans_coded)/nrow(house2)*100), digits = 0)



###### Univariate ##############
barplot(table(house2$ptrans_coded), xlab = "Frequency of PTRANS", ylab = "Counts", ylim = c(0, 1500))

barplot(round((table(house2$ptrans_coded)/nrow(house1)*100), digits = 2), xlab = "Frequency of PTRANS", ylab = "Percent Frequency", ylim = c(0, 50))

###### ptrans_coded Bivariate ##############
plot(house2$ptrans_coded, sqrt(house2$TRPMILES), ylab = "Square Root Trip miles", xlab = "Public Transportation is Burden")

boxplot(sqrt(house2$TRPMILES)~house2$ptrans_coded, data=house2, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby frequency of Walks",ylab = "Square root of Trip miles", xlab = "Public Transportation is Burden") # \n is a new line
#Distribution of TripMiles is almost the same across all classes

######## ptrans_coded Anova ################

aov_ptrans_coded <- aov(sqrt(TRPMILES)~ptrans_coded, house2)
summary(aov_ptrans_coded) #Statistically Significant

######## Linear Model ########
lm_ptrans_coded <- lm(sqrt(TRPMILES)~ptrans_coded, house2)
summary(lm_ptrans_coded)




################################# Analysis of WALK ##############################################

str(house2$WALK)
house2$WALK <- as.factor(house2$WALK)

table(house2$WALK)

house2$WALK <- dplyr::recode(house2$WALK, "01" = "Daily", "02" = "Few times a week", "03" = "Few times a month", "04" = "Few times a year", "05" = "Never", "-9" = "Other")
table(house2$WALK)
round((table(house2$WALK)/nrow(house2)*100), digits = 0)
#Only 18% of households Walk Daily while 18% of Households takes walk few times a year

###### Univariate ##############
barplot(table(house2$WALK), xlab = "Frequency of Walks", ylab = "Counts", ylim = c(0, 1500))

barplot(round((table(house2$WALK)/nrow(house1)*100), digits = 2), xlab = "Frequency of Walks", ylab = "Percent Frequency", ylim = c(0, 50))

###### Bivariate ##############
plot(house2$WALK, sqrt(house2$TRPMILES), ylab = "Square Root of Trip miles", xlab = "Frequency of Walks")

boxplot(sqrt(house2$TRPMILES)~house2$WALK, data=house2, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby frequency of Walks",ylab = "Square root of Trip miles", xlab = "Frequency of Walks") # \n is a new line
#Distribution of TripMiles is almost the same across all classes

######## WALK Anova ################

aov_walk <- aov(sqrt(TRPMILES)~WALK, house2)
summary(aov_walk) #Statistically Significant

######## Linear Model ########

lm_walk <- lm(sqrt(TRPMILES)~WALK, house2)
summary(lm_walk)

################################# Analysis of BIKE ##############################################

str(house2$BIKE)
house2$BIKE <- as.factor(house2$BIKE)

table(house2$BIKE)

house2$BIKE <- dplyr::recode(house2$BIKE, "01" = "Daily", "02" = "Few times a week", "03" = "Few times a month", "04" = "Few times a year", "05" = "Never", "-9" = "Other")
table(house2$BIKE)

round((table(house2$BIKE)/nrow(house2)*100), digits = 0)

###### Univariate ##############
barplot(table(house2$BIKE), xlab = "Frequency of Bike rides", ylab = "Counts", ylim = c(0, 3500))

barplot(round((table(house2$BIKE)/nrow(house2)*100), digits = 2), xlab = "Frequency of Bike rides", ylab = "Percent Frequency", ylim = c(0, 80))

###### Bivariate ##############
plot(house2$BIKE, sqrt(house2$TRPMILES), ylab = "Square Root of Trip miles", xlab = "Frequency of Bike rides")

boxplot(sqrt(house2$TRPMILES)~house2$BIKE, data=house2, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby frequency of Bike rides",ylab = " Square Root of  Trip miles", xlab = "Frequency of Bike rides") # \n is a new line

######## BIKE Anova ################

aov_bike <- aov(sqrt(TRPMILES)~BIKE, house2)
summary(aov_bike) #Statistically Significant

######## Linear Model ########

lm_bike <- lm(sqrt(TRPMILES)~BIKE, house2)
summary(lm_bike)

################################# Analysis of CAR ##############################################

str(house2$CAR)
house2$CAR <- as.factor(house2$CAR)

table(house2$CAR)

house2$CAR <- dplyr::recode(house2$CAR, "01" = "Daily", "02" = "Few times a week", "03" = "Few times a month", "04" = "Few times a year", "05" = "Never", "-1" = "Other")
table(house2$CAR)

round((table(house2$CAR)/nrow(house2)*100), digits = 0)
#80% of households use Car as mode of transportation Daily!

###### Univariate ##############
barplot(table(house2$CAR), xlab = "Frequency of Car rides", ylab = "Counts", ylim = c(0, 4000))

barplot(round((table(house2$CAR)/nrow(house2)*100), digits = 2), xlab = "Frequency of Car rides", ylab = "Percent Frequency", ylim = c(0, 100))

###### Bivariate ##############
plot(house2$CAR, sqrt(house2$TRPMILES), ylab = "Square Root of  Trip miles", xlab = "Frequency of Car rides")

boxplot(sqrt(house2$TRPMILES)~house2$CAR, data=house2, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby frequency of Car rides",ylab = "Square Root of  Trip miles", xlab = "Frequency of Car rides") # \n is a new line

######## CAR Anova ################

aov_car <- aov(sqrt(TRPMILES)~CAR, house2)
summary(aov_car) #Statistically Significant

######## Linear Model ########

lm_car <- lm(sqrt(TRPMILES)~CAR, house2)
summary(lm_car)

################################# Analysis of TAXI ##############################################

str(house2$TAXI)
house2$TAXI <- as.factor(house2$TAXI)

table(house2$TAXI)

house2$TAXI <- dplyr::recode(house2$TAXI, "01" = "Daily", "02" = "Few times a week", "03" = "Few times a month", "04" = "Few times a year", "05" = "Never", "-7" = "Other", "-9" = "Other")
table(house2$TAXI)

round((table(house2$TAXI)/nrow(house2)*100), digits = 0)

###### Univariate ##############
barplot(table(house2$TAXI), xlab = "Frequency of Taxi rides", ylab = "Counts", ylim = c(0, 3500))

barplot(round((table(house2$TAXI)/nrow(house2)*100), digits = 2), xlab = "Frequency of Taxi rides", ylab = "Percent Frequency", ylim = c(0, 80))

###### Bivariate ##############
plot(house2$TAXI, sqrt(house2$TRPMILES), ylab = "Square Root of Trip miles", xlab = "Frequency of Taxi rides")

boxplot(sqrt(house2$TRPMILES)~house2$TAXI, data=house2, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby frequency of Taxi rides",ylab = "Square Root of  Trip miles", xlab = "Frequency of Taxi rides") # \n is a new line

######## TAXI Anova ################

aov_taxi <- aov(sqrt(TRPMILES)~TAXI, house2)
summary(aov_taxi) #Staistically Significant

######## Linear Model ########

lm_taxi <- lm(sqrt(TRPMILES)~TAXI, house2)
summary(lm_taxi)

################################# Analysis of BUS ##############################################

str(house2$BUS)
house2$BUS <- as.factor(house2$BUS)

table(house2$BUS)

house2$BUS <- dplyr::recode(house2$BUS, "01" = "Daily", "02" = "Few times a week", "03" = "Few times a month", "04" = "Few times a year", "05" = "Never", "-7" = "Other", "-9" = "Other")
table(house2$BUS)

round((table(house2$BUS)/nrow(house2)*100), digits = 0)

###### Univariate ##############
barplot(table(house2$BUS), xlab = "Frequency of Bus rides", ylab = "Counts", ylim = c(0, 4000))

barplot(round((table(house2$BUS)/nrow(house2)*100), digits = 2), xlab = "Frequency of Bus rides", ylab = "Percent Frequency", ylim = c(0, 80))

###### Bivariate ##############
plot(house2$BUS, sqrt(house2$TRPMILES), ylab = "Square Root of  Trip miles", xlab = "Frequency of Bus rides")

boxplot(sqrt(house2$TRPMILES)~house2$BUS, data=house2, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby frequency of Bus rides",ylab = "Square Root of  Trip miles", xlab = "Frequency of Bus rides") # \n is a new line

########BUS Anova ################

aov_bus <- aov(sqrt(TRPMILES)~BUS, house2)
summary(aov_bus) #Statistically Significant

######## Linear Model ########

lm_bus <- lm(sqrt(TRPMILES)~BUS, house2)
summary(lm_bus)

################################# Analysis of TRAIN ##############################################

str(house2$TRAIN)
house2$TRAIN <- as.factor(house2$TRAIN)

table(house2$TRAIN)

house2$TRAIN <- dplyr::recode(house2$TRAIN, "01" = "Daily", "02" = "Few times a week", "03" = "Few times a month", "04" = "Few times a year", "05" = "Never", "-7" = "Other", "-9" = "Other")
table(house2$TRAIN)

round((table(house2$TRAIN)/nrow(house2)*100), digits = 0)

###### Univariate ##############
barplot(table(house2$TRAIN), xlab = "Frequency of Train rides", ylab = "Counts", ylim = c(0, 4000))

barplot(round((table(house2$TRAIN)/nrow(house2)*100), digits = 2), xlab = "Frequency of Train rides", ylab = "Percent Frequency", ylim = c(0, 80))

###### Bivariate ##############
plot(house2$TRAIN, sqrt(house2$TRPMILES), ylab = "Square Root of  Trip miles", xlab = "Frequency of Train rides")

boxplot(sqrt(house2$TRPMILES)~house2$TRAIN, data=house2, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby frequency of Train rides",ylab = "Square Root of  Trip miles", xlab = "Frequency of Train rides") # \n is a new line

######## Anova ################

aov_train <- aov(sqrt(TRPMILES)~TRAIN, house2)
summary(aov_train) #Statistically Significant

######## Linear Model ########

lm_train <- lm(sqrt(TRPMILES)~TRAIN, house2)
summary(lm_train)

################################# Analysis of PARA ##############################################

str(house2$PARA)
house2$PARA <- as.factor(house2$PARA)

table(house2$PARA)

house2$PARA <- dplyr::recode(house2$PARA, "01" = "Daily", "02" = "Few times a week", "03" = "Few times a month", "04" = "Few times a year", "05" = "Never", "-7" = "Other", "-9" = "Other")
table(house2$PARA)

round((table(house2$PARA)/nrow(house2)*100), digits = 0)

###### Univariate ##############
barplot(table(house2$PARA), xlab = "Frequency of Paratransit Use", ylab = "Counts", ylim = c(0, 4500))

barplot(round((table(house2$PARA)/nrow(house2)*100), digits = 2), xlab = "Frequency of Paratransit Use", ylab = "Percent Frequency", ylim = c(0, 80))

###### Bivariate ##############
plot(house2$PARA, sqrt(house2$TRPMILES), ylab = "Square Root of  Trip miles", xlab = "Frequency of Paratransit Use")

boxplot(sqrt(house2$TRPMILES)~house2$PARA, data=house2, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of Trip miles\nby frequency of Paratransit Use",ylab = "Square Root of  Trip miles", xlab = "Frequency of Paratransit Use") # \n is a new line

######## Anova ################

aov_para <- aov(sqrt(TRPMILES)~PARA, house2)
summary(aov_para) #Statistically Significant

######## Linear Model ########

lm_para <- lm(sqrt(TRPMILES)~PARA, house2)
summary(lm_para)

############################# Analysis of Walk2Save #######################################

table(house2$walk2save_coded)
str(house2$walk2save_coded)

house2$walk2save_coded <- factor(house2$walk2save_coded, levels = c("Agree", "Disagree", "Other"))
data6 <- house2 %>% count(walk2save_coded) %>%  mutate(perc = (n / nrow(house2)) *100)

#Plotting the percent data
walk2_bar <-  ggplot(data=data6, aes(x=walk2save_coded, y=perc )) + 
  geom_bar(fill="steelblue", stat = "identity") + theme_minimal()+
  labs(title='People who walk to save')
walk2_bar + ylim(0,75) +  xlab("Walk to Save") + ylab("Percentage")


ggplot(house2, aes(x=walk2save_coded, y=(sqrt(TRPMILES)), color=walk2save_coded)) + 
  geom_boxplot()+
  labs(title='Distribution of Trip Miles by people whi walk to save', x = "Walk to Save", y = "Square root of Trip Miles")

aov_walk2save <- aov(sqrt(TRPMILES)~walk2save_coded, house2)
summary(aov_walk2save)

################################## Analysis of BIKE2SAVE #####################################

table(house2$bike2save_coded)
str(house2$bike2save_coded)

house2$bike2save_coded <- factor(house2$bike2save_coded, levels = c("Agree", "Disagree", "Other"))

data7 <- house2 %>% count(bike2save_coded) %>%  mutate(perc = (n / nrow(house2)) *100)

#Plotting the percent data
bike2_bar <-  ggplot(data=data7, aes(x=bike2save_coded, y=perc )) + 
  geom_bar(fill="steelblue", stat = "identity") + theme_minimal()+
  labs(title='People who bike to save')
bike2_bar + ylim(0,100) +  xlab("Bike to Save") + ylab("Percentage")


ggplot(house2, aes(x=bike2save_coded, y=(sqrt(TRPMILES)), color=bike2save_coded)) + 
  geom_boxplot()+
  labs(title='Distribution of Trip Miles by people who bike to save', x = "Bike to Save", y = "Square root of Trip Miles")

aov_bike2save <- aov(sqrt(TRPMILES)~bike2save_coded, house2)
summary(aov_bike2save)

########################### Merging house1 & house2 #################################################

## Merging the two subsets of data to create a new dataset to check for better model. 

house3 <- merge(house1,house2, by.x="HOUSEID", by.y="HOUSEID")
dim(house3)

names(house3)

house4 <- house3[,-c(1, 2, 3, 9, 16, 24, 25, 26)]
dim(house4)
names(house4)

lm_m1 <- lm(sqrt(TRPMILES.y)~., data = house4)
summary(lm_m1)

par(mfrow = c(2,2))
lm_m2 <- lm(sqrt(TRPMILES.y)~HHSIZE+HHVEHCNT+DRVRCNT+WRKCOUNT+URBRUR+TRAVDAY+HHFAMINC, data = house4)
summary(lm_m2)
plot(lm_m2)

# This model gives us the highest R-squared value of 23.42% 
# as compared to the earlier models.

# Model interpretation:
# On an average, when the household size is "3 & above" the Square root 
# of trip miles traveled by by the household is 1.58 miles higher as compared 
# to households of size 1.

# On an average, when the count of vehicles is "3 & above" the Square root 
# of trip miles traveled by by the household is 1.06 miles higher as compared 
# to households with vehicle count of 1.

# On an average, when the count of vehicles is "3 & above" the Square root 
# of trip miles traveled by by the household is 1.06 miles higher as compared 
# to households with vehicle count of 1.

# On an average, for a household belongig to the Upper income bracket the Square 
# root of trip miles traveled by the household is 0.86 miles higher as compared 
# to households belonging to the Lower income bracket.

# On an average, for every household located in the Rural area the Square 
# root of trip miles traveled by the household is 0.86 miles higher as compared 
# to household located in Urban areas.

table(house4$WRKCOUNT)

################## Interaction Effects ###########################################################

class(house4$WRKCOUNT)
lm_m3 <- lm(sqrt(TRPMILES.y)~HHSIZE+HHVEHCNT+DRVRCNT+WRKCOUNT+URBRUR+TRAVDAY+HHFAMINC+HHSIZE:HHVEHCNT, data = house4)
summary(lm_m3)
plot(effect(term="HHSIZE:HHVEHCNT", mod=lm_m3, default.levels=20), multiline=T)

#############################US Map for TRIPMILES###############

dim(hhdata)

#Finding the TripMiles details per State and creating a dataframe
HH_STATETRPMILES <- aggregate(x = house3$TRPMILES.x, 
                              by = list(unique.values = house3$HHSTATE), 
                              FUN = sum)

HH_STATETRPMILES[order(HH_STATETRPMILES$x),]
dim(HH_STATETRPMILES)


#Creating the state name as dataset only has code values
HH_STATETRPMILES$region <- "0"

HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="AL"] <- "alabama"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="AK"] <- "alaska"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="AZ"] <- "arizona"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="AR"] <- "arkansas"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="CA"] <- "california"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="CO"] <- "colorado"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="CT"] <- "connecticut"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="DE"] <- "delaware"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="FL"] <- "florida"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="GA"] <- "georgia"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="HI"] <- "hawaii"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="ID"] <- "idaho"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="IL"] <- "illinois"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="IN"] <- "indiana"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="IA"] <- "iowa"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="KS"] <- "kansas"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="KY"] <- "kentucky"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="LA"] <- "louisiana"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="ME"] <- "maine"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="MD"] <- "maryland"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="MA"] <- "massachusetts"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="MI"] <- "michigan"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="MN"] <- "minnesota"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="MS"] <- "mississippi"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="MO"] <- "missouri"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="MT"] <- "montana"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="NE"] <- "nebraska"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="NV"] <- "nevada"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="NH"] <- "new hampshire"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="NJ"] <- "new jersey"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="NM"] <- "new mexico"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="NY"] <- "new york"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="NC"] <- "north carolina"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="ND"] <- "north dakota"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="OH"] <- "ohio"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="OK"] <- "oklahoma"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="OR"] <- "oregon"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="PA"] <- "pennsylvania"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="RI"] <- "rhode island"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="SC"] <- "south carolina"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="SD"] <- "south dakota"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="TN"] <- "tennessee"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="TX"] <- "texas"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="UT"] <- "utah"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="VT"] <- "vermont"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="VA"] <- "virginia"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="WA"] <- "washington"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="WV"] <- "west virginia"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="WI"] <- "wisconsin"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="WY"] <- "wyoming"
HH_STATETRPMILES$region[HH_STATETRPMILES$unique.values=="DC"] <- "district of columbia"



all_states <- map_data("state") #Using mapdata function to use 
head(all_states)
View(table(all_states$region))

Total <- merge(all_states, HH_STATETRPMILES , by="region")



head(Total)

#Plotting the TripMiles data to US State Map
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$x),colour="white"
) + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")  

P1 <- p + theme_bw()  + labs(fill = "Trip Miles" 
                             ,title = "NHTS 2017 - Trip Miles By State", x="", y="")

P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())

save.image(file = "finalproject.RData")
