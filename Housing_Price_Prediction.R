Data <- read.csv(file.choose())
Data$ocean_proximity <- as.factor(Data$ocean_proximity)
str(Data)
summary(Data)

# It can be observed that "ocean_proximity" is the only categorical variable in the data with five distinct values. 
# Among these, the number of observations where its value is "ISLAND" is just 5 which is very very less than the others.
# Also, there are 207 observations where the variable "total_bedrooms" has NA values.
#In the process of data cleaning, we need to take care of these issues.

Data$total_bedrooms[is.na(Data$total_bedrooms)] <- median(Data$total_bedrooms, na.rm=TRUE)
sum(is.na(Data))

summary(Data$ocean_proximity)

# As observed, the level "ISLAND" has just 5 records which is very low as compared to other levels which have more than 2000 records. We can eliminate this level from the data as there could be issues with model fitting.
library(ggplot2)
ggplot(Data, aes(x = factor(ocean_proximity))) +
  geom_bar(stat = "count", color = "black", fill = "blue")
Data <- Data[Data$ocean_proximity != "ISLAND", ]

nrow(Data)

# (c)

plot_map = ggplot(Data, 
                  aes(x = longitude, y = latitude, color = median_house_value)) +
  geom_point(aes(size = population), alpha = 0.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Data Map - Longtitude vs Latitude and Associated Variables") +
  scale_color_distiller(palette = "Paired") +
  labs(color = "Median House Value (in $USD)", size = "Population")
plot_map

# (d)
par(mfrow = c(3, 3))
hist(Data$longitude, main = "longitude", col="slateblue1")
hist(Data$latitude, main = "latitude", col="slateblue1")
hist(Data$housing_median_age, main = "housing_median_age", col="slateblue1")
hist(Data$total_rooms, main = "total_rooms", col="slateblue1")
hist(Data$total_bedrooms, main = "total_bedrooms", col="slateblue1")
hist(Data$population, main = "population", col="slateblue1")
hist(Data$households, main = "households", col="slateblue1")
hist(Data$median_income, main = "median_income", col="slateblue1")
hist(Data$median_house_value, main = "median_house_value", col="slateblue1")


# (e)
par(mfrow = c(1, 1))
pairs(Data, col = "slateblue1") 

# (f) 

corr_matrix <- round(cor(Data[, 1:9]), digits = 2)
corr_matrix


library(knitr) 
kable(t(corr_matrix))

'From this correlation matrix, it is observed that there is high correlation between "households" and "total_bedrooms", as well as "households" and "total_rooms". This can cause the problem of multicollinearity but since these can be influential in the pricing of homes, we decided to keep the covariates. We can deal with that, if required, using appropriate methods.'




# (g) Partition the dataset into train and test data
indx <- sample(2, nrow(Data), replace = T, prob = c(0.7, 0.3))
housing_training_data <- Data[indx == 1, ]
housing_test_data <- Data[indx == 2, ]
# (h) 
model1 = lm(median_house_value ~ ., data = housing_training_data)

# (i)

summary(model1) 
par(mfrow = c(2,2))
plot(model1)

'As you can see, two levels of the categorical variable, "ocean_proximityNEAR BAY" and "ocean_proximityNEAR OCEAN", are insignificant. We remove all the insignificant variables to create a new model.'
model2 <- lm(median_house_value ~ . - ocean_proximity, data = housing_training_data)
summary(model2)
par(mfrow = c(2,2))
plot(model2)
'Interpretation: As can be seen, the residual vs fitted value plot is slightly non-linear in nature. The hard capped line might be because one of the covariates has records with a constant value or the response vairable is linearly dependent on one or more covariates.
Also there are not influential outliers in this dataset'
# From the above plots, we notice that the LINE assumptions have been violated in this model. Hence, linear regression model may not be the best model to use for this dataset.
# (j) 
null <- lm(median_house_value ~1, data = housing_training_data)
full <- lm(median_house_value ~., data = housing_training_data)
step(null, scope = list(lower = null, upper = full),direction="both")

finalmodel <- lm(formula = median_house_value ~ median_income + ocean_proximity + 
                   housing_median_age + total_bedrooms + population + households + 
                   total_rooms + longitude + latitude, data = housing_training_data)
# (k)
pred <- predict(finalmodel, newdata = housing_test_data)
true <- housing_test_data$median_house_value
MSE <- mean((pred - true)^2)
MSE
'As can be seen the linear model does not perform very well (MSE is not small)'

# (l) 
library(rpart)
modeltree <- rpart(formula = median_house_value ~ ., data = housing_training_data) 

# (m)
library(rpart.plot)
rpart.plot(modeltree)
summary(modeltree)

'median_income and ocean_proximity are the most important variable considered in decision tree model'

print(modeltree)

# (n)
predtree <- predict(modeltree, newdata = housing_test_data)
MSE <- mean((predtree - true)^2)
MSE

# (o) 
# Both model do not perform well on this data set. But between these two we can pick the one with lower MSE.