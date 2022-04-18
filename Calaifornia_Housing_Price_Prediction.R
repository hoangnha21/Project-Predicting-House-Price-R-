#a.Load the dataset into R and check the summary of data
data <- read.csv(file.choose())
head(data)
summary(data)


#b. Cleaning the data
#use the graphs for finding outliers
library(ggplot2)
theme_set
install.packages("DataExplorer")
DataExplorer::plot_intro(data)
# Most of the data is discrete
#vcolumns that have missing data:
install.packages("dplyr")
library(dplyr)
p <- DataExplorer::plot_missing(data, missing_only = T)
cols_to_remove <- p$data %>% 
  filter(Band %in% c("Remove", "Bad")) %>% pull(feature)
##remove the missing from the data
data <- filter(data, !is.na(total_bedrooms))
#]outliers in the data:
x11()
boxplot(data[1:8],ylim = c(0, 10000),boxwex = 0.5, sep = ":",horizontal = TRUE, col = c("red", "blue","yellow"),main = "Box Plot for the data to detect Outliers",
        xlab = "Frequency",ylab = "Variables",xaxt = "n", yaxt = "n")
text(y = 1:length(data),
     ## Move the labels down by 0.45.
     x = par("usr")[3] - 0.45,
     labels = names(data),xpd = NA,srt = 35,
     cex = 1, adj = 1.2)


#c.ggplot() function
install.packages("ggplot2")
library(ggplot2)
ggplot(data =data, aes(x =longitude, y=latitude, fill= median_house_value))+geom_point(alpha=0.4) +aes(size=population)+
  labs(title ="Dots Plot for Latitude & Longnitude WRT Population",x = "Longnitude",y = "Latitude")
#d histograms for numeric variables
p2 <- ggplot(data = data, aes(x=longitude)) + geom_histogram(color='blue')+theme_bw()
p3 <- ggplot(data = data, aes(x=latitude)) + geom_histogram(color='darkblue')+theme_bw()
p4 <- ggplot(data = data, aes(x=housing_median_age)) + geom_histogram(color='darkgreen')+theme_bw()
p5 <- ggplot(data = data, aes(x=total_rooms)) + geom_histogram(color='darkred')+theme_bw()
p6 <- ggplot(data = data, aes(x=total_bedrooms)) + geom_histogram(color='red')+theme_bw()
p7 <- ggplot(data = data, aes(x=population)) + geom_histogram(color='yellow')+theme_bw()
p8 <- ggplot(data = data, aes(x=households)) + geom_histogram(color='green')+theme_bw()
p9 <- ggplot(data = data, aes(x=median_income)) + geom_histogram(color='darkred')+theme_bw()
p10 <- ggplot(data = data, aes(x=median_house_value)) + geom_histogram(color='blue')+theme_bw()
library(gridExtra)
p <- grid.arrange(p2,p3,p4,p5,p6,p7,p8,p9,p10, nrow = 3, ncol = 3, top = "Histogram for the all Numerical Variables")

#e and f: correlation between the data
x11()
pairs(data[1:9],                     # Data frame of variables
      labels = colnames(data[1:9]),  # Variable names
      pch = 21,                 # Pch symbol
      bg = rainbow(3),  # Background color of the symbol (pch 21 to 25)
      col = rainbow(3), # Border color of the symbol
      main = "Iris dataset",    # Title of the plot
      row1attop = TRUE,         # If FALSE, changes the direction of the diagonal
      gap = 1,                  # Distance between subplots
      cex.labels = NULL,        # Size of the diagonal text
      font.labels = 1)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
x11()
chart.Correlation(data[1:9], histogram = TRUE, method = "pearson")

#g partition of the data
# Dataset splitting: training and test set.
set.seed(309)
idTrain <- sample(nrow(data),floor(0.80*nrow(data)))
train_data <- data[idTrain,]
test_data <- data[-idTrain,]


#h perform the linear model
mod <- lm(median_house_value ~ housing_median_age + total_rooms+total_bedrooms+median_income
          +ocean_proximity, train_data)
summary(mod)

#j use the forward stepwise selection of variable
install.packages("olsrr")
library(olsrr)
ft <- ols_step_forward_p(mod, details = TRUE)
aic <- ols_step_forward_aic(mod)
plot(aic)

#k perform analysis for the test data

mod1 <- lm(median_house_value ~ housing_median_age + total_rooms+total_bedrooms+median_income
           +ocean_proximity, test_data)
summary(mod1)

#use the forward stepwise selection of variable
ft1 <- ols_step_forward_p(mod1, details = TRUE)
aic1 <- ols_step_forward_aic(mod1)
plot(aic1)

#l Decision Tree for Regression for train data
install.packages("rpart")
library(rpart)
tree <- rpart(median_house_value ~ housing_median_age + total_rooms+total_bedrooms+median_income
              +ocean_proximity,method = "anova", data = train_data)
#plot the tree
png(file = "decTreeGFG.png", width = 600, 
    height = 600)
# Plot
plot(tree, uniform = TRUE,main = "House Price Decision Tree using Regression")
text(tree, use.n = TRUE, cex = .7)
# Saving the file
dev.off()
print(tree)

#n Decision Tree for Regression for test data
tree <- rpart(median_house_value ~ housing_median_age + total_rooms+total_bedrooms+median_income
              +ocean_proximity,method = "anova", data = test_data)
#plot the tree
png(file = "decTreeGFG.png", width = 600, 
    height = 600)
# Plot
plot(tree, uniform = TRUE,main = "House Price Decision Tree using Regression")
text(tree, use.n = TRUE, cex = .7)
# Saving the file
dev.off()
print(tree)
