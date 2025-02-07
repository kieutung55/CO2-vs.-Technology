library(caret)
library(readr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(randomForest)
library(glmnet)
library(dplyr)
library(factoextra)
library(e1071)
library(class)
#Load the data
data <- read.csv("1af2a710-9d43-41b6-a1fd-a9cc9e60a778_Data.csv", header = TRUE)
View(data)

#=============Clean, Preprocessing the data==================
#Remove unnecessary columns
data <- data[, -c(2, 4)]

#Rename the columns
names(data) <- c("Country", "Series.Name", "Year.2022","Year.2021","Year.2020","Year.2019",
                 "Year.2018","Year.2017","Year.2016","Year.2015","Year.2014","Year.2013",
                 "Year.2012","Year.2011","Year.2010","Year.2009","Year.2008","Year.2007",
                 "Year.2006","Year.2005","Year.2004","Year.2003","Year.2002", "Year.2001",
                 "Year.2000")
View(data)
# Convert the "Country Name" column to factor
data$Country<- as.factor(data$Country)

# Convert the numeric columns to numeric data types
data[, 3:25] <- apply(data[, 3:25], 2, as.numeric)

# Calculate the row means
row_means <- rowMeans(data[, 3:25], na.rm = TRUE)

# Replace missing values with row means
for (i in 3:25) {
  data[is.na(data[, i]), i] <- row_means[is.na(data[, i])]
}
# Replace missing values represented by "NaN" with NA
data[data == "NaN"] <- NA
#Replace NA values for attributes have NA records to 0
# Identify rows with NA values, excluding first two columns
na_rows <- apply(data[, -(1:2)], 1, function(x) any(is.na(x)))
# Replace NA rows with 0, excluding first two columns
data[na_rows, -(1:2)] <- 0

write.csv(data, file = "Eurasia.csv", row.names = FALSE)

#create response variable for co2 & define pca
pca <- data

co2_mean_value <- mean(pca$`CO2 emissions (kt)`, na.rm = TRUE)
co2_mean <- ifelse(pca$`CO2 emissions (kt)` > co2_mean_value, 1, 0)

#==========Reshape the dataset===========
# Read the CSV file
data <- read_csv("Eurasia.csv")
# Reshape the dataset
data <- pivot_longer(data, 
                     cols = starts_with("Year"), 
                     names_to = "Year",
                     values_to = "Value")
data$Value <- as.numeric(as.character(data$Value))

# Reshape the data and calculate the mean value for each combination of "Country", "Year", and "Series.Name"
data <- dcast(data, Country + Year ~ Series.Name, value.var = "Value", fun.aggregate = mean)

# Move the column names to the first row
data <- rbind(colnames(data), data)

# Delete the first line
data <- data[-1, -40 ]
data[, 3:39] <- apply(data[, 3:39], 2, as.numeric)
# Convert the "Year" column to numeric
data$Year <- as.numeric(gsub("Year\\.", "", data$Year))
data <- na.omit(data)  # Remove rows with any NAs
write.csv(data, "Eurasia.csv", row.names = FALSE)

pca <- data#used further down for PCA

#create response variable for co2 
co2_mean_value <- mean(pca$`CO2 emissions (kt)`, na.rm = TRUE)
co2_mean <- ifelse(pca$`CO2 emissions (kt)` > co2_mean_value, 1, 0)

#============EDA=============
ggplot(data, aes(x = `Electricity production from coal sources (% of total)`, y = co2_mean, color = Country)) +
  geom_point() +
  labs(x = "Electricity production from coal sources (% of total)", y = "High or low emission (1 or 0)", 
       title = "Electricity production from coal sources vs CO2 emission")

ggplot(data, aes(x = `Electricity production from natural gas sources (% of total)`, y = co2_mean, color = Country)) +
  geom_point() +
  labs(x = "Electricity production from natural gas sources (% of total)", y = "High or low emission (1 or 0)", 
       title = "Electricity production from natural gas sources vs CO2 emission")

ggplot(data, aes(x = `Electricity production from oil sources (% of total)`, y = co2_mean, color = Country)) +
  geom_point() +
  labs(x = "Electricity production from oil sources (% of total)", y = "High or low emission (1 or 0)", 
       title = "Electricity production from oil sources vs CO2 emission")

ggplot(data, aes(x = `Electricity production from oil, gas and coal sources (% of total)`, y = co2_mean, color = Country)) +
  geom_point() +
  labs(x = "Electricity production from oil, gas and coal sources (% of total)", y = "High or low emission (1 or 0)", 
       title = "Electricity production from oil, gas and coal sources vs CO2 emission")

ggplot(data, aes(x = `Electricity production from hydroelectric sources (% of total)`, y = co2_mean, color = Country)) +
  geom_point() +
  labs(x = "Electricity production from hydroelectric sources (% of total)", y = "High or low emission (1 or 0)", 
       title = "Electricity production from hydroelectric sources (% of total) vs CO2 emission")

ggplot(data, aes(x = `Electricity production from nuclear sources (% of total)`, y = co2_mean, color = Country)) +
  geom_point() +
  labs(x = "Electricity production from nuclear sources (% of total)", y = "High or low emission (1 or 0)", 
       title = "Electricity production from nuclear sources vs CO2 emission")

ggplot(data, aes(x = `Fixed broadband subscriptions`, y = co2_mean, color = Country)) +
  geom_point() +
  labs(x = "Fixed broadband subscriptions", y = "High or low emission (1 or 0)", 
       title = "Fixed broadband subscriptions vs CO2 emission")

ggplot(data, aes(x = `Fixed telephone subscriptions`, y = co2_mean, color = Country)) +
  geom_point() +
  labs(x = "Fixed telephone subscriptions", y = "High or low emission (1 or 0)", 
       title = "Fixed telephone subscriptions vs CO2 emission")

ggplot(data, aes(x = `Mobile cellular subscriptions`, y = co2_mean, color = Country)) +
  geom_point() +
  labs(x = "Mobile cellular subscriptions", y = "High or low emission (1 or 0)", 
       title = "Mobile cellular subscriptions vs CO2 emission")

#============PCA Pre-Processing for electricity production and technology==========
# Select the variables of interest
ele_coal <- pca$`Electricity production from coal sources (% of total)`
ele_gas<-pca$`Electricity production from natural gas sources (% of total)`
ele_oil<-pca$`Electricity production from oil sources (% of total)`
ele_cgo<-pca$`Electricity production from oil, gas and coal sources (% of total)`
ele_hydro<- pca$`Electricity production from hydroelectric sources (% of total)`
ele_nucl<- pca$`Electricity production from nuclear sources (% of total)`
tech_broad <- pca$`Fixed broadband subscriptions`
tech_tele <- pca$`Fixed telephone subscriptions`
tech_mobile <- pca$`Mobile cellular subscriptions`

techEnergy <-data.frame(ele_cgo,ele_coal, ele_gas, ele_oil, ele_nucl, ele_hydro, tech_broad,tech_tele,tech_mobile)

# Perform PCA on the combined data
pca_result <- prcomp(techEnergy, scale = TRUE)

# List of access the PCA results
summary(pca_result)
pca_result$x
pc1 <- pca_result$x[, 1]
pc2 <- pca_result$x[, 2]
techEnergy_pca <- as.data.frame(pca_result$x)

ggplot(as.data.frame(pca_result$x), aes(x = pc1, y = pc2)) +
  geom_point() +
  labs(x = sprintf("Principal Component 1 (%.2f%% variance)", 100 * pca_result$sdev[1]^2 / sum(pca_result$sdev^2)),
       y = sprintf("Principal Component 2 (%.2f%% variance)", 100 * pca_result$sdev[2]^2 / sum(pca_result$sdev^2)),
       title = "PCA of electricity production and technology use") +
  theme_minimal()

#======Function to plot SVM decision boundaries========
plot_svm_decision_boundary_all_pcs <- function(data, svm_model, title) {
  # Calculate mean values for PCs 3 to 9
  mean_values <- apply(data[, 3:9], 2, mean, na.rm = TRUE)
  
  # Create a grid over the space of the first two principal components
  grid <- expand.grid(
    PC1 = seq(min(data$PC1), max(data$PC1), length.out = 100),
    PC2 = seq(min(data$PC2), max(data$PC2), length.out = 100)
  )
  
  # Repeat mean values for each point in the grid
  for (i in 3:9) {
    grid[, paste0("PC", i)] <- mean_values[i - 2]
  }
  
  # Predict the class for each point in the grid
  grid$pred <- predict(svm_model, newdata = grid)
  
  # Plot the decision boundary
  plt <- ggplot() +
    geom_tile(data = grid, aes(x = PC1, y = PC2, fill = factor(pred)), alpha = 0.3) +
    scale_fill_manual(values = c("#00BFFF", "#FF6347", "#90EE90"), name = "Prediction Zone") +  # Vibrant colors
    geom_point(data = data, aes(x = PC1, y = PC2, color = factor(y), shape = factor(y)), size = 4, fill = "white") +
    scale_shape_manual(values = c(21, 22, 23), name = "Category Shape") +
    scale_color_brewer(palette = "Set1", name = "Actual Category") +
    geom_contour(data = grid, aes(x = PC1, y = PC2, z = as.numeric(pred)), color = "grey", linewidth = 0.4, alpha= 0.8) +
    labs(x = "Principal Component 1", y = "Principal Component 2", title = title, subtitle = "SVM Decision Boundary with all 9 PCs") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16),
      legend.position = "right",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(plt)
}

#=============SVM for electricity production and technology================
co2_techEnergy <- data.frame(y=as.factor(co2_mean), techEnergy_pca)

set.seed(1)
n <- nrow(co2_techEnergy)
train <- sample(1:n, n*0.8)
test <- setdiff(1:n, n*0.8)

tune.out=tune(METHOD=svm,
              y~., data=co2_techEnergy[train,],
              kernel="radial",
              ranges=list(cost=c(0.001,0.01,0.1,1,10,100),
                          gamma = c(1,2,3,4,5,6,7,8)))
summary(tune.out)
y=as.factor(co2_techEnergy$y)
svmfit=svm(y~., 
           data=co2_techEnergy[train,],
           cost=10, gamma=1,
           kernel="radial",
           scale=TRUE)
summary(svmfit)

svm.pred <- predict(svmfit, co2_techEnergy[test,])
table (true = co2_techEnergy[test,"y"],
       pred = svm.pred)

#plot radial svm model
plot_svm_decision_boundary_all_pcs(co2_techEnergy, svmfit, "SVM Radial Model for Tech and Electricity")

#=============KNN for technology================
set.seed(1)
pca_5 = data.frame(pca_result$x[, 1], pca_result$x[, 2], pca_result$x[, 3], pca_result$x[, 4], pca_result$x[, 5])

sample <- sample(c(TRUE, FALSE), nrow(data.frame(co2_mean)), replace=TRUE, prob=c(0.8,0.2))
ele.train = data.frame(pca_5)[!sample, ]
ele.test = data.frame(pca_5)[sample, ]

co2.cl = data.frame(co2_mean)[!sample, ]
co2.cl2 = data.frame(co2_mean)[sample, ]
co2.cl = as.factor(co2.cl)

best_k = -1
err_k = 1000.0
for (i in 1:25){
  ele.knn = knn(train = data.frame(ele.train), test = data.frame(ele.test), cl= as.factor(co2.cl), k=i)
  m = mean(co2.cl2 != ele.knn)
  cat("k =", i, "Error =", m, "\n")
  
  if(m < err_k){
    best_k = i
    err_k = m
  }
  #print(i)
  #print(m)
}
print(best_k)
print(err_k)

ele.knn = knn(train = data.frame(ele.train), test = data.frame(ele.test), cl= as.factor(co2.cl), k=10)
mean(co2.cl2!= ele.knn)
table(true = co2.cl2, pred = ele.knn)

#=============SVM for whole dataset================
#fitting best SVM model with full data set
set.seed(1)
tune.out=tune(METHOD=svm,
              y~., data=co2_techEnergy,
              kernel="radial",
              ranges=list(cost=c(0.001,0.01,0.1,1,10,100), 
                          gamma = c(1,2,3,4,5,6,8)))
summary(tune.out)
svmfit=svm(y~., 
           data=co2_techEnergy,
           cost=10, gamma=1,
           kernel="radial",
           scale=TRUE)
summary(svmfit)

svm.pred <- predict(svmfit, co2_techEnergy)
table (true = co2_techEnergy$y,
       pred = svm.pred)

#plot radial svm model
plot_svm_decision_boundary_all_pcs(co2_techEnergy, svmfit, "SVM Radial Model for Tech and Electricity")
