#Load .csv file
cars = read.csv(file.choose())

#Check whether the dataset is properly loaded
head(cars)
tail(cars)
dim(cars)

#Install dplyr packages
install.packages("dplyr")
library(dplyr)

#Data Manipulation
carn$Make = as.character(carn$Make)
carn$Make = ifelse(carn$Make == "", yes = "undefined", no = carn$Make)
sum(carn$Make == "undefined")

# Add new categorical independent variable
cars.features = data.frame(cars[,c(6,10,14)])
cars.features = scale(cars.features)
cars.features = round(cars.features, digits = 2)

cars.features = as.data.frame(cars.features)

SSE_curve <- c()
for (n in 1:15) {
  kcluster <- kmeans(cars.features, n)
  sse <- sum(kcluster$withinss)
  SSE_curve[n] <- sse
}

SSE_curve

print("SSE cuve for the ideal k value")
plot(1:15, SSE_curve, type="b", xlab="Number of Clusters", ylab="SSE")

#Old code
results = kmeans(cars.features, 4, nstart = 100)
cars = data.frame(cars, cluster = results$cluster)

# Add new independent variables
cars.features = mutate(cars, 
				Avg.mpg = (City.mpg + Highway.mpg)/2, 
				RPM = (Horsepower * 5252)/Torque, 
				Volume = (Length * Width * Height)/0.000001, 
				Watts = Horsepower * 745.7
				)

#Dropping categorical value, to ease clustering 
cars.features$Classification = NULL
cars.features$Driveline = NULL
cars.features$Engine.Type = NULL
cars.features$Fuel.Type = NULL
cars.features$Hybrid = NULL
cars.features$Make = NULL

cars.features = subset(cars.features, select = -c(Classification, Driveline, Engine.Type, Fuel.Type, Hybrid, Make))

#Standardizing, if necessary
cars.features = scale(cars.features)

#Clustering, with respect with the predefined variable
results = kmeans(cars.features, 4, nstart = 100)
results$cluster
results$centers
results$size

#Constructing table to observe clustering 
table(cars$cluster, results$cluster)

#Charts and Graphs
plot(cars[c("City.mpg", "Highway.mpg")], col = results$cluster)
plot(cars[c("Horsepower", "Torque")], col = results$cluster)

#Data Visualization
#3D Plot
install.packages("plotly")
library(plotly)
plot_ly(cars, x = ~City.mpg, y = ~Highway.mpg, z = ~Horsepower, color = results$cluster)

#Boxplot
install.packages("ggplot2")
library(ggplot2)
cars$cluster = as.factor(cars$cluster)
ggplot(cars, mapping= aes(x = cluster , y = City.mpg, color = cars$cluster)) + geom_boxplot()
cars$cluster = as.numeric(cars$cluster)

#Barplot
cluster_drive = data.frame(cars %>% group_by(cluster) %>% summarize(Make = n()))
ggplot(cluster_drive, mapping = aes(x=cluster, y=Make, fill = cluster)) + geom_bar(stat = "identity") + labs(x="Observations", y="Count", title="Number of Observations by Cluster")

# Seperate Data frame
df1 = filter(cars.features, cluster == 1)		
df2 = filter(cars.features, cluster == 2)
df3 = filter(cars.features, cluster == 3)
df4 = filter(cars.features, cluster == 4)

#Linear regression analysis
attach(df1)
model1 = lm(Avg.mpg~Horsepower+Volume)
summary(model1)

attach(df2)
model2 = lm(Avg.mpg~Horsepower+Volume)
summary(model2)

attach(df3)
model3 = lm(Avg.mpg~Horsepower+Volume)
summary(model3)

attach(df4)
model4 = lm(Avg.mpg~Horsepower+Volume)
summary(model4)