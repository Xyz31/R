# R_programming
Complete our programming concepts with programmes

# DA LAB 1st Correlatation


```r

x <- women$height
y <- women$weight
# Plot with main and axis titles

plot(x, y, main = "Main title", xlab = "X axis Height", ylab = "Y axis Weight", pch = 19, frame =
       FALSE)

x<- mtcars$mpg
y<- mtcars$cyl

plot(x, y, main = "Main title", xlab = "X axis mpg", ylab = "Y axis cyl", pch = 19, frame =
       FALSE)


```


# DA LAB 2nd LungCapData BoxPlot


```r

LungCapData <- read.csv("C:/Users/Kashaf/Desktop/LungCap.csv")

LungCapData <- data.frame(LungCapData)

attach(LungCapData)
# Catgorise Age into groups
AgeGroups <- cut(LungCapData$Age,
                 breaks = c(0, 13, 15, 17, 25),
                 labels = c("<13", "14/15", "16/17", ">=18"))
head(LungCapData)
# BoxPlot 1
boxplot(LungCapData$LungCap~LungCapData$Smoke,
        ylab = "Capacity",
        main = "Lung Capacity of Smokers Vs Non-Smokers",
        las = 1)
# BoxPlot 2
boxplot(LungCapData$LungCap[LungCapData$Age>=18]~LungCapData$Smoke[LungCapData
                                                                   $Age>=18],
        ylab = "Capacity",
        main = "Lung Capacity of Smokers Vs Non-Smokers",
        las = 1)
# BoxPlot 3
boxplot(LungCapData$LungCap~LungCapData$Smoke*AgeGroups,
        ylab = "Capacity", xlab = "",
        main = "Lung Capacity of Smokers Vs Non-Smokers",
        col = c(4, 2), las = 2)

```


# DA LAB 3RD DATA CLEANING AIRQUALITY


```r

# Load the dataset
data("airquality")

airqual <- airquality

summary(airqual)

airqual[!complete.cases(airqual),]


boxplot(airqual)

# Boxplot of Ozone excluding outliers
boxplot(airqual$Ozone, horizontal = TRUE)

# Boxplot of Wind excluding outliers
boxplot(airqual$Wind, horizontal = TRUE)

# Remove outliers from the dataset
Updated_airqual <- subset(airqual, Ozone < 130 & Wind < 17)


boxplot(Updated_airqual)

# Impute missing values in Ozone and Solar.R with the mean of their respective variables
airqual$Ozone[is.na(airqual$Ozone)] <- mean(Updated_airqual$Ozone)
airqual$Solar.R[is.na(airqual$Solar.R)] <- mean(Updated_airqual$Solar.R, na.rm = TRUE)

summary(airqual)

# Remove outliers
data_airquality <- subset(airqual, Ozone < 70 & Wind < 17)

#updated dataset without outliers
boxplot(data_airquality)

data_airquality <- subset(data_airquality, Wind > 2)

boxplot(data_airquality)

nrow(data_airquality)

```

# DA LAB 4TH PCA ON FLOWERS


```r

data("iris") //Load the iris dataset
str(iris) //Compactly displaying the internal structure of a dataset
summary(iris) //The summary function returned descriptive statistics
set.seed(111) // function sets the starting number used to generate a sequence of random numbers
ind <- sample(2, nrow(iris),
replace = TRUE,
prob = c(0.8, 0.2))
training <- iris[ind==1,]
testing <- iris[ind==2,] //sample() function allows to take a random sample of elements from a
data-set
library(psych) // Scatter Plot & Correlations
// pairs.panels [in psych package] is used to create a scatter plot of matrices
pairs.panels(training[,-5],
gap = 0,
bg = c("red", "yellow", "blue")[training$Species],
pch=21)
pc <- prcomp(training[,-5],
center = TRUE,
scale. = TRUE ) //# Principal Component Analysis
attributes(pc)
pc$center
pc$scale
print(pc)
summary(pc)

```

# DA LAB 5TH DATA NORMALIZATION ON AGE AND EDU

```r

rm(list = ls(all.names = TRUE)) # Remove all objects in the present in the workspace

# Initialize the input data frame
x <- data.frame(
  name = c("Bala", "Ganesh", "Geevan"),
  age = c(43, 38, 42),
  education = c(2.0, 4.2, 4.1)
)

print(x)

# Extract age and education from data frame using $
age <- x$age
edu <- x$education

print(age)
print(edu)

# Form a Matrix M1
M1 <- matrix(c(age, edu), nrow = 3, byrow = FALSE)
print(M1)

# Call Euclidean distance and get E1
E1 <- dist(M1, method = 'euclidean')
print(E1)

# Age in years to Decade
ageD <- age / 10
print(ageD)

# Form a Matrix M2
M2 <- matrix(c(ageD, edu), nrow = 3, byrow = FALSE)
print(M2)

# Call Euclidean distance and get E2
E2 <- dist(M2, method = 'euclidean')
print(E2)

# Min-Max Normalize Age
rangA <- max(age) - min(age) # compute range
mi_maA <- (age - min(age)) / rangA
print(mi_maA)

# Normalize Education attribute
rangE <- max(edu) - min(edu)
mi_maE <- (edu - min(edu)) / rangE
print(mi_maE)

# Form a Matrix M3 with Normalized values
M3 <- matrix(c(mi_maA, mi_maE), nrow = 3, byrow = FALSE)
print(M3)

# Call Euclidean distance and get E3
E3 <- dist(M3, method = 'euclidean')
print(E3)

print("_______Data frame with different similarity measure matrix results______")
print(x)
print(E1)
print(E2)
print(E3)


```


# DA LAB 6TH DATA CONVERSION

```r

rm(list = ls(all.names = TRUE))

input = read.csv("filter.csv")
print(input)

x = cor(input[,c(2:7)])
print(x)

pairs(input[,c(2:7)])

y = x[6,1:5]
print(y)

x = sort(y, decreasing = TRUE)
print(x)

print("selected attributes")

cnt = 1
while(cnt <= length(y)) {
  if (y[cnt] > 0.5) {
    print(y[cnt])
  }
  cnt = cnt + 1
}


```


# DA LAB 7TH K-MEANS ALGO


```r

# Installing Packages
install.packages("ClusterR")
install.packages("cluster")

# Loading package
library(ClusterR)
library(cluster)

# Removing initial label of Species from original dataset
iris_1 <- iris[, -5]

# Fitting K-Means clustering Model to the training dataset
set.seed(240) # Setting seed
kmeans.re <- kmeans(iris_1, centers = 3, nstart = 20)

# Cluster identification for each observation
kmeans.re$cluster

# Confusion Matrix
cm <- table(iris$Species, kmeans.re$cluster)
cm

# Model Evaluation and visualization
plot(iris_1[c("Sepal.Length", "Sepal.Width")])
plot(iris_1[c("Sepal.Length", "Sepal.Width")], col = kmeans.re$cluster)
plot(iris_1[c("Sepal.Length", "Sepal.Width")], col = kmeans.re$cluster, main = "K-means with 3 clusters")

## Plotting cluster centers
kmeans.re$centers
kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")]
points(kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex = 3)

## Visualizing clusters
y_kmeans <- kmeans.re$cluster
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],
y_kmeans,
lines = 0,
shade = TRUE, color = TRUE,
labels = 2,
plotchar = FALSE,
span = TRUE,
main = "Cluster iris", xlab = 'Sepal.Length', ylab = 'Sepal.Width')


```
 
# DA LAB 10TH PROGRAM LINEAR REGRESSION 


```r

# Create the dataset
df <- data.frame(
  hours = c(1, 2, 4, 5, 5, 6, 6, 7, 8, 10, 11, 11, 12, 12, 14),
  score = c(64, 66, 76, 73, 74, 81, 83, 82, 80, 88, 84, 82, 91, 93, 89)
)

# View the first six rows
head(df)

# Attach the dataset
attach(df)

# Create a scatterplot 
plot(hours, score, main = 'Hours studied vs. Exam Score', xlab = 'Hours', ylab = 'Exam Score')

# Fit a simple linear regression model
model <- lm(score ~ hours)

# View the model summary
summary(model)

# Define the residuals of the model
res <- resid(model)

# Create a residual vs. fitted plot
plot(fitted(model), res, main = 'Residuals vs. Fitted', xlab = 'Fitted Values', ylab = 'Residuals')

# Add a horizontal line at 0 to help visualize the residuals' distribution around 0
abline(h = 0, col = 'red')

# Create a Q-Q plot (Quantile-Quantile plot) to check the normality of residuals
qqnorm(res)
qqline(res,col='red')  # Add a straight diagonal line to the Q-Q plot for comparison

# Detach the dataset to avoid conflicts with other data objects
detach(df)

```

