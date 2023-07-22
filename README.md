# R_programming
Complete our programming concepts with programmes

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

