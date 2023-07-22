# R_programming
Complete our programming concepts with programmes

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

