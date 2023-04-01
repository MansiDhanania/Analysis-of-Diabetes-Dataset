# Install Necessary Packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("missForest")
install.packages("mice")
install.packages("VIM")
install.packages("modeest")
install.packages("ggplot2")
install.packages("reshape2")
# Call Necessary Libraries
library("dplyr")
library("tidyr")
library("missForest")
library("mice")
library("VIM")
library("modeest")
library("ggplot2")
library("reshape2")

# Clear the environment
rm(list=ls())
# Call the data

data <- read.csv("Diabetes_Data.csv")
data

# Removing unnecessary columns
df <- select(data, c(-Outcome))
# Move the dependent variable to the right
df <- df%>%relocate(Glucose, .after = Age)
# Relocate Columns
df <- df%>%relocate(c(Insulin, BMI, DiabetesPedigreeFunction, Age), .before = SkinThickness)
df <- df%>%relocate(Pregnancies, .after = SkinThickness)
df

# Data Pre-processing
nrow(data) # no.of rows
ncol(data) # no.of columns
rownames(data) # row names
colnames(data) # column names
summary(data) # data summary
head(data, 5) # display first 5 rows
tail(data, 5) # display last 5 rows
str(data) # data type
distinct(data) # remove duplicate rows
summary(distinct(data)) # distinct data summary

# Descriptive Statistics
colMeans(df) # Mean of every column
apply(df, 2, median) # Median of every column
apply(df, 2, mfv) # Mode of every column
apply(df, 2, var) # Variance of every column
apply(df, 2, sd) # Standard Deviation of every column
apply(df, 2, range) # Range of every column
apply(df, 2, quantile) # Quantile of every column
apply(df, 2, IQR) # IQR (Interquartile Range) of every column

# Check for Null/Missing values
is.na(df)
# Since there are no missing/null characters, no need to mutate
# Mutate Null values with 0 Example
# df <- df %>% mutate(Pregnancies = replace(Pregnancies, Pregnancies == ""| Pregnancies == 
# "na"| Pregnancies == "NA"| Pregnancies == "n/A"| Pregnancies == "Nan"| Pregnancies == "n/a", 0))

# Data Imputation
# Sample data
df1 <- sample_n(df, 20)
# Generate Missing Values
df1.miss <- prodNA(df1, noNA = 0.4)
# Plot
md.pattern(df1.miss)
# Aggregate
aggr(df1.miss, col = mdc(2:5), numbers = TRUE, sortVars = TRUE, 
     labels = names(df1.miss), cex.axis = 0.9, gap = 3, ylab = c("Proportion", "Pattern"))
# Imputation
mice_imputes <- mice(df1.miss, m = 4, maxit = 20)
mice_imputes$method
df1.complete <- complete(mice_imputes, 2)
df1.complete

# Plots of individual labels
hist(df$Glucose, main = "Glucose Levels", xlab = "Glucose", col = "green", border = "black")
plot(df$Glucose, main = "Glucose Levels", xlab = "Glucose", col = "green")
hist(df$BloodPressure, main = "Blood Pressure Levels", xlab = "Blood Pressure", col = "pink", 
     border = "black")
plot(df$BloodPressure, main = "Blood Pressure Levels", xlab = "Blood Pressure", col = "pink")
hist(df$Insulin, main = "Insulin Levels", xlab = "Insulin", col = "blue", border = "black")
plot(df$Insulin, main = "Insulin Levels", xlab = "Insulin", col = "blue")
hist(df$BMI, main = "BMI", xlab = "BMI", col = "yellow", border = "black")
plot(df$BMI, main = "BMI", xlab = "BMI", col = "yellow")
hist(df$DiabetesPedigreeFunction, main = "Diabetes Pedigree Function", 
     xlab = "Diabetes Pedigree Function", col = "grey", border = "black")
plot(df$DiabetesPedigreeFunction, main = "Diabetes Pedigree Function", 
     xlab = "Diabetes Pedigree Function", col = "grey")
hist(df$Age, main = "Age", xlab = "Age", col = "orange", border = "black")
plot(df$Age, main = "Age", xlab = "Age", col = "orange")
hist(df$SkinThickness, main = "Skin Thickness", xlab = "Skin Thickness", col = "violet", border = "black")
plot(df$SkinThickness, main = "Skin Thickness", xlab = "Skin Thickness", col = "violet")
hist(df$Pregnancies, main = "Pregnancies", xlab = "Pregnancies", col = "cyan", border = "black")
plot(df$Pregnancies, main = "Pregnancies", xlab = "Pregnancies", col = "cyan")

# Box plots
boxplot(Glucose ~ BloodPressure, data = df, xlab = "Blood Pressure", ylab = "Glucose", main = "Box Plot", 
        col = "pink")
boxplot(Glucose ~ Insulin, data = df, xlab = "Insulin", ylab = "Glucose", main = "Box Plot", col = "blue")
boxplot(Glucose ~ BMI, data = df, xlab = "BMI", ylab = "Glucose", main = "Box Plot", col = "yellow")
boxplot(Glucose ~ DiabetesPedigreeFunction, data = df, xlab = "Diabetes Pedigree Function", 
        ylab = "Glucose", main = "Box Plot", col = "grey")
boxplot(Glucose ~ Age, data = df, xlab = "Age", ylab = "Glucose", main = "Box Plot", col = "orange")
boxplot(Glucose ~ SkinThickness, data = df, xlab = "Skin Thickness", ylab = "Glucose", 
        main = "Box Plot", col = "violet")
boxplot(Glucose ~ Pregnancies, data = df, xlab = "Pregnancies", ylab = "Glucose", main = "Box Plot", 
        col = "cyan")

# Linear Regression
# Linear Regression of every independent variable with Glucose

# Blood Pressure vs. Glucose
l1 = lm(Glucose~BloodPressure, df)
summary(l1)
plot(l1$resid, col = "pink")  # Residual plot
plot(l1$resid~df$Glucose[order(df$Glucose)], main="Glucose vs Residuals - Simple Linear Regression", 
     xlab="Glucose", ylab="Residuals", col = "pink")
hist(l1$resid, main="Histogram of Residuals", ylab="Residuals", col = "pink")
qqnorm(l1$resid, col = "pink")   # Q-Q Plot
qqline(l1$resid, col = "pink")
plot(l1,which =2, col = "pink")

# Insulin vs. Glucose
l2 = lm(Glucose~Insulin, df)
summary(l2)
plot(l2$resid, col = "blue")  # Residual plot
plot(l2$resid~df$Glucose[order(df$Glucose)], main="Glucose vs Residuals - Simple Linear Regression", 
     xlab="Glucose", ylab="Residuals", col = "blue")
hist(l2$resid, main="Histogram of Residuals", ylab="Residuals", col = "blue")
qqnorm(l2$resid, col = "blue")   # Q-Q Plot
qqline(l2$resid, col = "blue")
plot(l2,which =2, col = "blue")

# BMI vs. Glucose
l3 = lm(Glucose~BMI, df)
summary(l3)
plot(l3$resid, col = "yellow")  # Residual plot
plot(l3$resid~df$Glucose[order(df$Glucose)], main="Glucose vs Residuals - Simple Linear Regression", 
     xlab="Glucose", ylab="Residuals", col = "yellow")
hist(l3$resid, main="Histogram of Residuals", ylab="Residuals", col = "yellow")
qqnorm(l3$resid, col = "yellow")   # Q-Q Plot
qqline(l3$resid, col = "yellow")
plot(l3,which =2, col = "yellow")

# Diabetes Pedigree Function vs. Glucose
l4 = lm(Glucose~DiabetesPedigreeFunction, df)
summary(l4)
plot(l4$resid, col = "grey")  # Residual plot
plot(l4$resid~df$Glucose[order(df$Glucose)], main="Glucose vs Residuals - Simple Linear Regression", 
     xlab="Glucose", ylab="Residuals", col = "grey")
hist(l4$resid, main="Histogram of Residuals", ylab="Residuals", col = "grey")
qqnorm(l4$resid, col = "grey")   # Q-Q Plot
qqline(l4$resid, col = "grey")
plot(l4,which =2, col = "grey")

# Age vs. Glucose
l5 = lm(Glucose~Age, df)
summary(l5)
plot(l5$resid, col = "orange")  # Residual plot
plot(l5$resid~df$Glucose[order(df$Glucose)], main="Glucose vs Residuals - Simple Linear Regression", 
     xlab="Glucose", ylab="Residuals", col = "orange")
hist(l5$resid, main="Histogram of Residuals", ylab="Residuals", col = "orange")
qqnorm(l5$resid, col = "orange")   # Q-Q Plot
qqline(l5$resid, col = "orange")
plot(l5,which =2, col = "orange")

# Skin Thickness vs. Glucose
l6 = lm(Glucose~SkinThickness, df)
summary(l6)
plot(l6$resid, col = "violet")  # Residual plot
plot(l6$resid~df$Glucose[order(df$Glucose)], main="Glucose vs Residuals - Simple Linear Regression", 
     xlab="Glucose", ylab="Residuals", col = "violet")
hist(l6$resid, main="Histogram of Residuals", ylab="Residuals", col = "violet")
qqnorm(l6$resid, col = "violet")   # Q-Q Plot
qqline(l6$resid, col = "violet")
plot(l6,which =2, col = "violet")

# Pregnancies vs. Glucose
l7 = lm(Glucose~Pregnancies, df)
summary(l7)
plot(l7$resid, col = "cyan")  # Residual plot
plot(l7$resid~df$Glucose[order(df$Glucose)], main="Glucose vs Residuals - Simple Linear Regression", 
     xlab="Glucose", ylab="Residuals", col = "cyan")
hist(l7$resid, main="Histogram of Residuals", ylab="Residuals", col = "cyan")
qqnorm(l7$resid, col = "cyan")   # Q-Q Plot
qqline(l7$resid, col = "cyan")
plot(l7,which =2, col = "cyan")

# Multiple Regression
mlr = lm(Glucose~(BloodPressure+Insulin+BMI+DiabetesPedigreeFunction+Age+SkinThickness+Pregnancies), 
         df) # lm(output~(input1+input2+...), data)
summary(mlr) # coefficient summary
plot(mlr$resid)  # Residual plot
plot(mlr$resid~df$Glucose[order(df$Glucose)], main="Glucose vs Residuals - Multiple Linear Regression", 
     xlab="Glucose", ylab="Residuals")
hist(mlr$resid, main="Histogram of Residuals",
     ylab="Residuals")
qqnorm(mlr$resid)   # Q-Q Plot
qqline(mlr$resid)
plot(mlr,which =2)

# Correlation
# Correlation of every independent variable with Glucose

# Plot of Glucose Levels
plot(seq(1, length(df$Glucose), by = 1), df$Glucose, main = "Glucose Level", col = "green", pch = 19, 
     frame = TRUE)

# Plot of Blood Pressure vs. Glucose
ggplot(df, aes(x = BloodPressure, y = Glucose))+geom_point()
# Correlation of Blood Pressure with Glucose
cor.test(df$BloodPressure, df$Glucose)

# Plot of Insulin vs. Glucose
ggplot(df, aes(x = Insulin, y = Glucose))+geom_point()
# Correlation of Insulin with Glucose
cor.test(df$Insulin, df$Glucose)

# Plot of BMI vs. Glucose
ggplot(df, aes(x = BMI, y = Glucose))+geom_point()
# Correlation of BMI with Glucose
cor.test(df$BMI, df$Glucose)

# Plot of Diabetes Pedigree Function vs. Glucose
ggplot(df, aes(x = DiabetesPedigreeFunction, y = Glucose))+geom_point()
# Correlation of Diabetes Pedigree Function with Glucose
cor.test(df$DiabetesPedigreeFunction, df$Glucose)

# Plot of Age vs. Glucose
ggplot(df, aes(x = Age, y = Glucose))+geom_point()
# Correlation of Age with Glucose
cor.test(df$Age, df$Glucose)

# Plot of Skin Thickness vs. Glucose
ggplot(df, aes(x = SkinThickness, y = Glucose))+geom_point()
# Correlation of Skin Thickness with Glucose
cor.test(df$SkinThickness, df$Glucose)

# Plot of Pregnancies vs. Glucose
ggplot(df, aes(x = Pregnancies, y = Glucose))+geom_point()
# Correlation of Pregnancies with Glucose
cor.test(df$Pregnancies, df$Glucose)

# Correlation Matrix of all variables
df2 <- select(df, c(1:8))
df2mat <- round(cor(df2), 4)
df2mat_melted <- melt(df2mat)
ggplot(df2mat_melted, aes(x = Var1, y = Var2, fill = value))+geom_tile()