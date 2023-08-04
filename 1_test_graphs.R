install.packages("MuMIn")
library(MuMIn)

#Load data set
attach(Test_model_12_responses)
test_graph <- Test_model_12_responses

#Null ID column
#test_graph$ID <- NULL

test_graph <- subset(test_graph, select = c(Willingness_Score, Economic_Value_WS, Costs_WS, Alternative_Solutions_WS, Ecological_Importance_WS, Emotional_WS))
# Install and load the ggplot2 package 
install.packages("ggplot2")
library(ggplot2)

Willingness_Score <- test_graph$Willingness_Score
Economic_Value_WS <- test_graph$Economic_Value_WS
Costs_WS <- test_graph$Costs_WS
Alternative_Solutions_WS <- test_graph$Alternative_Solutions_WS
Ecological_Importance_WS <- test_graph$Ecological_Importance_WS
Emotional_WS <- test_graph$Emotional_WS

#Create a histogram
ggplot(test_graph, aes(x = Willingness_Score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Willingness to Accept Comepnsation", x = "WTA Compensation Value", y = "Frequency of Responses")


ggplot(test_graph, aes(x = Costs_WS)) +
  geom_histogram(binwidth = 1, fill = "pink", color = "black") +
  labs(title = "Histogram of Factors Influencing Willingness to Accept Comepnsation", x = "Costs associated with mitigating beaver activity", y = "Frequency of Responses")


ggplot(test_graph, aes(x = Economic_Value_WS)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  labs(title = "Histogram of Factors Influencing Willingness to Accept Comepnsation", x = "Economic Value of Land", y = "Frequency of Responses")



ggplot(test_graph, aes(x = Alternative_Solutions_WS)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Factors Influencing Willingness to Accept Comepnsation", x = "Alternative Solutions Available", y = "Frequency of Responses")


# Sample data
x <- Total_Indiv_Neg_Impact_Score
y <- Willingness_Score
data <- data.frame(x, y)

# Calculate the correlation coefficient
correlation_coefficient <- cor(data$x, data$y)
print(paste("Correlation Coefficient: ", correlation_coefficient))

# Scatter plot with a linear regression line
ggplot(data, aes(x = x, y = y)) +
  geom_point() +                       # Add points
  geom_smooth(method = "lm", se = FALSE) +   # Add a linear regression line
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "Total Individual Negative Impact Score",
       y = "Willingness to Accept Compensation Score") +
  theme_minimal()


# Sample data
x <- Emotional_WS
y <- Willingness_Score
data <- data.frame(x, y)

# Calculate the correlation coefficient
correlation_coefficient <- cor(data$x, data$y)
print(paste("Correlation Coefficient: ", correlation_coefficient))

# Scatter plot with a linear regression line
ggplot(data, aes(x = x, y = y)) +
  geom_point() +                       # Add points
  geom_smooth(method = "lm", se = FALSE) +   # Add a linear regression line
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "Emotional and Sentimental Attachment to the Land Influencing WTA",
       y = "Willingness to Accept Compensation Score") +
  theme_minimal()

# Load required libraries
install.packages("car") # for VIF calculation
library(ggplot2)
library(car)

# Step 1: Prepare the data
# Assuming you have a dataset 'data' with the dependent variable 'y' and predictor variables 'x1', 'x2', etc.

# Step 2: Fit the GLM (assuming Gaussian family and identity link)
model <- glm(Willingness_Score ~ Land_Ownership + Total_Indiv_Neg_Impact_Score + Total_Indiv_Pos_Impact_Score + Emotional_WS + Costs_WS, data = data, family = gaussian(link = "identity"))

# Step 3: Check for multicollinearity
# Calculate the correlation matrix
cor_matrix <- cor(test_graph[, c("Land_Ownership", "Total_Indiv_Neg_Impact_Score", "Total_Indiv_Pos_Impact_Score", "Emotional_WS", "Costs_WS")])

# Calculate VIF for each predictor
vif_values <- car::vif(model)

# Step 4: Check for homoscedasticity
# Plot residuals against predicted values
residuals <- resid(model)
fitted_values <- fitted(model)

ggplot(data.frame(residuals, fitted_values), aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Step 5: Check for multivariate normality
# Plot histogram and Q-Q plot of model residuals
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Histogram of Residuals") +
  theme_minimal()

qqnorm(residuals)
qqline(residuals)

# Perform the Shapiro-Wilk test for normality
shapiro.test(residuals)

