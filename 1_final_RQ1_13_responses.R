#RQ1:What are the determinants of landowners' willingness to accept (WTA) compensation for beaver activity?
#To identify the underlying determinants impacting landowners' WTA compensation, a generalised linear model (GLM) will be built.
#By building a GLM, the degree of influence of each significant determinant can be explored.

#Install all packages
install.packages("MuMIn")
library(MuMIn)
install.packages("gplots") 
library(gplots)
install.packages("Hmisc")
library(Hmisc)
install.packages("corrplot")
library(corrplot)

#Load data set
attach(X0_test_data_13_responses)
test_model <- X0_test_data_13_responses

#Null ID column
test_model$ID <- NULL

#STEP 1: CHECKING WHICH VARIABLES ARE CORRELATED WITH WILLINGNESS SCORE
#Transform Gender to binary data
test_model$Gender <- ifelse(test_model$Gender == "Male", 1, 0)

#Deal with any missing values
test_model_cleaned <- test_model[complete.cases(test_model), ]

#Assess correlation of all variables
cor_matrix <- cor(test_model_cleaned)
show(cor_matrix)

mydata.cor = cor(test_model_cleaned)

corrplot(mydata.cor)

#AFTER ASSESSING WHICH VARIABLES ARE CORRELATED, CREATE A SMALLER SUBSET OF VARIABLES
attach(X0_test_data_13_responses)
test_model <- X0_test_data_13_responses

test_model$ID <- NULL

test_model <- subset(test_model, select = c(Land_Size, Land_Ownership, Total_Indiv_Pos_Impact_Score, Total_Indiv_Neg_Impact_Score, Total_Com_Pos_Impact_Score, Total_Com_Neg_Impact_Score, Overall_Benefit_Individual, Overall_Benefit_Community, Willingness_Score, Economic_Value_WS, Costs_WS, Alternative_Solutions_WS, Ecological_Importance_WS, Emotional_WS))

#Run correlational matrix
cor_matrix <- cor(test_model)


# Calculate significance values of variables
test_model.rcorr = rcorr(as.matrix(test_model))
test_model.rcorr

test_model.coeff = test_model.rcorr$r
test_model.p = test_model.rcorr$P

#Show p-values
pearson_model <- subset(test_model.p, select = c(Willingness_Score))
show(pearson_model)

#Show correlation values
coeff_model <- subset(test_model.coeff, select = c(Willingness_Score))
show(coeff_model)

#Merge to show in one table
WillingnesCor <- data.frame(coeff_model,pearson_model)
names(WillingnesCor) <- c("Correlation", "P-Value")
show(WillingnesCor)


#Create correlation plot map
mydata.cor = cor(test_model)

corrplot(mydata.cor)


#STEP 2: BUILDING HISTOGRAMS FOR SENSE CHECKING AND UNDERSTANDING DATA

test_histograms <- subset(test_model, select = c(Willingness_Score, Economic_Value_WS, Costs_WS, Alternative_Solutions_WS, Ecological_Importance_WS, Emotional_WS))

# Install and load the ggplot2 package 
install.packages("ggplot2")
library(ggplot2)

Willingness_Score <- test_histograms$Willingness_Score
Economic_Value_WS <- test_histograms$Economic_Value_WS
Costs_WS <- test_histograms$Costs_WS
Alternative_Solutions_WS <- test_histograms$Alternative_Solutions_WS
Ecological_Importance_WS <- test_histograms$Ecological_Importance_WS
Emotional_WS <- test_histograms$Emotional_WS

#Create a histogram
ggplot(test_histograms, aes(x = Willingness_Score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Willingness Score", x = "Willingness Score", y = "Frequency of Responses")


ggplot(test_graph, aes(x = Costs_WS)) +
  geom_histogram(binwidth = 1, fill = "pink", color = "black") +
  labs(title = "Histogram of Factors Influencing Willingness Score", x = "Costs associated with mitigating beaver activity", y = "Frequency of Responses")


ggplot(test_graph, aes(x = Economic_Value_WS)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  labs(title = "Histogram of Factors Influencing Willingness to Accept Comepnsation", x = "Economic Value of Land", y = "Frequency of Responses")



ggplot(test_graph, aes(x = Alternative_Solutions_WS)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Factors Influencing Willingness to Accept Comepnsation", x = "Alternative Solutions Available", y = "Frequency of Responses")



ggplot(test_graph, aes(x = Ecological_Importance_WS)) +
  geom_histogram(binwidth = 1, fill = "yellow", color = "black") +
  labs(title = "Histogram of Factors Influencing Willingness to Accept Comepnsation", x = "Ecological Importance", y = "Frequency of Responses")



ggplot(test_graph, aes(x = Emotional_WS)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "Histogram of Factors Influencing Willingness to Accept Comepnsation", x = "Emotional and Cultutal Significance", y = "Frequency of Responses")



#STEP 3: BUILDING LINEAR REGRESSION SCATTER PLOTS 
glm_model <- X0_test_data_13_responses

#Null ID column
glm_model$ID <- NULL

glm_model <- subset(glm_model , select = c(Land_Size, Land_Ownership, Total_Indiv_Pos_Impact_Score, Total_Indiv_Neg_Impact_Score, Total_Com_Pos_Impact_Score, Total_Com_Neg_Impact_Score, Overall_Benefit_Individual, Overall_Benefit_Community, Willingness_Score, Economic_Value_WS, Costs_WS, Alternative_Solutions_WS, Ecological_Importance_WS, Emotional_WS))

# Plot a linear regression scatterplot of WS and Total Individual Negative Impact Score
x <- glm_model$Total_Indiv_Neg_Impact_Score
y <- glm_model$Willingness_Score
data <- data.frame(x, y)

# Calculate the correlation coefficient
correlation_coefficient <- cor(data$x, data$y)
print(paste("Correlation Coefficient: ", correlation_coefficient))

# Scatter plot with a linear regression line
ggplot(data, aes(x = x, y = y)) +
  geom_point() +                      
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "Total Individual Negative Impact Score",
       y = "Willingness to Accept Compensation Score") +
  theme_minimal()


# STEP 4: BUILDING GLM

# Load required libraries
install.packages("car") # for VIF calculation
library(ggplot2)
library(car)

# Step 4.1: Prepare the data with dependent variable (response) being WTA and explanatory variables

Land_Ownership <- glm_model$Land_Ownership
Total_Indiv_Pos_Impact_Score <- glm_model$Total_Indiv_Pos_Impact_Score
Total_Indiv_Neg_Impact_Score <- glm_model$Total_Indiv_Neg_Impact_Score
Overall_Benefit_Individual <- glm_model$Overall_Benefit_Individual
Emotional_WS <- glm_model$Emotional_WS
Ecological_Importance_WS <- glm_model$Ecological_Importance_WS 
  

# Step 4.2: Fit the GLM (assuming Gaussian family and identity link)
model <- glm(Willingness_Score ~ Land_Ownership + Total_Indiv_Neg_Impact_Score + 
               Total_Indiv_Pos_Impact_Score + Overall_Benefit_Individual + Emotional_WS + 
               Ecological_Importance_WS, data = glm_model, family = gaussian(link = "identity"))

summary(model)


# Step 5: ADDITIONAL STASTICAL CHECKS FOR VALIDITY
# Step 5.1: Check for multi-collinearity by calculating the correlation matrix between explanatory variables
cor_matrix <- cor(test_model [, c("Land_Ownership", "Total_Indiv_Neg_Impact_Score", "Total_Indiv_Pos_Impact_Score",
                                  "Costs_WS", "Emotional_WS", "Ecological_Importance_WS")])
show(cor_matrix)

# Step 5.2: Calculate Variance Inflation Factor (VIF) for each predictor
vif_values <- car::vif(model)
summary(vif_values)

# Step 5.3: Check for homoscedasticity by plotting residuals against predicted values
residuals <- resid(model)
fitted_values <- fitted(model)

ggplot(data.frame(residuals, fitted_values), aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Step 5.4: Check for multivariate normality by plotting histogram and Q-Q plot of model residuals
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Histogram of Residuals") +
  theme_minimal()

qqnorm(residuals)
qqline(residuals)


# Step 5.6: Perform the Shapiro-Wilk test for normality
shapiro.test(residuals)


# Step 6: Perform power analysis
# Step 6.1: Install packages needed for power analysis
install.packages("pwr")
library(pwr)

# Step 6.2: State the effect size, power and significance level
effect_size <- 0.5
alpha <- 0.05
power <- 0.80

# Step 6.3: Run the equation for the power analysis
result <- pwr.t.test(d = effect_size, sig.level = alpha, power = power)

# Step 6.4: Print the required sample size
result$n

# Step 6.5:Test to see the effect of different effect sizes
# Create a range of effect sizes
effect_sizes <- seq(0.2, 1.0, by = 0.1)

# Perform power analysis for each effect size
required_sample_sizes <- sapply(effect_sizes, function(d) {
  pwr::pwr.t.test(d = d, sig.level = 0.05, power = 0.80)$n
})

# Create a data frame for plotting
data <- data.frame(effect_size = effect_sizes, required_sample_size = required_sample_sizes)

# Show the result of different effect sizes and the required sample size in a scatter plot
ggplot(data, aes(x = effect_size, y = required_sample_size)) +
  geom_point() +
  geom_line() +
  labs(x = "Effect size (Cohen's d) (d)",
       y = "Required sample size (n)",
       title = "Power analysis results : required sample size vs. effect size")





# FURTHER EVALUATION OF ASSUMPTIONS THROUGH SENSE CHECKING OF OTHER MODELS 
# Checking model with only one statistically significant variable 
model_2 <- glm(Willingness_Score ~ Total_Indiv_Neg_Impact_Score, data = data, family = gaussian(link = "identity"))
summary(model_2)

# Fitting an ordinal GLM, therefore checking a different family link
install.packages("ordinal")
library(ordinal)
install.packages("MASS")
library(MASS)

# Convert Willingness_Score to an ordered factor
test_model$Willingness_Score <- factor(test_model$Willingness_Score, ordered = TRUE)

# Specify the order of the levels if needed (e.g., for a 5-point Likert scale)
test_model$Willingness_Score <- factor(test_model$Willingness_Score, 
                                       ordered = TRUE, 
                                       levels = c("1", "2", "3", "4", "5"))

Willingness_Score <- test_model$Willingness_Score

model_3 <- polr(Willingness_Score ~ Land_Ownership + Total_Indiv_Neg_Impact_Score + Total_Indiv_Pos_Impact_Score + Overall_Benefit_Individual + Emotional_WS + Ecological_Importance_WS, data = data, Hess = TRUE)

# Print model summary
summary(model_3)

# Checking against a cumulative link model
model_4 <- clm(Willingness_Score ~ Land_Ownership + Total_Indiv_Neg_Impact_Score + Total_Indiv_Pos_Impact_Score + Overall_Benefit_Individual + Emotional_WS + Ecological_Importance_WS, data = data)

summary(model_4)
