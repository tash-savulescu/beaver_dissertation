#RQ2:What is the desirable compensation cost required to harmonise beaver-landowner conflicts in Kent?
#Of the survey respondents, 11/13 stated they would accept compensation. 5/11 of the landowners who stated they would accept compensation provided a quantitative compensation value.
#The other 6/11 landowners provided qualitative answers such as 'land value' or 'loss of income'.
#To further explore RQ2, the quantitative values provided by 5/11 of the respondents where explored to see if there were variables correlated. 
#The purpose of this is to see if there are determinants influencing a higher compensation cost stated.However, as the sample size for this exploration is small (n=5). This exploration is only for future research and added depth. It is not statistically significant.

# Install the required packages
install.packages("MuMIn")
library(MuMIn)

install.packages("gplots") 
library(gplots)           

install.packages("Hmisc")
library(Hmisc)

install.packages("corrplot")
library(corrplot)

install.packages("ggplot2")
library(ggplot2)


#STEP 1: PREPARE THE DATA
#Load data set
attach(X0_test_data_5_responses_compensation_value)
test_c_value <- X0_test_data_5_responses_compensation_value

#Null ID column
test_c_value$ID <- NULL

#Check data is numeric
class(test_c_value)
test_c_value <- as.numeric(test_c_value)
str(test_c_value)
head(test_c_value)

# Transform gender to binary (1 for Male, 0 for Female)
test_c_value$Gender <- ifelse(test_c_value$Gender == "Male", 1, 0)
print(test_c_value)

colnames(test_c_value)


#STEP 2:CHECKING WHICH VARIABLES ARE CORRELATED WITH COMPENSATION VALUES
test_c_value <- subset(test_c_value, select = c(Age, Land_Size, Land_Ownership, Total_Indiv_Pos_Impact_Score, Com_Neg_Impact_Crop_Damage, Com_Neg_Impact_Irrigation, Com_Neg_Impact_Damage_Fences, Total_Com_Pos_Impact_Score, Total_Com_Neg_Impact_Score, Economic_Value_WS, Costs_WS, Alternative_Solutions_WS, Ecological_Importance_WS, Emotional_WS, `Annual_compensation`))

#Run correlational matrix
cor_matrix <- cor(test_c_value)

mydata.cor = cor(test_c_value)

corrplot(mydata.cor)


#Calculate Significance values 
test_c_value.rcorr = rcorr(as.matrix(test_c_value))
test_c_value.rcorr

test_c_value.coeff = test_c_value.rcorr$r
test_c_value.p = test_c_value.rcorr$P

#Show p-values
pearson_model <- subset(test_c_value.p, select = c(Annual_compensation))
show(pearson_model)

#Show correlation values
coeff_model <- subset(test_c_value.coeff, select = c(Annual_compensation))
show(coeff_model)

#Merge to show in one table
CompensationCor <- data.frame(coeff_model,pearson_model)
names(CompensationCor) <- c("Correlation", "P-Value")
show(CompensationCor)



#STEP 3: BUILDING LINEAR REGRESSION SCATTER PLOTS TO ASSESS HOW VARIABLES INFLUENCE COMPENSATION VALUE 
x <- test_c_value$Com_Neg_Impact_Irrigation
y <- test_c_value$Annual_compensation
data <- data.frame(x, y)

# Calculate the correlation coefficient
correlation_coefficient <- cor(data$x, data$y)
print(paste("Correlation Coefficient: ", correlation_coefficient))

#Scatter plot with a linear regression line
ggplot(data, aes(x = x, y = y)) +
  geom_point() +                      
  geom_smooth(method = "lm", se = FALSE) +   
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "Perceived Community Negative Impact (Interferences with Irrigation",
       y = "Annual Compensation Value") +
  theme_minimal()



# STEP 4: BUILD A GLM TO SEE THE INFLUENCE OF EXPLANATORY VARIABLES ON THE RESPONSE VARIABLE (COMPENSATION VALUE)
# Load required libraries
install.packages("car") # for VIF calculation
library(ggplot2)
library(car)

# Step 4.1: Prepare the data
Annual_compensation <- test_c_value$Annual_compensation
Com_Neg_Impact_Irrigation <- test_c_value$Com_Neg_Impact_Irrigation
Com_Neg_Impact_Crop_Damage <- test_c_value$Com_Neg_Impact_Crop_Damage
Com_Neg_Impact_Damage_Fences <- test_c_value$Com_Neg_Impact_Damage_Fences

# Step 4.2: Fit the GLM (assuming Gaussian family and identity link)
model <- glm(Annual_compensation ~ Com_Neg_Impact_Irrigation + Com_Neg_Impact_Crop_Damage + Com_Neg_Impact_Damage_Fences, data = data, family = gaussian(link = "identity"))

summary(model)

# Step 4.3: Check for multi-collinearity by calculating the correlation matrix of the explanatory variables
cor_matrix <- cor(test_c_value [, c("Com_Neg_Impact_Irrigation", "Com_Neg_Impact_Crop_Damage", "Com_Neg_Impact_Damage_Fences")])
show(cor_matrix)

# Calculate VIF for each predictor
vif_values <- car::vif(model)
summary(vif_values)

# Step 4.4: Check for homoscedasticity by plotting residuals against predicted values
residuals <- resid(model)
fitted_values <- fitted(model)

ggplot(data.frame(residuals, fitted_values), aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

#Step 4.5: Check for multivariate normality by plotting histogram and Q-Q plot of model residuals
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Histogram of Residuals") +
  theme_minimal()

qqnorm(residuals)
qqline(residuals)

#Step 4.6:Perform the Shapiro-Wilk test for normality for further validity checks
shapiro.test(residuals)
