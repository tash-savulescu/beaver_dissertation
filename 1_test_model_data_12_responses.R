install.packages("MuMIn")
library(MuMIn)

#Load data set
attach(Test_model_12_responses)
test_model <- Test_model_12_responses

#Null ID column
test_model$ID <- NULL

test_model <- subset(test_model, select = c(Age, Land_Size, Land_Ownership, Total_Indiv_Pos_Impact_Score, Total_Indiv_Neg_Impact_Score, Total_Com_Pos_Impact_Score, Total_Com_Neg_Impact_Score, Overall_Benefit_Individual, Overall_Benefit_Community, Willingness_Score, Economic_Value_WS, Costs_WS, Alternative_Solutions_WS, Ecological_Importance_WS, Emotional_WS))

#Run correlational matrix
cor_matrix <- cor(test_model)


# Install the gplots package (if not installed) for graph 
install.packages("gplots") 

# Load the gplots package
library(gplots)           


# Option 1: Remove rows with any missing values
test_model_cleaned <- test_model[complete.cases(test_model), ]

# Calculate the correlation matrix
cor_matrix <- cor(test_model_cleaned)

# Create the heatmap
# heatmap.2(cor_matrix,
#trace = "none",
#key = TRUE,
#keysize = 1.0,
#col = colorRampPalette(c("blue", "white", "red"))(100),
# symkey = FALSE
          # )

# Calculate Significance values 
# install packages
install.packages("Hmisc")
library(Hmisc)
install.packages("corrplot")
library(corrplot)

test_model_cleaned.rcorr = rcorr(as.matrix(test_model_cleaned))
test_model_cleaned.rcorr

test_model_cleaned.coeff = test_model_cleaned.rcorr$r
test_model_cleaned.p = test_model_cleaned.rcorr$P


mydata.cor = cor(test_model_cleaned)

corrplot(mydata.cor)
