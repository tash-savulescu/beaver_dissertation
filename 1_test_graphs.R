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




