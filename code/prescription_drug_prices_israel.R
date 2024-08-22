#Project: I compared the maximum retailer price vs. the maximum consumer price for the sales of the drug "ABBOSYNAGIS 100MG PUL FOR INJ+SOLV" in Israel between 2007 and 2021
#I used a multiple linear regression model to compare the actual retailer and consumer prices to the predicted prices. Once that was completed, I created singular linear regression models to predict both the retailer and consumer prices for the years 2022-2035
#This data is from the data set: 'Prescription Drugs Prices in Israel 2007-2021' by Thomas Konstantin (2020). This data set was discovered on Kaggle

#Opening required libraries:
library(dplyr)
library(readr)
library(ggplot2)

#Importing data for each year (2007-2021):

#2007:
file_path_07 = "/Users/stevenleicht/Downloads/archive-3/TM_2007.csv"
data_07 = read.csv(file_path_07)
head(data_07)

#2008:
file_path_08 = "/Users/stevenleicht/Downloads/archive-3/TM_2008.csv"
data_08 = read.csv(file_path_08)
head(data_08)

#2009:
file_path_09 = "/Users/stevenleicht/Downloads/archive-3/TM_2009.csv"
data_09 = read.csv(file_path_09)
head(data_09)

#2010:
file_path_10 = "/Users/stevenleicht/Downloads/archive-3/TM_2010.csv"
data_10 = read.csv(file_path_10)
head(data_10)

#2011:
file_path_11 = "/Users/stevenleicht/Downloads/archive-3/TM_2011.csv"
data_11 = read.csv(file_path_11)
head(data_11)

#2012:
file_path_12 = "/Users/stevenleicht/Downloads/archive-3/TM_2012.csv"
data_12 = read.csv(file_path_12)
head(data_12)

#2013:
file_path_13 = "/Users/stevenleicht/Downloads/archive-3/TM_2013.csv"
data_13 = read.csv(file_path_13)
head(data_13)

#2014:
file_path_14 = "/Users/stevenleicht/Downloads/archive-3/TM_2014.csv"
data_14 = read.csv(file_path_14)
head(data_14)

#2015:
file_path_15 = "/Users/stevenleicht/Downloads/archive-3/TM_2015.csv"
data_15 = read.csv(file_path_15)
head(data_15)

#To note: no data is provided for the year 2016

#2017:
file_path_17 = "/Users/stevenleicht/Downloads/archive-3/TM_2017.csv"
data_17 = read.csv(file_path_17)
head(data_17)

#2018:
file_path_18 = "/Users/stevenleicht/Downloads/archive-3/TM_2018.csv"
data_18 = read.csv(file_path_18)
head(data_18)

#2019:
file_path_19 = "/Users/stevenleicht/Downloads/archive-3/TM_2019.csv"
data_19 = read.csv(file_path_19)
head(data_19)

#2020:
file_path_20 = "/Users/stevenleicht/Downloads/archive-3/TM_2020.csv"
data_20 = read.csv(file_path_20)
head(data_20)

#2021:
file_path_21 = "/Users/stevenleicht/Downloads/archive-3/TM_2021.csv"
data_21 = read.csv(file_path_21)
head(data_21)

#Now, combining all .csv files into one data set:
all_data = ls()
gather_data = all_data[sapply(all_data, function(x) is.data.frame(get(x)))]
combined_data_07_to_21 = bind_rows(lapply(gather_data, get))

#Now, filtering/selecting the data to only look at the drug named 'ABBOSYNAGIS 100MG PUL FOR INJ+SOLV' for each year for further analysis:
#Looking at the maximum retailer and consumer prices for every year for the specified drug:
drug_selected_data = combined_data_07_to_21 %>%
  filter(Medication_Name == "ABBOSYNAGIS 100MG PUL FOR INJ+SOLV") %>%
  select(Year, Max_Retailer_Price, Max_Consumer_Price)
print(drug_selected_data)
print(drug_selected_data$Max_Retailer_Price)
print(drug_selected_data$Max_Consumer_Price)

#Creating a scatter plot to see how prices changed throughout the years:
ggplot(drug_selected_data, aes(x = Year)) +
  geom_point(aes(y = Max_Retailer_Price, color = "Max Retailer Price")) +
  geom_smooth(aes(y = Max_Retailer_Price, color = "Max Retailer Price"), method = "lm", se = FALSE) +
  geom_point(aes(y = Max_Consumer_Price, color = "Max Consumer Price")) +
  geom_smooth(aes(y = Max_Consumer_Price, color = "Max Consumer Price"), method = "lm", se = FALSE) +
  labs(title = "Scatter Plot with Best Fit Line for Selected Drug",
       x = "Year",
       y = "Price",
       color = "Legend") +
  theme_minimal()
#As time goes by, both retailer and consumer prices for the selected drug decrease
#Reasons as to why this may be occurring are individualized or specified to the drug. However, a factor could be due to simple economic supply-and-demand of the product

#Based on the above data, I will now create a predictive model (multiple linear regression model) for both retailer and consumer prices of the specific drug for the years 2022 to 2035:
#The first model I will create will be for retailer prices:
mlr_model = lm(Max_Retailer_Price ~ Year + Max_Consumer_Price, data = drug_selected_data)
summary(mlr_model)
#The residual standard error for retailer prices is $37.67 on 23 degrees of freedom
#The adjusted r-squared value is 0.9988, meaning that the model is very reliable
#The p-value is <0.001, meaning that the model is very statistically significant

#Calculating the predictive values of retailer prices from 2007-2021:
predicted_retailer_price = -69270 + 34.28 * drug_selected_data$Year + 0.9113 * drug_selected_data$Max_Consumer_Price
print(predicted_retailer_price)

#Comparing the predicted retailer prices to the actual retailer prices from 2007-2021:
comparison_data = data.frame(
  Year = drug_selected_data$Year,
  Actual_Retailer_Price = drug_selected_data$Max_Retailer_Price,
  Predicted_Retailer_Price = predicted_retailer_price
)

#Measuring the numerical differences between the actual and predicted retailer prices from 2007-2021:
comparison_data$Difference = comparison_data$Actual_Retailer_Price - comparison_data$Predicted_Retailer_Price
print(comparison_data$Difference)

#Creating a scatter plot to show the differences between the actual and predicted retailer values:
ggplot(comparison_data, aes(x = Year)) +
  geom_point(aes(y = Actual_Retailer_Price, color = "Actual Retailer Price")) +
  geom_smooth(aes(y = Actual_Retailer_Price, color = "Actual Retailer Price"), method = "lm", se = FALSE) +
  geom_point(aes(y = Predicted_Retailer_Price, color = "Predicted Retailer Price")) +
  geom_smooth(aes(y = Predicted_Retailer_Price, color = "Predicted Retailer Price"), method = "lm", se = FALSE) +
  labs(title = "Differences Between Actual and Predicted Retailer Prices: 2007-2021",
       x = "Year",
       y = "Price",
       color = "Legend") +
  theme_minimal()
#The actual retailer prices compared to the predicted are extremely similar

#Creating a multiple linear regression model for consumer prices:
mlr_model_2 = lm(Max_Consumer_Price ~ Year + Max_Retailer_Price, data = drug_selected_data)
summary(mlr_model_2)
#The residual standard error for consumer prices is $40.79 on 23 degrees of freedom
#The adjusted r-squared value is 0.9991, meaning the this model is also very reliable
#The p-value is also <0.001, which means that this model is also very statistically significant

#Calculating the predictive values of retailer prices from 2007-2021:
predicted_consumer_price = 90190 - 44.59 * drug_selected_data$Year + 1.069 * drug_selected_data$Max_Retailer_Price
print(predicted_consumer_price)

#Comparing the predicted consumer prices to the actual consumer prices from 2007-2021:
comparison_data_2 = data.frame(
  Year = drug_selected_data$Year,
  Actual_Consumer_Price = drug_selected_data$Max_Consumer_Price,
  Predicted_Consumer_Price = predicted_consumer_price
)

#Measuring the numerical differences between the actual and predicted consumer prices from 2007-2021:
comparison_data_2$Difference = comparison_data_2$Actual_Consumer_Price - comparison_data_2$Predicted_Consumer_Price
print(comparison_data_2$Difference)

#Creating a scatter plot to show the differences between the actual and predicted retailer values:
ggplot(comparison_data_2, aes(x = Year)) +
  geom_point(aes(y = Actual_Consumer_Price, color = "Actual Consumer Price")) +
  geom_smooth(aes(y = Actual_Consumer_Price, color = "Actual Consumer Price"), method = "lm", se = FALSE) +
  geom_point(aes(y = Predicted_Consumer_Price, color = "Predicted Consumer Price")) +
  geom_smooth(aes(y = Predicted_Consumer_Price, color = "Predicted Consumer Price"), method = "lm", se = FALSE) +
  labs(title = "Differences Between Actual and Predicted Consumer Prices: 2007-2021",
       x = "Year",
       y = "Price",
       color = "Legend") +
  theme_minimal()
#The actual consumer prices compared to the predicted are extremely similar

#Now that the data from 2007-2021 is fully analyzed, I will create another predictive model to estimate changes in retailer/consumer pricing for the drug from 2022-2035:
#Changing the price data into historical data to create future models:
historical_data = data.frame(
  Year = 1:13, 
  Retailer_Price = c(6022.540, 5392.350, 5720.989, 4895.290, 4442.260, 4263.310, 3866.000, 3826.960,
                     3382.659, 3308.447, 2942.914, 2747.972, 2773.983),
  Consumer_Price = c(7076.485, 6336.012, 6722.162, 5751.970, 5219.660, 5009.390, 4542.550, 4496.680,
                     3890.058, 3722.003, 3237.205, 3022.769, 3051.381))

#Creating singular linear regression models for both retailer and consumer prices:
#Linear regression model for retailer prices:
retailer_model_22_to_35 = lm(Retailer_Price ~ Year, data = historical_data)
print(retailer_model_22_to_35)

# Linear regression model for consumer prices:
consumer_model_22_to_35 <- lm(Consumer_Price ~ Year, data = historical_data)
print(consumer_model_22_to_35)

#Defining the years 2022-2035 for predictive purposes:
future_years = data.frame(Year = 14:27)

#Predicting future retailer and consumer prices for 2022-2035:
retailer_prices_22_to_35 = predict(retailer_model_22_to_35, newdata = future_years)
print(retailer_prices_22_to_35)
consumer_prices_22_to_35 = predict(consumer_model_22_to_35, newdata = future_years)
print(consumer_prices_22_to_35)

#Combining the predicted price data into another data frame:
future_prices = data.frame(
  Year = 14:27,
  Predicted_Retailer_Price = retailer_prices_22_to_35,
  Predicted_Consumer_Price = consumer_prices_22_to_35
)

#Creating another scatter plot to showcase the differences in historical price data to the predicted values for 2022-2035:

#Combining historical and predicted retailer data:
ovr_combined_retailer_data <- rbind(
  data.frame(Year = historical_data$Year, 
             Price = historical_data$Retailer_Price, 
             Type = "Actual Retailer Price"),
  data.frame(Year = future_prices$Year, 
             Price = future_prices$Predicted_Retailer_Price, 
             Type = "Predicted Retailer Price")
)
print(ovr_combined_retailer_data)

#Creating a scatter plot for retailer prices:
ggplot(ovr_combined_retailer_data, aes(x = Year, y = Price, color = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Change in Maximum Retailer Price for Specified Drug from 2007-2035",
       x = "Year",
       y = "Price",
       color = "Legend") +
  theme_minimal()
#Based on current predictions, the maximum retailer price for this drug will drop below $0 after year 21 (2028). Obviously, that would not be possible, but it is interesting to see how prices will change in years to come and if this linear decline will continue

#Combining historical and predicted consumer data:
ovr_combined_consumer_data = rbind(
  data.frame(Year = historical_data$Year, 
             Price = historical_data$Consumer_Price, 
             Type = "Actual Consumer Price"),
  data.frame(Year = future_prices$Year, 
             Price = future_prices$Predicted_Consumer_Price, 
             Type = "Predicted Consumer Price")
)
print(ovr_combined_consumer_data)

#Creating a scatter plot for consumer prices:
ggplot(ovr_combined_consumer_data, aes(x = Year, y = Price, color = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Change in Maximum Consumer Price for Specified Drug from 2007-2035",
       x = "Year",
       y = "Price",
       color = "Legend") +
  theme_minimal()
#Similarly to maximum retailer price, maximum consumer price for this drug will drop below $0 after year 20 (2027). It will also be interesting to see how this changes as time goes on

#Overall conclusion:
#The specified drug of "ABBOSYNAGIS 100MG PUL FOR INJ+SOLV" had a relatively and rapdi linear decline in both maximum retailer and consumer prices during the years from 2007 to 2021
#The predicted values for the years 2007 to 2021 were extremely similar, reliable and statistically significant to the actual values
#The predicted values for future years showcase the continued linear decline of the drug, but do not show an accurate representation of what is likely to happen. Moreover, even following basic supply-and-demand philosophy, the demand of the drug will increase again - as will the prices
#My prediction is that the rise in drug prices (for the specified drug) will start to occur around 2027... based on all of this data/analysis
#Future research could explore different types of predictive models (non-linear regression, logarithmic, etc.) to predict a more accurate price range for the selected drug
