#Day 3 (B.2)
#Installs the dplyr package.
#Remove # to install the dplyr package, afterwards add the # back to not keep re-installing it.
#install.packages("dplyr") R
library(dplyr)
#Installs the skimr package.
#Remove # to install the skimr package, afterwards add the # back to not keep re-installing it.
#install.packages("skimr")  
library(skimr)

#Day 4 (4.3)
#Installs the ggplot2 package.
#install.packages("ggplot2")  <- Remove the # to install the ggplot2 package, afterwards add the # back to not keep re-installing it.
library(ggplot2)

#Day 1 Project Setup & Data Import
#Reads the uploaded data sheet.
data_name <- read.csv("HINTSData_2020_clean.csv")

#Inspects data sheet for missing values and cleans it if applicable.
summary(data_name)
any(is.na(data_name))
head(data_name)

#Day 2 Data Management & Wrangling
#Separates specific variables from the data sheet.
age <- data_name$Age
avg_drink_per_week <- data_name$AvgDrinksPerWeek
weekly_minutes_moderate_excercise <- data_name$WeeklyMinutesModerateExercise
bmi <- data_name$BMI
quality_care <- data_name$QualityCare
birth_gender <- data_name$BirthGender
smoke_stat <- data_name$smokeStat
race_ethn5 <- data_name$RaceEthn5

#Day 3 Descriptive Analysis & Function Writing 
#B1/B3 Statistical Summaries
#Displays the statistical summaries of mean, median, standard deviation, & variance.
my_summary_func <- function(data_column) {
  cat("Summary:\n")
  print(summary(data_column))
  cat("\nMean: ", mean(data_column), 
      "\nMedian: ", median(data_column), 
      "\nStd. Dev: ", sd(data_column), 
      "\nVariance: ", var(data_column), "\n")
}
#Provides the summary functions of each variable.
my_summary_func(age)
my_summary_func(avg_drink_per_week)
my_summary_func(weekly_minutes_moderate_excercise)
my_summary_func(bmi)
my_summary_func(quality_care)
my_summary_func(birth_gender)
my_summary_func(smoke_stat)
my_summary_func(race_ethn5)

#B2 
# Generates the stratified summary statistics for continuous variables
grouped_summary <- data_name %>%
  group_by(smokeStat) %>%
  summarise(
    mean_Age = mean(Age, na.rm = TRUE),
    sd_Age = sd(Age, na.rm = TRUE),
    
    mean_BMI = mean(BMI, na.rm = TRUE),
    sd_BMI = sd(BMI, na.rm = TRUE),
    
    mean_Exercise = mean(WeeklyMinutesModerateExercise, na.rm = TRUE),
    sd_Exercise = sd(WeeklyMinutesModerateExercise, na.rm = TRUE),
    
    mean_Drinks = mean(AvgDrinksPerWeek, na.rm = TRUE),
    sd_Drinks = sd(AvgDrinksPerWeek, na.rm = TRUE)
  )
#Interpretation

# Print the results
print(grouped_summary)

#Produces statistical summary for the continuous variables
skim(data_name)

#Day 4 Uni-variate Visualization
#4.1
#Creates a histogram for variable quality care.
hist(quality_care)
#Creates a boxplot of variable race ethn5.
boxplot(race_ethn5)
#Interpret

#4.2
#Creates a histogram for variable BMI.
hist(bmi)
#Creates a histogram for variable age.
boxplot(age)
#Interpret

#4.3
#Uses the ggplot2 package to create a visual analysis of Quality care.
ggplot(data_name, aes(x = quality_care)) +
  geom_histogram() +
  labs(
    title = "Distribution of Quality Care",
    x = "Quality Care",
    y = "Frequency",
  )
#Uses the ggplot2 package to create a visual analysis of Race Ethnc5.
ggplot(data_name, aes(x = race_ethn5)) +
  geom_histogram() +
  labs(
    title = "Distribution of Race Ethc5",
    x = "Race Ethc5",
    y = "Frequency",
  )
#Uses the ggplot2 package to create a visual analysis of Age.
ggplot(data_name, aes(x = age)) +
  geom_histogram() +
  labs(
    title = "Distribution of Age",
    x = "Age",
    y = "Frequency",
  )
#Uses the ggplot2 package to create a visual analysis of BMI.
ggplot(data_name, aes(x = bmi)) +
  geom_histogram() +
  labs(
    title = "Distribution of BMI",
    x = "BMI",
    y = "Frequency",
  )
#4.4
#Creates visual function I for analysis 4.1.
my_visual_func_I <- function(hist_data, box_data){
  hist(quality_care)
  boxplot(race_ethn5)
}
#Creates visual function II for analysis 4.2.
my_visual_func_II <- function(hist_data, box_data){
  hist(bmi)
  boxplot(age)
}