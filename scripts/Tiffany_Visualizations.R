# PUBH 422 - Group Project 1
# Tiffany's individual visualization work

# Import HINTS 2020 dataset
data_name <- read.csv("HINTSData_2020_clean.csv")

# Age histogram
hist(data_name$Age,
     main = "Distribution of Age",
     xlab = "Age (years)",
     ylab = "Frequency",
     col = "lightblue")