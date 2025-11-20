# Sample Student Dataset: Plant Growth Experiment (MESSY VERSION)
# This dataset simulates REAL data that middle school students might collect
# It includes common data entry mistakes and messiness

# Create the messy dataset
plant_data <- data.frame(
  Student_Name = c("Alex", "Bailey", "Casey ", "Drew", "emerson", "Finley", "Alex"),  # Extra space, lowercase, duplicate
  Group = c("A", "A", "B", "B", "C", "C", "A"),  # Matches the duplicate
  Treatment = c("Sunlight", "sunlight", "Partial Shade", "partial shade", "Full Shade", "full shade", "Sunlight"),  # Inconsistent capitalization
  `Week 1 Height (cm)` = c(2.5, 2.3, 2.4, 2.6, 2.2, 2.4, NA),  # Spaces in column name, missing value
  `Week 2 Height (cm)` = c(4.1, 3.9, 3.2, 3.5, 2.8, 2.9, 3.8),
  `Week_3_Height_cm` = c(6.8, 6.2, 4.5, 4.9, 3.5, 3.7, 6.5),  # Inconsistent naming convention
  Week_4_Height_cm = c(9.5, 8.9, 6.1, 6.8, 4.2, 4.6, 9.2),
  Final_Leaf_Count = c("12", "11", "8", "9", "6", "six", "12"),  # Mixed numbers and text!
  Notes = c("Healthy", "Slight yellowing", "", "Very healthy!", "slow growth", NA, "Healthy"),  # Empty string, missing, inconsistent
  Date_Started = c("9/1/2024", "09-01-2024", "9/1/24", "Sept 1 2024", "9/1/2024", "9/1/2024", "9/1/2024")  # Inconsistent date formats
)

# Save as CSV
write.csv(plant_data, "plant_growth_experiment.csv", row.names = FALSE)

# View the data
print(plant_data)
cat("\n\nMESSINESS INCLUDED:\n")
cat("- Duplicate student entry (Alex appears twice)\n")
cat("- Extra whitespace in names ('Casey ')\n")
cat("- Inconsistent capitalization (emerson, sunlight vs Sunlight)\n")
cat("- Inconsistent column naming (spaces vs underscores)\n")
cat("- Missing values (NA and empty strings)\n")
cat("- Mixed data types (Final_Leaf_Count has '6' and 'six')\n")
cat("- Inconsistent date formats\n")
cat("- Wide format for weekly measurements\n\n")

# Messy second dataset for JOIN practice
student_info <- data.frame(
  student_name = c("Alex", "Bailey", "Casey", "Drew", "Emerson", "finley", "Jordan"),  # Different case, extra student
  Grade = c(7, 7, 8, "7th", 8, 8, 7),  # Mixed format (7 vs "7th")
  Class_Period = c(2, 2, 3, 3, 4, 4, 5),
  Teacher = c("Ms. Smith", "Ms. Smith", "Mr. Jones", "Mr. Jones", "Ms. Smith", "Ms. Smith", "Mr. Jones")
)

write.csv(student_info, "student_info.csv", row.names = FALSE)

# Messy third dataset for JOIN practice
treatment_details <- data.frame(
  Treatment = c("Sunlight", "Partial Shade", "Full Shade", "Control"),  # Extra treatment not in main data
  Light_Hours_Per_Day = c("8 hours", "4", "1", "0"),  # Mixed formats (text vs numbers)
  Expected_Growth_Rate = c("High", "medium", "Low", "None"),  # Inconsistent capitalization
  Water_Amount_mL = c(100, 100, 100, NA)  # Missing value
)

write.csv(treatment_details, "treatment_details.csv", row.names = FALSE)

cat("\nThree CSV files created:\n")
cat("1. plant_growth_experiment.csv - Main messy dataset\n")
cat("2. student_info.csv - Student info with mismatched names\n")
cat("3. treatment_details.csv - Treatment details with extra row\n")