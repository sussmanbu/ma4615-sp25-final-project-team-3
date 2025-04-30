library(dplyr)

# Load raw Kaggle housing dataset
housing_kaggle <- read.csv("dataset/Housing.csv")

# Clean and transform
second_housing_cleaned <- housing_kaggle %>%
  # Convert yes/no to binary (0/1)
  mutate(
    mainroad         = ifelse(mainroad == "yes", 1, 0),
    guestroom        = ifelse(guestroom == "yes", 1, 0),
    basement         = ifelse(basement == "yes", 1, 0),
    hotwaterheating  = ifelse(hotwaterheating == "yes", 1, 0),
    airconditioning  = ifelse(airconditioning == "yes", 1, 0),
    prefarea         = ifelse(prefarea == "yes", 1, 0),
    furnishingstatus = as.factor(furnishingstatus)
  ) %>%
  # Select only relevant variables
  select(price, area, bedrooms, bathrooms, stories, mainroad, guestroom,
         basement, hotwaterheating, airconditioning, parking, prefarea, furnishingstatus)

# Save cleaned dataset
saveRDS(second_housing_cleaned, "dataset/second_housing_cleaned.rds")
