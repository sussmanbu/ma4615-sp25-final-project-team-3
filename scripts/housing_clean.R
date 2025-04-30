library(dplyr)

# Load original data
housing_original <- readRDS("dataset/housing_original.rds")

# Clean the data
housing_cleaned <- housing_original |>
  filter(
    STATEICP <= 82,  
    ACREHOUS != 0, 
    VALUEH != 9999999,
    FTOTINC != 9999999
  ) |>
  mutate(
    ACREHOUS = ifelse(ACREHOUS == 1, 0,
                      ifelse(ACREHOUS == 2, 1, ACREHOUS))
  ) |>
  select(-RENT) |>
  select(-RACED)
saveRDS(housing_cleaned, "dataset/housing_cleaned.rds")