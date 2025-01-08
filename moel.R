# Load necessary libraries
library(tidyverse)
library(janitor)
library(randomForest)


# Step 1: Upload CSV file and clean column names
sales_data <- read_csv("sales_data.csv") %>%
  janitor::clean_names()  

# Step 2: Replace NA values with 0 in all date columns
sales_data <- sales_data %>%
  mutate(across(starts_with("x202"), ~ replace_na(., 0))) 

# Step 3: Remove 'usa' and 'total' columns before reshaping
sales_data <- sales_data %>%
  select(-usa, -total)  

# Step 4: Reshape the data using pivot_longer
sales_data_long <- sales_data %>%
  pivot_longer(
    cols = starts_with("x202"),         
    names_to = "date",                  
    values_to = "sales_volume"          
  )

# Step 5: Clean data a little more
sales_data_long <- sales_data_long %>%
  mutate(date = gsub("^x", "", date))  %>% 
  select(-model, -adi, -drive_type)


##############################################
################## Modeling ##################
##############################################

######### Linear Regression (Co-efficient) #########
################# Tesla & Hyundai Linear Regression #################

############################## Hyundai (All Segments) ##############################


# Step 1: Filter for Hyundai
hyundai_data <- sales_data_long %>%
  filter(brands == "Hyundai")

# Step 2: Handle Missing Values 
hyundai_data <- hyundai_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))



# Re-encode categorical variables to ensure proper levels
hyundai_data <- hyundai_data %>%
  mutate(
    region = as.factor(region),
    fuel_type = as.factor(fuel_type),
    segmentations = as.factor(segmentations),
    models = as.factor(models),
    date = as.factor(date)
  )



# Fit the Regression Model
hyundai_models <- lm(
  sales_volume ~ region + models + date,
  data = hyundai_data
)

# View the Model Summary
summary(hyundai_models)


# Fit the Regression Model
hyundai_segmentations <- lm(
  sales_volume ~ region + segmentations + date,
  data = hyundai_data
)

# View the Model Summary
summary(hyundai_segmentations)



# Fit the Regression Model
hyundai_fuel_type <- lm(
  sales_volume ~ region + fuel_type + date,
  data = hyundai_data
)

# View the Model Summary
summary(hyundai_fuel_type)






############################## Tesla ##############################
# Step 1: Filter for Tesla
tesla_data <- sales_data_long %>%
  filter(brands == "Tesla")

# Step 2: Handle Missing Values 
tesla_data <- tesla_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))



# Re-encode categorical variables to ensure proper levels
tesla_data <- tesla_data %>%
  mutate(
    region = as.factor(region),
    fuel_type = as.factor(fuel_type),
    segmentations = as.factor(segmentations),
    models = as.factor(models),
    model_year = as.factor(model_year),
    date = as.factor(date)
  )


# Fit the Regression Model
tesla_models <- lm(
  sales_volume ~ region + models + date,
  data = tesla_data
)

# View the Model Summary
summary(tesla_models)



# Fit the Regression Model
tesla_segmentations <- lm(
  sales_volume ~ region + segmentations + date,
  data = tesla_data
)

# View the Model Summary
summary(tesla_segmentations)



