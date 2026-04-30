# 7.1

library(palmerpenguins)
library(dplyr)

# Some analysis functions with bugs
calculate_mean<-function(data,col_name){
  result=data%>%summarise(mean_val=mean(col_name,na.rm=T))
  return result
}

clean_species_name <- function(species_text) {
  # Remove extra whitespace and fix common typos  
  species_text %>% 
    str_replace_all("  +", " ") %>%
    str_replace_all("adelie", "Adelie") %>%
    str_replace_all("chinstrap", "Chinstrap") 
}

penguin_summary<-penguins%>%
  filter(species="Adelie")%>%
  mutate(bill_ratio=bill_length_mm/bill_depth_mm)%>%
  summarise(
    mean_ratio=mean(bill_ratio,na.rm=TRUE),
    median_ratio=median(bill_ratio,na.rm=TRUE)
  )

# solution

library(palmerpenguins)
library(dplyr)
library(stringr)

# Some analysis functions with bugs
calculate_mean <- function(data, col_name) {
  result <- data %>%
    summarise(mean_val = mean({{ col_name }}, na.rm = TRUE))
  return(result)
}

clean_species_name <- function(species_text) {
  # Remove extra whitespace and fix common typos
  species_text %>%
    str_replace_all("  +", " ") %>%
    str_replace_all("adelie", "Adelie") %>%
    str_replace_all("chinstrap", "Chinstrap")
}

penguin_summary <- penguins %>%
  filter(species == "Adelie") %>%
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) %>%
  summarise(
    mean_ratio = mean(bill_ratio, na.rm = TRUE),
    median_ratio = median(bill_ratio, na.rm = TRUE)
  )

# 7.2

library(stringr)

messy_survey <- tibble(
  id = 1:8,
  species_reported = c(
    "adelie penguin", 
    "  Adelie  ",
    "ADELIE",
    "Chinstrap penguin",
    "chin strap", 
    "Gentoo",
    "gentoo penguin  ",
    "adelie"
  ),
  body_mass = c(3750, 3800, 3900, 3733, 3950, 5076, 5000, 3625)
)

clean_survey <- messy_survey %>%
  mutate(
    species_clean = species_reported %>%
      str_trim() %>%  # Remove leading/trailing whitespace
      str_to_title() %>%  # Proper case
      str_replace_all("\\s*Penguin\\s*", "") %>%  # Remove "penguin"
      str_replace_all("Chin\\s*Strap", "Chinstrap") %>%  # Fix chinstrap variants
      str_extract("Adelie|Chinstrap|Gentoo")  # Extract valid species only
  )

clean_survey %>% 
  filter(is.na(species_clean) | 
           !species_clean %in% c("Adelie", "Chinstrap", "Gentoo"))

# solution

library(tidyverse)
library(stringr)

messy_survey <- tibble(
  id = 1:8,
  species
  
  _reported = c(
    "adelie penguin", 
    "  Adelie  ",
    "ADELIE",
    "Chinstrap penguin",
    "chin strap", 
    "Gentoo",
    "gentoo penguin  ",
    "adelie"
  ),
  body_mass = c(3750, 3800, 3900, 3733, 3950, 5076, 5000, 3625)
)

clean_survey <- messy_survey %>%
  mutate(
    species_clean = species_reported %>%
      str_trim() %>%
      str_to_title() %>%
      str_replace_all("\\s*Penguin\\s*", "") %>%
      str_replace_all("Chin\\s*Strap", "Chinstrap") %>%
      str_extract("^(Adelie|Chinstrap|Gentoo)$")
  )

clean_survey %>% 
  filter(is.na(species_clean) | 
           !species_clean %in% c("Adelie", "Chinstrap", "Gentoo"))

# 7.3

validate_measurement <- function(measurement_string) {
  # Valid format: number (integer or decimal) followed by "mm" 
  # Examples: "39.1mm", "42mm", "18.7mm"
  
  valid_pattern <- "^\\d+(\\.\\d+)?mm$"
  
  if (str_detect(measurement_string, valid_pattern)) {
    # Extract numeric value
    numeric_value <- str_extract(measurement_string, "\\d+(\\.\\d+)?") %>% 
      as.numeric()
    return(list(valid = TRUE, value = numeric_value))
  } else {
    return(list(valid = FALSE, value = NA, 
                error = paste("Invalid format:", measurement_string)))
  }
}

# Test cases
test_inputs <- c("39.1mm", "42mm", "18.7mm", "39.1", "42 mm", "abc", "39.1cm")

map(test_inputs, validate_measurement)

# solution

validate_measurement <- function(measurement_string) {
  valid_pattern <- "^(\\d+|\\d*\\.\\d+)mm$"
  
  if (str_detect(measurement_string, valid_pattern)) {
    numeric_value <- str_extract(measurement_string, "\\d*\\.?\\d+") %>% 
      as.numeric()
    return(list(valid = TRUE, value = numeric_value))
  } else {
    return(list(
      valid = FALSE,
      value = NA,
      error = paste("Invalid format:", measurement_string)
    ))
  }
}