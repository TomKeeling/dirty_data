
library(here)
library(dplyr)

rwa_data <- read_csv(here("dirty_data/rwa.csv")) %>% 
  janitor::clean_names()

#some answers were recorded with a reversed value system - this can be reversed with 10-.x as the value.


edit <- c("q4", "q6", "q8", "q9", "q11", "q13", "q15", "q18", "q20", "q21")

rwa_data <- rwa_data %>% 
  mutate(across(edit, ~ 10-.x)
  ) 

#others were a simple yes or no answer - this can be recorded as a logical input.

rwa_data <- rwa_data %>% 
  mutate(across(vcl1:vcl16, ~ as.logical(.x))) 

#some of these will be requiring more detailed information for analysis.

rwa_data <- rwa_data %>% 
  mutate(
    gender = case_when(
      gender == 1 ~ "m",
      gender == 2 ~ "f",
      gender == 3 ~ "other",
      TRUE ~ NA_character_
    ),
    hand = case_when(
      hand == 1 ~ "r",
      hand == 2 ~ "l",
      hand == 3 ~ "ambi",
      TRUE ~ NA_character_
    ),
    
    urban = case_when(
      urban == 1 ~ "rural",
      urban == 2 ~ "suburban",
      urban == 3 ~ "urban",
      TRUE ~ NA_character_
    ),
    education = case_when(
      education == 1 ~ "less than highschool",
      education== 2 ~ "high school",
      education == 3 ~ "university degree",
      education == 4 ~ "graduate degree",
      TRUE ~ NA_character_
    )
  )

#a totals column will also be required for analysis

rwa_data <- rwa_data %>% 
  mutate(
    total_score = rowSums(.[3:22]), .before = q1
  )

rwa_data %>% 
write_csv(here("clean_data/rwa_clean.csv"))

  