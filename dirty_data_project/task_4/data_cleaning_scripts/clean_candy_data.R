library(here)
library(dplyr)

#Read in the data - this is an xlsx format so we need the "readxl" library


boing_candy_2015 <- readxl::read_xlsx(here("raw_data/boing-boing-candy-2015.xlsx"))
boing_candy_2016 <- readxl::read_xlsx(here("raw_data/boing-boing-candy-2016.xlsx"))
boing_candy_2017 <- readxl::read_xlsx(here("raw_data/boing-boing-candy-2017.xlsx"))

boing_candy_2015 <- janitor::clean_names(boing_candy_2015)
boing_candy_2016 <- janitor::clean_names(boing_candy_2016)
names(boing_candy_2017) <- names(boing_candy_2017) %>% 
  str_remove_all("^Q[0-9]+[? :|]*")
boing_candy_2017 <- janitor::clean_names(boing_candy_2017)
#clean names

boing_candy_2016 <- boing_candy_2016 %>% 
  rename(sweetums = sweetums_a_friend_to_diabetes)

boing_candy_2017 <- boing_candy_2017 %>% 
  rename(sweetums = sweetums_a_friend_to_diabetes,
         are_you_going_actually_going_trick_or_treating_yourself = going_out,
         state = state_province_county_etc,
         anonymous_brown_globs_that_come_in_black_and_orange_wrappers = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes,)
#adding year columns
boing_candy_2015 <- boing_candy_2015 %>% 
  mutate(year = "2015", .before = 1)

boing_candy_2016 <- boing_candy_2016 %>% 
  mutate(year = "2016", .before = 1)

boing_candy_2017 <- boing_candy_2017 %>% 
  mutate(year = "2017", .before = 1)

#make sure datatypes for rows are right



#combine rows 



candy_combined_pre <- bind_rows(boing_candy_2016, boing_candy_2015) %>% 
  rename(
    age = how_old_are_you,
    gender = your_gender,
    country = which_country_do_you_live_in,
    state = which_state_province_county_do_you_live_in
  )



candy_combined <- bind_rows(candy_combined_pre, boing_candy_2017)





names(candy_combined)

candy_combined <- candy_combined %>% rename(
  attending_t_or_t = are_you_going_actually_going_trick_or_treating_yourself
)



class(boing_candy_2017$age)
class(candy_combined_pre$how_old_are_you)




#remove columns

to_remove <- c(13, 22, 23, 27, 28, 32, 44, 80, 102, 103, 105, 106, 108:124, 13, 140:155, 158, 159, 162:172)

candy_combined <- candy_combined %>% 
  select(!to_remove) 

# make ages numeric and impute NA values

candy_combined <- candy_combined %>% 
  mutate(
    age = str_replace_all(age, "[a-zA-Z]", NA_character_),
    age = as.numeric(age),
    age = case_when(
      is.na(age) ~ NA_real_,
      age > 100 ~ NA_real_,
      TRUE ~ age
    ),
    age = coalesce(age, mean(age, na.rm = TRUE))
  )

country_compare <- read_csv(url("https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv")) %>% 
  select(Name) %>% 
  mutate(Name = str_to_upper(Name))
here()
states_compare <- datasets::state.name
states_compare <- str_to_upper(states_compare)


candy_combined_country_sorted <- candy_combined %>% 
  mutate(
    country = str_to_upper(country),
    country = case_when(
      str_detect(country, "THE NETHERLANDS") ~ "THE NETHERLANDS",
      str_detect(country, "TAIWAN") ~ "TAIWAN",
      str_detect(country, "CANADA'") ~ "CANADA",
      str_detect(country, "UAE") ~ "UNITED ARAB EMIRATES",
      str_detect(country, "ESPAÑA") ~ "SPAIN",
      str_detect(country, "AUSTRALIA") ~ "AUSTRALIA",
      str_detect(country,"(?i)HUNGARY") ~ "HUNGARY",
      str_detect(country,"(?i)AUSTRIA") ~ "AUSTRIA",
      str_detect(country,"(?i)BRASIL") ~ "BRASIL",
      str_detect(country,"(?i)(the )?(un[iteds]?)(?! kingdom)") ~ "USA",
      str_detect(country,"ENGLAND|SCOTLAND|WALES|UK|U K|NORTHERN IRELAND|U.K.") ~ "UNITED KINGDOM",
      str_detect(country,"(?i)u.?s.?a?") ~ "USA",
      str_detect(country,"(?i)[a']?m[eu]rica") ~ "USA",
      str_detect(country,"amer") ~ "USA",
      str_detect(country,"KOREA") ~ "SOUTH KOREA",
      str_detect(country,"[0-9]") ~ NA_character_,
      str_count(country, ".") > 28 ~ NA_character_,
      country %in% country_compare$Name ~ country,
      country %in% states_compare ~ "USA",
      TRUE ~ NA_character_
    )
    
    
  )


write_csv(candy_combined_country_sorted, here("clean_data/candy_combined_sorted.csv"))

