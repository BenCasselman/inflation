# This code generates monthly 'relative importance' values (weights) for all products
# in the Consumer Price Index.

# BLS updates its weights each year in December, and provides them here: https://www.bls.gov/cpi/tables/relative-importance/home.htm#Archived%20Relative%20Importance%20Data
# Unfortunately, unlike the index values themselves, BLS  doesn't provide the weights 
# in convenient tabular format, and (far more annoyingly) doesn't provide item codes.
# So we have to first download the annual relative importance files and match the produce
# names with those in a separate file that has the codes.
# Then, once we have the annual data, we have to calculate the non-December monthly values.

library(tidyverse)
library(lubridate)

# First, get the file with the CPI item codes from this page: https://download.bls.gov/pub/time.series/cu/
# We'll be coming back here later for the index values.
cpi_item_match <- read_tsv("https://download.bls.gov/pub/time.series/cu/cu.item")

# We're going to simplify the formatting of the names to aid in matching.
cpi_item_match <- cpi_item_match %>% 
  select(item_code, item_name) %>% 
  mutate(item_name = gsub(",", "", item_name),
         item_name = gsub("'", "", item_name),
         item_name = tolower(item_name))

# Now we start downloading the relative importance files. We have to do this one-by-one
# to get the matching right.

# 2022
# This one and 2020/21 are available in .xlsx format
temp <- tempfile(fileext = ".xlsx")
download.file("https://www.bls.gov/cpi/tables/relative-importance/2022.xlsx", temp, mode = "wb")

wts_2022 <- readxl::read_xlsx(temp, sheet = 1, skip = 9, 
                              col_names = c("indent_level", "item_name", "wt", "w_wt")) # Note: 'wt' is the weight for the CPI-U. 'wt_w' is for CPI-W.

# We're going to create a new variable "match_name" to match with the file with the codes.
# (Leaving the original item name unchanged.)
wts_2022 <- wts_2022 %>% 
  mutate(rownum = row_number(), # Row number will be helpful as we're doing the matching
         match_name = gsub(",", "", item_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name)) %>% 
  filter(!is.na(wt)) %>% 
  select(rownum, item_name, wt, w_wt, match_name)

# Now we match these on the item names and see which ones fail to parse.
# Commenting out because you shouldn't need to do this.

# wts_2022 %>%
#   left_join(cpi_item_match, by = c("match_name" = "item_name")) %>%
#   filter(is.na(item_code),
#          !grepl("Unsampled", item_name))

# Now we join these:
wts_2022 <- wts_2022 %>% 
  left_join(cpi_item_match, by = c("match_name" = "item_name")) %>% 
  mutate(date = ymd("2022-12-01")) %>% # add a date since eventually we're merging these files
  filter(!duplicated(item_code))

# Check this carefully!
# wts_2022


# 2021
# This one and 2020 are available in .xlsx format
temp <- tempfile(fileext = ".xlsx")
download.file("https://www.bls.gov/cpi/tables/relative-importance/2021.xlsx", temp, mode = "wb")

wts_2021 <- readxl::read_xlsx(temp, sheet = 1, skip = 9, 
                              col_names = c("indent_level", "item_name", "wt", "w_wt")) # Note: 'wt' is the weight for the CPI-U. 'wt_w' is for CPI-W.

# We're going to create a new variable "match_name" to match with the file with the codes.
# (Leaving the original item name unchanged.)
wts_2021 <- wts_2021 %>% 
  mutate(rownum = row_number(), # Row number will be helpful as we're doing the matching
         match_name = gsub(",", "", item_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name)) %>% 
  filter(!is.na(wt)) %>% 
  select(rownum, item_name, wt, w_wt, match_name)

# Now we match these on the item names and see which ones fail to parse.
# Commenting out because you shouldn't need to do this.

# wts_2021 %>%
#   left_join(cpi_item_match, by = c("match_name" = "item_name")) %>%
#   filter(is.na(item_code),
#          !grepl("Unsampled", item_name))

# Hand-fix a few items -- there aren't that many! 
# We're matching to the names in the `cpi_item_match` file. (We're editing the 'match_name' 
# variable, not the original item name.)
# Some of these it's obvious how they match, but a few (day care == child care?) I had to
# double-check.
wts_2021 <- wts_2021 %>% 
  mutate(match_name = case_when(rownum == 165 ~ "Women's underwear, nightwear, swimwear and accessories",
                                rownum == 266 ~ "Day care and preschool",
                                rownum == 278 ~ "Computers, peripherals, and smart home assistants",
                                TRUE ~ match_name),
         match_name = gsub(",", "", match_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name))

# Now we join these:
wts_2021 <- wts_2021 %>% 
  left_join(cpi_item_match, by = c("match_name" = "item_name")) %>% 
  mutate(date = ymd("2021-12-01")) %>% # add a date since eventually we're merging these files
  filter(!duplicated(item_code))

# Check this carefully!
# wts_2021



# 2020
# This one (and only this one) is available in .xlsx
temp <- tempfile(fileext = ".xlsx")
download.file("https://www.bls.gov/cpi/tables/relative-importance/2020.xlsx", temp, mode = "wb")

wts_2020 <- readxl::read_xlsx(temp, sheet = 1, skip = 9, 
                              col_names = c("indent_level", "item_name", "wt", "w_wt")) # Note: 'wt' is the weight for the CPI-U. 'wt_w' is for CPI-W.

# We're going to create a new variable "match_name" to match with the file with the codes.
# (Leaving the original item name unchanged.)
wts_2020 <- wts_2020 %>% 
  mutate(rownum = row_number(), # Row number will be helpful as we're doing the matching
         match_name = gsub(",", "", item_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name)) %>% 
  filter(!is.na(wt)) %>% 
  select(rownum, item_name, wt, w_wt, match_name)

# Now we match these on the item names and see which ones fail to parse.
# Commenting out because you shouldn't need to do this.

# wts_2020 %>% 
#   left_join(cpi_item_match, by = c("match_name" = "item_name")) %>% 
#   filter(is.na(item_code),
#          !grepl("Unsampled", item_name))

# Hand-fix a few items -- there aren't that many! 
# We're matching to the names in the `cpi_item_match` file. (We're editing the 'match_name' 
# variable, not the original item name.)
# Some of these it's obvious how they match, but a few (day care == child care?) I had to
# double-check.
wts_2020 <- wts_2020 %>% 
  mutate(match_name = case_when(rownum == 161 ~ "Women's underwear, nightwear, swimwear and accessories",
                                rownum == 258 ~ "Day care and preschool",
                                rownum == 270 ~ "Computers, peripherals, and smart home assistants",
                                TRUE ~ match_name),
         match_name = gsub(",", "", match_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name))

# Now we join these:
wts_2020 <- wts_2020 %>% 
  left_join(cpi_item_match, by = c("match_name" = "item_name")) %>% 
  mutate(date = ymd("2020-12-01")) %>% # add a date since eventually we're merging these files
  filter(!duplicated(item_code))

# Check this carefully!
# wts_2020


# 2019
# The rest of these are in .txt files:
wts_2019 <- read_table("https://www.bls.gov/cpi/tables/relative-importance/2019.txt",
                     skip = 15, col_names = FALSE)

# These files need a fair bit of cleaning up:
wts_2019 <- wts_2019 %>% 
  filter(X1 != "") %>% 
  rename(item_name = X1, wt = X2, w_wt = X3) %>% 
  mutate(item_name = gsub("\\.", "", item_name),
         rownum = row_number(),
         item_name = case_when(rownum == 1 ~ item_name,
                               is.na(lag(wt, 1, na.pad = T)) & is.na(lag(wt, 2, na.pad = T)) ~ paste(lag(item_name, 2), lag(item_name, 1), item_name, sep = " "),
                               is.na(lag(wt, 1, na.pad = T)) ~ paste(lag(item_name, 1, na.pad = T), item_name, sep = " "),
                        TRUE ~ item_name),
         match_name = gsub(",", "", item_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name)) %>% 
  filter(!is.na(wt)) %>% 
  select(rownum, item_name, wt, w_wt, match_name)

# Same approach:
# wts_2019 %>% 
#   left_join(cpi_item_match, by = c("match_name" = "item_name")) %>% 
#   filter(is.na(item_code),
#          !grepl("Unsampled", item_name))

# hand-fix items -- only a few now! (Preserve original name in item_name field)
wts_2019 <- wts_2019 %>% 
  mutate(match_name = case_when(rownum == 268 ~ "Day care and preschool",
                               rownum == 282 ~ "Computers, peripherals, and smart home assistants",
                               TRUE ~ match_name),
         match_name = gsub(",", "", match_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name))

wts_2019 <- wts_2019 %>% 
  left_join(cpi_item_match, by = c("match_name" = "item_name")) %>% 
  mutate(date = ymd("2019-12-01"))

# wts_2019

# 2018
# Basically the same as 2019

wts_2018 <- read_table("https://www.bls.gov/cpi/tables/relative-importance/2018.txt",
                       skip = 15, col_names = FALSE)

wts_2018 <- wts_2018 %>% 
  filter(X1 != "") %>% 
  rename(item_name = X1, wt = X2, w_wt = X3) %>% 
  mutate(item_name = gsub("\\.", "", item_name),
         rownum = row_number(),
         item_name = case_when(rownum == 1 ~ item_name,
                               is.na(lag(wt, 1, na.pad = T)) & is.na(lag(wt, 2, na.pad = T)) ~ paste(lag(item_name, 2), lag(item_name, 1), item_name, sep = " "),
                               is.na(lag(wt, 1, na.pad = T)) ~ paste(lag(item_name, 1, na.pad = T), item_name, sep = " "),
                               TRUE ~ item_name),
         match_name = gsub(",", "", item_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name)) %>% 
  filter(!is.na(wt)) %>% 
  select(rownum, item_name, wt, w_wt, match_name)

# wts_2018 %>% 
#   left_join(cpi_item_match, by = c("match_name" = "item_name")) %>% 
#   filter(is.na(item_code),
#          !grepl("Unsampled", item_name))

# hand-fix items
wts_2018 <- wts_2018 %>% 
  mutate(match_name = case_when(rownum == 69 ~ "Sugar and sugar substitutes",
                               rownum == 156 ~ "Men's underwear, nightwear, swimwear and accessories",
                               rownum == 167 ~ "Women's underwear, nightwear, swimwear and accessories",
                               rownum == 267 ~ "Day care and preschool",
                               rownum == 280 ~ "Computers, peripherals, and smart home assistants",
                               TRUE ~ match_name),
         match_name = gsub(",", "", match_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name))

wts_2018 <- wts_2018 %>% 
  left_join(cpi_item_match, by = c("match_name" = "item_name")) %>% 
  mutate(date = ymd("2018-12-01"))

# wts_2018

# 2017

wts_2017 <- read_table("https://www.bls.gov/cpi/tables/relative-importance/2017.txt",
                       skip = 15, col_names = FALSE)

wts_2017 <- wts_2017 %>% 
  filter(X1 != "") %>% 
  rename(item_name = X1, wt = X2, w_wt = X3) %>% 
  mutate(item_name = gsub("\\.", "", item_name),
         rownum = row_number(),
         item_name = case_when(rownum == 1 ~ item_name,
                               is.na(lag(wt, 1, na.pad = T)) & is.na(lag(wt, 2, na.pad = T)) ~ paste(lag(item_name, 2), lag(item_name, 1), item_name, sep = " "),
                               is.na(lag(wt, 1, na.pad = T)) ~ paste(lag(item_name, 1, na.pad = T), item_name, sep = " "),
                               TRUE ~ item_name),
         match_name = gsub(",", "", item_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name)) %>% 
  filter(!is.na(wt)) %>% 
  select(rownum, item_name, wt, w_wt, match_name)

wts_2017 %>%
  left_join(cpi_item_match, by = c("match_name" = "item_name")) %>%
  filter(is.na(item_code),
         !grepl("Unsampled", item_name))

# hand-fix items
wts_2017 <- wts_2017 %>% 
  mutate(match_name = case_when(rownum == 28 ~ "Other pork including roasts, steaks, and ribs",
                                rownum == 32 ~ "Other uncooked poultry including turkey",
                                rownum == 69 ~ "Sugar and sugar substitutes",
                                rownum == 156 ~ "Men's underwear, nightwear, swimwear and accessories",
                                rownum == 167 ~ "Women's underwear, nightwear, swimwear and accessories",
                                rownum == 243 ~ "Photographers and photo processing",
                                rownum == 267 ~ "Day care and preschool",
                                rownum == 280 ~ "Computers, peripherals, and smart home assistants",
                                TRUE ~ match_name),
         match_name = gsub(",", "", match_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name))

wts_2017 <- wts_2017 %>% 
  left_join(cpi_item_match, by = c("match_name" = "item_name")) %>% 
  mutate(date = ymd("2017-12-01"))

# wts_2017

# 2016
wts_2016 <- read_table("https://www.bls.gov/cpi/tables/relative-importance/2016.txt",
                       skip = 14, col_names = FALSE) # Make sure to double-check you're skipping the right number of rows!

wts_2016 <- wts_2016 %>% 
  filter(X1 != "") %>% 
  rename(item_name = X1, wt = X2, w_wt = X3) %>% 
  mutate(item_name = gsub("\\.", "", item_name),
         rownum = row_number(),
         item_name = case_when(rownum == 1 ~ item_name,
                               is.na(lag(wt, 1, na.pad = T)) & is.na(lag(wt, 2, na.pad = T)) ~ paste(lag(item_name, 2), lag(item_name, 1), item_name, sep = " "),
                               is.na(lag(wt, 1, na.pad = T)) ~ paste(lag(item_name, 1, na.pad = T), item_name, sep = " "),
                               TRUE ~ item_name),
         match_name = gsub(",", "", item_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name)) %>% 
  filter(!is.na(wt)) %>% 
  select(rownum, item_name, wt, w_wt, match_name)

wts_2016 %>%
  left_join(cpi_item_match, by = c("match_name" = "item_name")) %>%
  filter(is.na(item_code),
         !grepl("Unsampled", item_name))

# hand-fix items
wts_2016 <- wts_2016 %>% 
  mutate(match_name = case_when(rownum == 28 ~ "Other pork including roasts, steaks, and ribs",
                                rownum == 32 ~ "Other uncooked poultry including turkey",
                                rownum == 69 ~ "Sugar and sugar substitutes",
                                rownum == 156 ~ "Men's underwear, nightwear, swimwear and accessories",
                                rownum == 167 ~ "Women's underwear, nightwear, swimwear and accessories",
                                rownum == 203 ~ "Airline fares",
                                rownum == 227 ~ "Cable and satellite television service",
                                rownum == 230 ~ "Video discs and other media, including rental of video",
                                rownum == 232 ~ "Recorded music and music subscriptions",
                                rownum == 243 ~ "Photographers and photo processing",
                                rownum == 252 ~ "Club membership for shopping clubs, fraternal, or other organizations, or participant sports fees",
                                rownum == 266 ~ "Day care and preschool",
                                rownum == 279 ~ "Computers, peripherals, and smart home assistants",
                                TRUE ~ match_name),
         match_name = gsub(",", "", match_name),
         match_name = gsub("'", "", match_name),
         match_name = tolower(match_name))

wts_2016 <- wts_2016 %>% 
  left_join(cpi_item_match, by = c("match_name" = "item_name")) %>% 
  mutate(date = ymd("2016-12-01"))

# wts_2016


# Combine these into a single dataframe:
wts_all <- bind_rows(wts_2016, wts_2017, wts_2018, wts_2019, wts_2020, wts_2021, wts_2022) %>% 
  select(-rownum) 

# rm(wts_2016, wts_2017, wts_2018, wts_2019, wts_2020)

# We'll calculate the intra-December months using the methodology described here: https://www.bls.gov/cpi/tables/relative-importance/home.htm
# That requires the CPI index values.

# Download full CPI dataset:
cpi_data <- read_tsv("https://download.bls.gov/pub/time.series/cu/cu.data.0.Current") %>% 
  mutate(date = ymd(paste(year, substr(period, 2,3), "01", sep = "-")))

# Also the series codes
cpi_series <- read_tsv("https://download.bls.gov/pub/time.series/cu/cu.series")

# Now merge these together. We can also filter out the series we don't need here to make 
# this dataset more manageable.

cpi_wts <- cpi_data %>% 
  filter(date >= ymd("2016-12-01")) %>% # Adjust this if you download more years of weights
  left_join(cpi_series, by = "series_id") %>% 
  filter(seasonal == "U",
         area_code == "0000",
         periodicity_code == "R") %>% 
  select(date, series_id, item_code, value)

# rm(cpi_data, cpi_series)

# And merge in the weights that we already have, by date and item code
cpi_wts <- left_join(cpi_wts,
                     wts_all %>% select(-match_name),
                     by = c("date", "item_code"))

# cpi_wts

# Now we need to calculate the weights for the missing dates. There's probably a cleaner
# way to do this, but this way works.

# We calculate the updated weights by looking at the change in index values from the December
# periods where we DO have weights. So we need to establish what the relevant baseline
# is for each date.

cpi_wts <- cpi_wts %>% 
  mutate(base_wt = wt,
         base_w_wt = w_wt,
         base_val = case_when(is.na(wt) ~ NA_real_,
                              TRUE ~ value),
         base_date = case_when(is.na(wt) ~ NA_character_,
                               TRUE ~ as.character(date))) %>% 
  group_by(series_id) %>% 
  fill(base_wt, base_w_wt, base_val, base_date) %>% 
  mutate(base_date = ymd(base_date))

# We also need the "all items" value for both periods so we can normalize these to 100:
cpi_wts <- cpi_data %>% 
  filter(series_id == "CUUR0000SA0") %>% 
  select(date, all_items = value) %>% 
  left_join(cpi_wts, ., by = "date")

cpi_wts <- cpi_data %>% 
  filter(series_id == "CUUR0000SA0") %>% 
  select(date, all_items_base = value) %>% 
  left_join(cpi_wts, ., by = c("base_date" = "date"))

# Now actually calculate the missing weights. Can do this for both `wt` and `w_wt`
cpi_wts <- cpi_wts %>% 
  group_by(series_id) %>% 
  mutate(wt = case_when(!is.na(wt) ~ wt,
                        TRUE ~ base_wt * (value/base_val) / (all_items/all_items_base)),
         w_wt = case_when(!is.na(w_wt) ~ w_wt,
                        TRUE ~ base_w_wt * (value/base_val) / (all_items/all_items_base))) 

# Just want the item codes and the weights for each date. Note that we use these weights
# for both NSA *and* SA series, so we don't actually want the series_ids here.
cpi_wts <- cpi_wts %>% 
  ungroup() %>% 
  select(date, item_code, wt, w_wt) %>% 
  filter(!is.na(wt))

# Spot-check against weights in latest CPI release:
cpi_wts %>%
  filter(item_code == "SETA02") %>%
  arrange(desc(date))

# Save and output to .csv
save(cpi_wts, file = "cpi_wts.RData")
cpi_wts %>% 
  write_csv("cpi_weights.csv")



# Function for updating with new data. Assumes we're using all the dataframes above.
# (At some point I'll rewrite this to be a bit more generalized.)
wt_update <- function() {
  
  cpi_wts <- cpi_data %>% 
    filter(date >= ymd("2016-12-01")) %>% # Adjust this if you download more years of weights
    left_join(cpi_series, by = "series_id") %>% 
    filter(seasonal == "U",
           area_code == "0000",
           periodicity_code == "R") %>% 
    select(date, series_id, item_code, value)
  
  # rm(cpi_data, cpi_series)
  
  # And merge in the weights that we already have, by date and item code
  cpi_wts <- left_join(cpi_wts,
                       wts_all %>% select(-match_name),
                       by = c("date", "item_code"))
  
  # Now we need to calculate the weights for the missing dates. There's probably a cleaner
  # way to do this, but this way works.
  
  # We calculate the updated weights by looking at the change in index values from the December
  # periods where we DO have weights. So we need to establish what the relevant baseline
  # is for each date.
  
  cpi_wts <- cpi_wts %>% 
    mutate(base_wt = wt,
           base_w_wt = w_wt,
           base_val = case_when(is.na(wt) ~ NA_real_,
                                TRUE ~ value),
           base_date = case_when(is.na(wt) ~ NA_character_,
                                 TRUE ~ as.character(date))) %>% 
    group_by(series_id) %>% 
    fill(base_wt, base_w_wt, base_val, base_date) %>% 
    mutate(base_date = ymd(base_date))
  
  # We also need the "all items" value for both periods so we can normalize these to 100:
  cpi_wts <- cpi_data %>% 
    filter(series_id == "CUUR0000SA0") %>% 
    select(date, all_items = value) %>% 
    left_join(cpi_wts, ., by = "date")
  
  cpi_wts <- cpi_data %>% 
    filter(series_id == "CUUR0000SA0") %>% 
    select(date, all_items_base = value) %>% 
    left_join(cpi_wts, ., by = c("base_date" = "date"))
  
  # Now actually calculate the missing weights. Can do this for both `wt` and `w_wt`
  cpi_wts <- cpi_wts %>% 
    group_by(series_id) %>% 
    mutate(wt = case_when(!is.na(wt) ~ wt,
                          TRUE ~ base_wt * (value/base_val) / (all_items/all_items_base)),
           w_wt = case_when(!is.na(w_wt) ~ w_wt,
                            TRUE ~ base_w_wt * (value/base_val) / (all_items/all_items_base))) 
  
  # Just want the item codes and the weights for each date. Note that we use these weights
  # for both NSA *and* SA series, so we don't actually want the series_ids here.
  cpi_wts <- cpi_wts %>% 
    ungroup() %>% 
    select(date, item_code, wt, w_wt) %>% 
    filter(!is.na(wt))
  
  cpi_wts
  
}