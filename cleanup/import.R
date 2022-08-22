# title: Import and Clean Raw Data
# author: Sarah Bitter

# load libraries
source(here::here("always_run","libraries.R"))

#### Load, Clean, and Import Workforce Data ####
# Load each year's data into list
data <- vector("list", 13)
year_range <- as.character(seq(2009, 2021,1))
data <- data %>% setNames(year_range)
for (year in year_range) {
  file_name <- paste0(year, "_StateDepartment.csv")
  data[[year]] <- read_csv(here::here("data_raw", file_name), # TODO change to feather?
                        na = "NA")
}

# Combine each year's data into one dataset
data_all_years_dataframe <- data %>% bind_rows()

# Write to feather format
data_all_years_dataframe %>% write_feather(here::here("data_processed", "data_09_21"))
