# title: Pay Range Analysis
# author: Sarah Bitter

# Description: Determine uniqueness of pay ranges - do they vary by year and position,
# year, position, and employer, or some other subset of variables?

# Load libraries
source(here::here("always_run","libraries.R"))

#### Load data ####
data <- read_feather(here::here("data_processed", "processed_data"))
####

#### Investigate uniqueness of pay ranges ####
unique_data_all_categories <- data %>% select(year, 
                               employer_name, 
                               rds_name, 
                               rds_class, 
                               range, 
                               position, 
                               min_position_salary, 
                               max_position_salary) %>%
                              distinct()
unique_data <- data %>% select(rds_class, 
                               range,
                               min_position_salary, 
                               max_position_salary) %>%
  distinct()

unique_data <- unique_data %>% arrange(rds_class, range)

# Unique data with year most informative:
unique_data_w_year <- data %>% select(year, 
                                      rds_class, 
                                      range,
                                      min_position_salary, 
                                      max_position_salary) %>%
  distinct()

unique_data_w_year <- unique_data_w_year %>% arrange(rds_class, range, year)

# Graph min/max salary by class and range over time
data <- data %>% mutate(rds_class_range = ifelse(is.na(range), rds_class, paste0(rds_class, ", RANGE ", range))) %>%
  relocate(rds_class_range, .after = range)

analyst_range_plots <- data %>% filter(grepl("RESEARCH DATA ANALYST", rds_class)) %>% ggplot(aes(x = year)) + 
  geom_point(aes(y = min_position_salary, color = "Min. Position Salary")) + 
  geom_point(aes(y = max_position_salary,  color = "Max. Position Salary")) +
  scale_x_continuous(breaks = seq(2009, 2021, 1), labels = paste0("'", c("09", seq(10, 21, 1)))) +
  scale_y_continuous(breaks = seq(30000, 90000, 10000), labels = paste0("$", seq(30, 90, 10), "k")) +
  xlab("") +
  ylab("") +
  labs(title = "Analyst pay ranges change only with year", color = "Legend") +
  facet_wrap(facets = c("rds_class_range"))

specialist_range_plots <- data %>% filter(grepl("RESEARCH DATA SPECIALIST", rds_class)) %>% ggplot(aes(x = year)) + 
  geom_point(aes(y = min_position_salary, color = "Min. Position Salary")) + 
  geom_point(aes(y = max_position_salary,  color = "Max. Position Salary")) +
  scale_x_continuous(breaks = seq(2009, 2021, 1), labels = paste0("'", c("09", seq(10, 21, 1)))) +
  scale_y_continuous(breaks = seq(0, 90000, 15000), labels = paste0("$", seq(0, 90, 15), "k")) +
  xlab("") +
  ylab("") +
  labs(title = "Specialist pay ranges are NOT unique by position and year", subtitle = "May be due to data entry error", color = "Legend") +
  facet_wrap(facets = c("rds_class_range"))

supervisor_manager_range_plots <- data %>% filter(grepl("RESEARCH DATA SUPERVISOR", rds_class) | grepl("RESEARCH DATA MANAGER", rds_class)) %>% ggplot(aes(x = year)) + 
  geom_point(aes(y = min_position_salary, color = "Min. Position Salary")) + 
  geom_point(aes(y = max_position_salary,  color = "Max. Position Salary")) +
  scale_x_continuous(breaks = seq(2009, 2021, 1), labels = paste0("'", c("09", seq(10, 21, 1)))) +
  scale_y_continuous(breaks = seq(70000, 110000, 10000), labels = paste0("$", seq(70, 110, 10), "k")) +
  xlab("") +
  ylab("") +
  labs(title = "Manager and supervisor pay ranges change only with year", color = "Legend") +
  facet_wrap(facets = c("rds_class_range"))

analyst_manager_supervisor <- data %>% filter(grepl("RESEARCH DATA ANALYST", rds_class) | grepl("RESEARCH DATA SUPERVISOR", rds_class) | grepl("RESEARCH DATA MANAGER", rds_class)) %>% ggplot(aes(x = year)) + 
  geom_point(aes(y = min_position_salary, color = "Min. Position Salary")) + 
  geom_point(aes(y = max_position_salary,  color = "Max. Position Salary")) +
  scale_x_continuous(breaks = seq(2009, 2021, 1), labels = paste0("'", c("09", seq(10, 21, 1)))) +
  scale_y_continuous(breaks = seq(30000, 110000, 15000), labels = paste0("$", seq(30, 110, 15), "k")) +
  xlab("") +
  ylab("") +
  labs(title = "Analyst, manager, and supervisor pay ranges change only with year", color = "Legend") +
  facet_wrap(facets = c("rds_class_range"))
####

#### Specialist Range Variations ####
# Zoom in on max 
specialist_discrepancies <-  data %>% filter(grepl("RESEARCH DATA SPECIALIST III", rds_class) & is.na(range) & year <= 2012) %>% ggplot(aes(x = year)) + 
  geom_point(aes(y = min_position_salary), color = "blue") + 
  geom_point(aes(y = max_position_salary), color = "orange") +
  # geom_text(aes(label = ifelse(min_position_salary < 70000, min_position_salary, "")), vjust = 1) +
  # geom_text(aes(label = ifelse(min_position_salary > 70000, min_position_salary, "")), vjust = -1) +
  scale_x_continuous(breaks = seq(2009, 2021, 1), labels = paste0("'", c("09", seq(10, 21, 1)))) +
  scale_y_continuous(breaks = seq(70000, 85000, 5000), labels = paste0("$", seq(70, 85, 5), "k")) +
  xlab("") +
  ylab("") +
  labs(color = "Legend") + 
  facet_zoom(ylim = c(85000, 85150), zoom.size = .75) #+
  #facet_zoom(ylim = c(69950, 70050), zoom.size = .75)
####