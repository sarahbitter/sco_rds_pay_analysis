# title: Pay Range Distribution
# author: Sarah Bitter

# Description: Plot distribution of RDS pay ranges

# Load libraries
source(here::here("always_run","libraries.R"))

#### Load data ####
data <- read_feather(here::here("data_processed", "processed_data"))
####

#### Calculate distribution statistics ####
stats_data <- data %>% group_by(year, rds_class_range) %>% 
  mutate(regular_pay_median = median(regular_pay),
         regular_pay_mean = mean(regular_pay)) %>%
  ungroup() %>%
  relocate(regular_pay_median, regular_pay_mean, .after = regular_pay)


hist_2021 <- stats_data %>% filter(year == 2021) %>% ggplot(aes(x = regular_pay)) + 
  geom_histogram(binwidth = 10000, fill = "white", col = "#3B3E40") +
  geom_vline(aes(xintercept = max_position_salary), color = "blue", size = 1) +
  geom_vline(aes(xintercept =  min_position_salary), color = "red", size = 1) +
  geom_vline(aes(xintercept =  regular_pay_median), color = "black", size = 1) +
  geom_text(aes(x = regular_pay_median, y = 75, label = "median"), color = "black", size = 5, hjust = -.05) +
  geom_vline(aes(xintercept =  regular_pay_mean), color = "darkgreen", size = 1) +
  geom_text(aes(x = regular_pay_mean, y = 75, label = "mean"), color = "darkgreen", size = 5, hjust = 1.05, vjust = -2) +
  scale_x_continuous(breaks = seq(0, 150000, 20000), labels = paste0("$", seq(0, 150, 20), "k")) +
  facet_wrap(facets = c("rds_class_range")) +
  labs(title = "", subtitle = "") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.x = unit(0, "mm"),
        axis.ticks.length.y = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"))
####

data <- data %>% mutate(rds_class_range = ifelse(is.na(range), rds_class, paste0(rds_class, ", RANGE ", range))) %>%
  relocate(rds_class_range, .after = range)

#### 2021 Facet Box Plots ####
# Mark unusual pay ranges
data <- data %>% mutate(unusual_range = ifelse(grepl("SPECIALIST", rds_class) & max_position_salary < 30000,
                        TRUE,
                        ifelse(max_position_salary == 85104 & employer_name == "FINANCE, DEPARTMENT OF",
                        TRUE,
                        FALSE)))

data <- data %>% relocate(unusual_range, .before = min_position_salary)

# Pay range data
pay_ranges <- data %>% filter(!unusual_range) %>% select(year, 
                                                         rds_class_range,
                                                         min_position_salary, 
                                                         max_position_salary) %>%
  distinct() %>%
  arrange(rds_class_range, year)

# Remove non-RDS ranges
pay_ranges <- pay_ranges %>% filter(rds_class_range != "NA, RANGE C" & !is.na(rds_class_range))

# String wrap position names for easy graphing
data <- data %>% mutate(rds_class_range_print = str_wrap(rds_class_range, width = 8))

analyst_max_2021 <- pay_ranges %>% filter(year == 2021 & grepl("ANALYST", rds_class_range)) %>% select(rds_class_range, max_position_salary)

analyst_dist_2021 <- data %>% filter(grepl("ANALYST", rds_class) & year == 2021) %>% ggplot(aes(x = rds_class_range_print, y = regular_pay)) + 
  geom_jitter() +
  geom_segment(aes(x = 0.75, xend = 1.25, y = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE A"& pay_ranges$year == 2021,]$max_position_salary, yend = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE A" & pay_ranges$year == 2021,]$max_position_salary), color = "blue") +
  geom_segment(aes(x = 0.75, xend = 1.25, y = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE A"& pay_ranges$year == 2021,]$min_position_salary, yend = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE A" & pay_ranges$year == 2021,]$min_position_salary), color = "red") +
  geom_segment(aes(x = 1.75, xend = 2.25, y = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE B"& pay_ranges$year == 2021,]$max_position_salary, yend = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE B" & pay_ranges$year == 2021,]$max_position_salary), color = "blue") +
  geom_segment(aes(x = 1.75, xend = 2.25, y = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE B"& pay_ranges$year == 2021,]$min_position_salary, yend = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE B" & pay_ranges$year == 2021,]$min_position_salary), color = "red") +
  geom_segment(aes(x = 2.75, xend = 3.25, y = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE C"& pay_ranges$year == 2021,]$max_position_salary, yend = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE C" & pay_ranges$year == 2021,]$max_position_salary), color = "blue") +
  geom_segment(aes(x = 2.75, xend = 3.25, y = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE C"& pay_ranges$year == 2021,]$min_position_salary, yend = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE C" & pay_ranges$year == 2021,]$min_position_salary), color = "red") +
  geom_segment(aes(x = 3.75, xend = 4.25, y = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE N"& pay_ranges$year == 2021,]$max_position_salary, yend = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE N" & pay_ranges$year == 2021,]$max_position_salary), color = "blue") +
  geom_segment(aes(x = 3.75, xend = 4.25, y = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE N"& pay_ranges$year == 2021,]$min_position_salary, yend = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST I, RANGE N" & pay_ranges$year == 2021,]$min_position_salary), color = "red") +
  geom_segment(aes(x = 4.75, xend = 5.25, y = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST II, RANGE A"& pay_ranges$year == 2021,]$max_position_salary, yend = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST II, RANGE A" & pay_ranges$year == 2021,]$max_position_salary), color = "blue") +
  geom_segment(aes(x = 4.75, xend = 5.25, y = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST II, RANGE A"& pay_ranges$year == 2021,]$min_position_salary, yend = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST II, RANGE A" & pay_ranges$year == 2021,]$min_position_salary), color = "red") +
  geom_segment(aes(x = 5.75, xend = 6.25, y = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST II, RANGE L"& pay_ranges$year == 2021,]$max_position_salary, yend = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST II, RANGE L" & pay_ranges$year == 2021,]$max_position_salary), color = "blue") +
  geom_segment(aes(x = 5.75, xend = 6.25, y = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST II, RANGE L"& pay_ranges$year == 2021,]$min_position_salary, yend = pay_ranges[pay_ranges$rds_class_range == "RESEARCH DATA ANALYST II, RANGE L" & pay_ranges$year == 2021,]$min_position_salary), color = "red") +
  labs(title = "Regular pay for analysts is not fitting into pay ranges in 2021", subtitle = "Might be due to new hires or part time workers") +
  ylab("Regular Pay") +
  xlab("")


regular_pay_2021 <- data %>% filter(year == 2021) %>% ggplot(aes(x = 3, y = regular_pay)) + 
  geom_jitter() +
  geom_hline(aes(yintercept = max_position_salary), color = "blue") +
  geom_hline(aes(yintercept =  min_position_salary), color = "red") +
  scale_y_continuous(breaks = seq(0, 150000, 25000), labels = paste0("$", seq(0, 150, 25), "k")) +
  facet_wrap(facets = c("rds_class_range")) +
  labs(title = "Regular pay does not fit exactly into pay ranges in 2021", subtitle = "Might be due to new hires or part time workers") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.x = unit(0, "mm"),
        axis.ticks.length.y = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"),
        axis.text.x = element_blank())

regular_boxplot_2021 <- data %>% filter(year == 2021) %>% ggplot(aes(x = 3, y = regular_pay, group = rds_class_range)) + 
  geom_boxplot(color = "black") +
  geom_jitter(alpha = 0.25) +
  geom_hline(aes(yintercept = max_position_salary), color = "blue") +
  geom_hline(aes(yintercept =  min_position_salary), color = "red") +
  scale_y_continuous(breaks = seq(0, 150000, 25000), labels = paste0("$", seq(0, 150, 25), "k")) +
  facet_wrap(facets = c("rds_class_range")) +
  labs(title = "Regular pay does not fit exactly into pay ranges in 2021", subtitle = "Might be due to new hires or part time workers") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.x = unit(0, "mm"),
        axis.ticks.length.y = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"),
        axis.text.x = element_blank())

total_wages_2021 <- data %>% filter(year == 2021) %>% ggplot(aes(x = rds_class_range_print, y = total_wages)) + 
  geom_jitter() +
  geom_hline(aes(yintercept = max_position_salary), color = "blue") +
  geom_hline(aes(yintercept =  min_position_salary), color = "red") +
  scale_y_continuous(breaks = seq(0, 150000, 25000), labels = paste0("$", seq(0, 150, 25), "k")) +
  facet_wrap(facets = c("rds_class_range")) +
  labs(title = "Total wages in 2021 fall into pay ranges better, but still have tails") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.x = unit(0, "mm"),
        axis.ticks.length.y = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"),
        axis.text.x = element_blank())


regular_18_21 <- data %>% filter(year >= 2018) %>% ggplot(aes(x = year, y = regular_pay)) + 
  geom_jitter() +
  geom_point(aes(x = year, y = max_position_salary), color = "blue", size = 3) +
  geom_point(aes(x = year, y = min_position_salary), color = "red", size = 3) +
  scale_x_continuous(breaks = seq(2018, 2021, 1)) +
  scale_y_continuous(breaks = seq(0, 150000, 25000), labels = paste0("$", seq(0, 150, 25), "k")) +
  facet_wrap(facets = c("rds_class_range")) +
  labs(title = "Regular pay since RDS was formed") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.x = unit(0, "mm"),
        axis.ticks.length.y = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"))

regular_18_21 <- data %>% filter(year >= 2018) %>% ggplot(aes(x = year, y = regular_pay)) + 
  geom_jitter() +
  geom_point(aes(x = year, y = max_position_salary), color = "blue", size = 3) +
  geom_point(aes(x = year, y = min_position_salary), color = "red", size = 3) +
  scale_x_continuous(breaks = seq(2018, 2021, 1)) +
  scale_y_continuous(breaks = seq(0, 150000, 25000), labels = paste0("$", seq(0, 150, 25), "k")) +
  facet_wrap(facets = c("rds_class_range")) +
  labs(title = "Regular pay since RDS was formed") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.x = unit(0, "mm"),
        axis.ticks.length.y = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"))

regular_boxplot_09_21 <- data %>% filter(year >= 2018) %>% ggplot(aes(x = year, y = regular_pay, group = year)) + 
  geom_boxplot() +
  geom_jitter(alpha = .5) +
  geom_point(aes(x = year, y = max_position_salary), color = "blue", size = 3) +
  geom_point(aes(x = year, y = min_position_salary), color = "red", size = 3) +
  scale_x_continuous(breaks = seq(2018, 2021, 1)) +
  scale_y_continuous(breaks = seq(0, 150000, 25000), labels = paste0("$", seq(0, 150, 25), "k")) +
  facet_wrap(facets = c("rds_class_range")) +
  labs(title = "Regular pay since RDS was formed") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.x = unit(0, "mm"),
        axis.ticks.length.y = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"))

total_wages_18_21 <- data %>% filter(year >= 2018) %>% ggplot(aes(x = year, y = regular_pay)) + 
  geom_jitter() +
  geom_point(aes(x = year, y = max_position_salary), color = "blue", size = 3) +
  geom_point(aes(x = year, y = min_position_salary), color = "red", size = 3) +
  scale_x_continuous(breaks = seq(2018, 2021, 1)) +
  scale_y_continuous(breaks = seq(0, 175000, 25000), labels = paste0("$", seq(0, 175, 25), "k")) +
  facet_wrap(facets = c("rds_class_range")) +
  labs(title = "Total wages since RDS was formed") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.x = unit(0, "mm"),
        axis.ticks.length.y = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"))


regular_09_21 <- data %>% filter(!unusual_range & !is.na(rds_class_range) & rds_class_range != "NA, RANGE C") %>% ggplot(aes(x = year, y = regular_pay)) + 
  geom_jitter() +
  geom_point(aes(x = year, y = max_position_salary), color = "blue", size = 3) +
  geom_point(aes(x = year, y = min_position_salary), color = "red", size = 3) +
  scale_x_continuous(breaks = seq(2009, 2021, 1), labels = paste0("'", c("09", seq(10,21,1)))) +
  scale_y_continuous(breaks = seq(0, 150000, 25000), labels = paste0("$", seq(0, 150, 25), "k")) +
  facet_wrap(facets = c("rds_class_range"), ncol = 6) +
  labs(title = "Regular pay across all years") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.x = unit(0, "mm"),
        axis.ticks.length.y = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"))

total_wages_09_21 <- data %>% filter(!unusual_range & !is.na(rds_class_range) & rds_class_range != "NA, RANGE C") %>% ggplot(aes(x = year, y = total_wages)) + 
  geom_jitter() +
  geom_point(aes(x = year, y = max_position_salary), color = "blue", size = 3) +
  geom_point(aes(x = year, y = min_position_salary), color = "red", size = 3) +
  scale_x_continuous(breaks = seq(2009, 2021, 1), labels = paste0("'", c("09", seq(10,21,1)))) +
  scale_y_continuous(breaks = seq(0, 175000, 25000), labels = paste0("$", seq(0, 175, 25), "k")) +
  facet_wrap(facets = c("rds_class_range"), ncol = 6) +
  labs(title = "Total wages across all years") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.x = unit(0, "mm"),
        axis.ticks.length.y = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"))

regular_boxplot_09_21 <- data %>% filter(!unusual_range & !is.na(rds_class_range) & rds_class_range != "NA, RANGE C") %>% ggplot(aes(x = year, y = regular_pay, group = year)) + 
  geom_jitter() +
  geom_boxplot() +
  geom_point(aes(x = year, y = max_position_salary), color = "blue", size = 3) +
  geom_point(aes(x = year, y = min_position_salary), color = "red", size = 3) +
  scale_x_continuous(breaks = seq(2009, 2021, 1), labels = paste0("'", c("09", seq(10,21,1)))) +
  scale_y_continuous(breaks = seq(0, 150000, 25000), labels = paste0("$", seq(0, 150, 25), "k")) +
  facet_wrap(facets = c("rds_class_range"), ncol = 6) +
  labs(title = "Regular pay across all years") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.x = unit(0, "mm"),
        axis.ticks.length.y = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"))

#### Quick look at employee totals ####
total_employees_class_range <- data %>% count(year, rds_class_range)

total_employees_chart <- total_employees_class_range %>% ggplot(aes(x = year, y = n)) +
  facet_wrap(facets = c("rds_class_range"))

total_employees_class_range_dept <- data %>% count(year, employer_name, rds_class_range)
