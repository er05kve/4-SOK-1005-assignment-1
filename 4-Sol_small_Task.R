
rm(list=ls())


library(tidyverse)
library(lubridate)
library(zoo)


urls <- list(
  "Lower_Troposphere" = "https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt",
  "Mid_Troposphere" = "https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt",
  "Tropopause" = "https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt",
  "Lower_Stratosphere" = "https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt"
)


column_names <- c("Year", "Month", "Globe", "Land", "Ocean", "Extra1", "Extra2", "Extra3", "Extra4", "Extra5")


process_data <- function(url) {
  df <- read_table2(url, col_names = column_names, skip = 1)
  
 
  last_row <- which(df$Year == "Year")[1] - 1
  df <- df[1:last_row, ]
  
 
  df <- df %>%
    mutate(across(c(Year, Month, Globe), as.numeric)) %>%
    mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>%
    filter(Date >= "1980-01-01") 
  

  df <- df %>% mutate(Temp_MA = rollmean(Globe, 12, fill = NA, align = "right"))
  
  return(df %>% select(Date, Temp_MA))
}


data_list <- map(urls, process_data)


merged_data <- reduce(data_list, left_join, by = "Date")
colnames(merged_data) <- c("Date", "Lower_Troposphere", "Mid_Troposphere", "Tropopause", "Lower_Stratosphere")


merged_data <- merged_data %>%
  mutate(Average = rowMeans(select(., Lower_Troposphere:Lower_Stratosphere), na.rm = TRUE))

merged_data <- merged_data %>% drop_na()


ggplot(merged_data, aes(x = Date)) +
  geom_line(aes(y = Lower_Troposphere, color = "Lower Troposphere"), linewidth = 1.3) +
  geom_line(aes(y = Mid_Troposphere, color = "Mid Troposphere"), linewidth = 1.3) +
  geom_line(aes(y = Tropopause, color = "Tropopause"), linewidth = 1.3) +
  geom_line(aes(y = Lower_Stratosphere, color = "Lower Stratosphere"), linewidth = 1.3) +
  geom_line(aes(y = Average, color = "Average"), linetype = "dashed", linewidth = 1.7) +  
    title = "Global Temperature Average",
    x = "Year",
    y = "Temperature",
    color = "Atmospheric Layer"
  ) +
  theme_minimal()

