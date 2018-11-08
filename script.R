#### CONVENTIONAL SCRIPT FILE

## Access, clean and plot APRA Annual Fund Level Superannuation Statistics ## 

## CODE CHUNK 1: Access data ----

# Libraries
library(tidyverse)
library(pander)
library(readxl)
library(PCcharts)
options(scipen = 999) 

# Download APRA data
if("2018-AFLSS-201706.xlsx" %in% list.files() == FALSE) { 
   link <- "https://www.apra.gov.au/sites/default/files/2018-AFLSS-201706.xlsx"
   download.file(link, "2018-AFLSS-201706.xlsx", mode = "wb")
}

# Import Table 2
table2 <- read_xlsx("2018-AFLSS-201706.xlsx", sheet = "Table 2", range = "A5:AA206", col_names = TRUE, trim_ws = TRUE) 
table2 <- table2 %>% 
   filter(!is.na(`Fund name`) )
# Remove carriage return and line feed from colnames
colnames(table2) <- gsub("\r\n", " ", colnames(table2) )

# Import Table 3
table3 <- read_xlsx("2018-AFLSS-201706.xlsx", sheet = "Table 3", range = "A5:CE206", col_names = TRUE, trim_ws = TRUE)
table3 <- table3 %>%
   select(`Fund name`, `Total administration and operating expenses`, `Total Investment expenses`, `Cash flow adjusted net assets`) %>% 
   filter(!is.na(`Fund name`) )

# Join tables 2 and 3
apra <- table2 %>% 
   left_join(table3, by = "Fund name")

rm(table2, table3)

## CODE CHUNK 2: Tidy data ----
apra <- apra %>% 
   # Select required variables
   select(`Fund name`, `Fund type`, `Total assets`, `Number of MySuper products authorised`, 
          `Total number of member accounts`, `Average member account balance` ,
          `Total administration and operating expenses`, `Total Investment expenses` , `Cash flow adjusted net assets`, 
          `Proportion of total assets in default or MySuper strategy`,
          `One-year rate of return`, `Five-year rate of return`, `Ten-year rate of return` ) %>% 
   # Convert vars that we want to be numeric
   mutate_at(.tbl = ., .vars = 3:length(.), .funs = as.numeric ) %>% 
   # Turn proportions into percentages
   mutate_at(.tbl = ., .vars = 10:length(.), .funs = funs( .*100) ) %>% 
   # Tidy fund type variable
   mutate(`Fund type` = case_when(`Fund type` == "Public Sector" ~ "Public sector" ,
                                  `Fund type` == "Retail - ERF" ~ "Retail" , 
                                  TRUE ~ `Fund type`) ) %>% 
   mutate(`Fund type` = fct_relevel(as_factor(`Fund type`), c("Industry", "Retail", "Corporate", "Public sector") ) ) %>% 
   # Calculate operating and investment expenses ratios
   mutate(`Operating expense ratio` = `Total administration and operating expenses`/ `Cash flow adjusted net assets`*100 , 
          `Investment expenses ratio` = `Total Investment expenses`/ `Cash flow adjusted net assets`*100 ) %>% 
   # Remove unnecessary variables
   select( -c("Total administration and operating expenses" , "Total Investment expenses" , "Cash flow adjusted net assets") )

# Missing values
apra <- apra %>%
   filter(!is.na(`Total assets`) & `Total assets` != 0 ) %>%
   filter(!is.na(`Total number of member accounts`) & `Total number of member accounts` != 0 ) %>% 
   # Assets are initially in $'000s
   mutate(`Total assets` = `Total assets`*1000) 
write_csv(apra, "apra.csv")

## CODE CHUNK 3: Create summary table ----
apra <- read_csv("apra.csv")
summary_table <- apra %>%
   group_by(`Fund type`) %>%
   summarise(`Mean assets ($m)` = sum(`Total assets`, na.rm=TRUE)/n()/1000000 ,
             `Number of funds` = n() ,
             `Operating expense ratio (%)` = weighted.mean(`Operating expense ratio`, `Total assets`, na.rm = TRUE) ,
             `Investment expense ratio (%)` = weighted.mean(`Investment expenses ratio`, `Total assets`, na.rm = TRUE) ,
             `One-year return (%)` = weighted.mean(`One-year rate of return`, `Total assets`, na.rm = TRUE) ,
             `Five-year return (%)` = weighted.mean(`Five-year rate of return`, `Total assets`, na.rm = TRUE) ,
             `Ten-year return (%)` = weighted.mean(`Ten-year rate of return`, `Total assets`, na.rm = TRUE) )

## CODE CHUNK 4: Look at summary table ----
summary_table

## CODE CHUNK 5: Plot operating expenses ratio by rate of return  ----
plot1 <- apra %>%
   # Note: Total assets are squared here to emphasise differences in fund size
   mutate(`Total assets` = `Total assets`^2 ) %>% 
   ggplot( aes(x = `Operating expense ratio`, y = `Ten-year rate of return`) ) +
   geom_point( aes(size = `Total assets`, colour = `Fund type`), shape = 1, stroke=1.1) +
   geom_smooth()+
   PC.theme.scat() +
   scale_colour_manual(values = c(PC_green, PC_orange, PC_purple, PC_yellow) ) +
   scale_x_continuous( limits=c(0, 1) ) +
   guides(size=FALSE) 
plot1

## CODE CHUNK 6: Plot investment expenses ratio by rate of return ----
plot2 <- apra %>% 
   mutate(`Total assets` = `Total assets`^2 ) %>% 
   ggplot( aes(x = `Investment expenses ratio`, y = `Ten-year rate of return`) ) +
   geom_point( aes(size = `Total assets`, colour = `Fund type`), shape = 1, stroke=1.1) +
   geom_smooth()+
   PC.theme.scat() +
   scale_colour_manual(values = c(PC_green, PC_orange, PC_purple, PC_yellow) ) +
   scale_x_continuous( limits=c(0, 1) ) +
   guides(size=FALSE) 
plot2

## CODE CHUNK 7: Plot investment and operating expenses ratios ----
plot3 <- apra %>% 
   mutate(`Total assets` = `Total assets`^2 ) %>% 
   ggplot( aes(y = `Investment expenses ratio`, x = `Operating expense ratio`, colour = `Fund type`) ) +
   geom_point( aes(size = `Total assets` ), shape = 1, stroke=1.5 ) +
   scale_colour_manual(values = c(PC_green, PC_orange, PC_purple, PC_yellow) ) +
   scale_y_continuous( limits=c(0,1.5) ) +
   scale_x_continuous( limits=c(0,5) ) +
   PC.theme.scat()+
   guides(size=FALSE) 
plot3

## CODE CHUNK 8: Plot returns by proportion of default/MySuper assets ----
plot4 <- apra %>% 
   mutate(`Total assets` = `Total assets`^2 ) %>% 
   ggplot( aes(y = `Ten-year rate of return`, x = `Proportion of total assets in default or MySuper strategy`) ) +
   geom_point( aes(size = `Total assets` , colour = `Fund type` ), shape = 1, stroke = 1.05) +
   scale_colour_manual(values = c(PC_green, PC_orange, PC_purple, PC_yellow) ) +
   scale_y_continuous( limits=c(-2,7) ) +
   geom_smooth() +
   PC.theme.scat()+
   guides(size=FALSE) 
plot4