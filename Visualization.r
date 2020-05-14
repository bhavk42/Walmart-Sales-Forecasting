#Importing Libraries
library(ggplot2)
library(data.table)
library(reshape)
library(reshape2)
library(dplyr)
library(forecast)
library(tidyr)

#Reading Data
df = read.csv('sales_train_validation.csv')
cal_df = read.csv('calendar.csv')
cost_df = read.csv('df_sales.csv')
sales_df = read.csv('df_sales_state.csv')

get_day <- function(day){
  return(paste('d_', day, sep = ''))
}

cost_df$d <- sapply(cost_df$day, FUN = get_day)

#Total number of items sold per day
total_sold = colSums(df[, c(7: (ncol(df) - 1))])

#plotting number of items sold daily
ggplot(data = as.data.frame(total_sold)) + 
  geom_line(aes(x = c(1: length(total_sold)), y = total_sold), 
            color = '#0182FF') + 
  labs(title = 'Total Items Sold', x = 'Days', y = 'Quantities Sold')

ggsave('Total Units Sold.png')

#Plotting daily sales
ggplot() + 
  geom_line(aes(x = c(1: nrow(cost_df)), y = cost_df$Sales), 
            color = '#25D22A') + 
  labs(title = 'Total Sales', x = 'Days', y = 'Sales (in dollars)')

ggsave('Total Sales.png')

#Number of items sold per day in the state of California
ca_total = colSums(df[df['state_id'] == 'CA', c(7: ncol(df))])
ca_total = melt(ca_total)

#Number of items sold per day in the state of Texas
tx_total = colSums(df[df['state_id'] == 'TX', c(7: ncol(df))])
tx_total = melt(tx_total)

#Number of items sold per day in the state of Wisconsin
wi_total = colSums(df[df['state_id'] == 'WI', c(7: ncol(df))])
wi_total = melt(wi_total)

total_sales = data.frame(c(ca_total, tx_total, wi_total))

names(total_sales) = c('California', 'Texas', 'Wisconsin')

day = c(1:nrow(total_sales))

total_sales <- cbind(day, total_sales)

plot_df <- melt(as.data.table(total_sales), id = c('day'))

names(plot_df) <- c('Day', 'State', 'Value')

#Plotting number of items sold per state
ggplot(plot_df) + 
  geom_line(aes(x = Day, 
                y = Value, 
                color = State)) + 
  labs(title = 'Total Items Sold', 
       x = 'Days', 
       y = 'Number of Items Sold')

ggsave('Number of Items Sold per State.png')

total_sales = merge(sales_df[sales_df$state_id == 'TX',], 
                    merge(sales_df[sales_df$state_id == 'CA',], 
                          sales_df[sales_df$state_id == 'WI',], by = 'day'), 
                    by = 'day')

names(total_sales) <- c('d', 'tx_x', 'state_tx', 'Texas', 'ca_x', 'state_ca', 
                        'California', 'wi_x', 'state_wi', 'Wisconsin')

total_sales[, c('tx_x', 'state_tx', 'ca_x', 'state_ca', 'wi_x', 'state_wi')] <- 
  NULL

#Number of items sold per day in food category
food_items_sold <- colSums(df[df['cat_id'] == 'FOODS', c(7: ncol(df))])
food_items_sold <- melt(food_items_sold)

#Number of items sold per day in hobbies category
hobbies_items_sold <- colSums(df[df['cat_id'] == 'HOBBIES', c(7: ncol(df))])
hobbies_items_sold <- melt(hobbies_items_sold)

#Number of items sold per day in household category
household_items_sold <- colSums(df[df['cat_id'] == 'HOUSEHOLD', c(7: ncol(df))])
household_items_sold <- melt(household_items_sold)

category_items_sold <- data.frame(c(food_items_sold, hobbies_items_sold, 
                                    household_items_sold))

names(category_items_sold) <- c('Food', 'Hobbies', 'Household')

day <- c(1: nrow(category_items_sold))

category_items_sold <- cbind(day, category_items_sold)

plot_df = melt(category_items_sold, id = c('day'))

names(plot_df) <- c('Day', 'Category', 'Value')

#Plotting number of items sold per category
ggplot(plot_df) + 
  geom_line(aes(x = Day, y = Value, 
                color = Category)) + 
  labs(fill = 'Category', title = 'Total Items Sold', 
       x = 'Days', 
       y = 'Number of Items Sold')

ggsave('Number of Items Sold per Category.png')

#Sum of number of items per row
df['sum'] = rowSums(df[, c(7:ncol(df))])

#Total number of items sold per state
state_wise_items_sold <- aggregate(df$sum, by = list(df$state_id), FUN = sum)

names(state_wise_items_sold) <- c('Group', 'Value')

state_wise_items_sold <- mutate(state_wise_items_sold, 
                                Proportion = Value / sum(Value))

#Pie chart showing percentage of number of items sold by states
ggplot(state_wise_items_sold, aes(x = "", y = Value, fill = Group)) + 
  geom_bar(stat = 'identity', width = 1) + 
  coord_polar('y', start = 0) + 
  geom_text(aes(label = scales::percent(round(Proportion, 3))), 
            position = position_stack(vjust = 0.5)) + 
  labs(fill = 'State', 
       x = '', 
       y = '', 
       title = 'Total Items Sold (per State)') + 
  theme_void()

ggsave('Proportion of Number of Items Sold per state.png')

#Total number of items sold per Category
category_wise_items_sold <- aggregate(df$sum, by = list(df$cat_id), FUN = sum)

names(category_wise_items_sold) <- c('Group', 'Value')

category_wise_items_sold <- mutate(category_wise_items_sold, 
                                   Proportion = Value / sum(Value))

#Pie chart showing percentage of number of items sold by category
ggplot(category_wise_items_sold, aes(x = "", y = Value, fill = Group)) + 
  geom_bar(stat = 'identity', width = 1) + 
  coord_polar('y', start = 0) + 
  geom_text(aes(label = scales::percent(round(Proportion, 3))), 
            position = position_stack(vjust = 0.5)) + 
  labs(fill = 'Category', 
       x = '', 
       y = '', 
       title = 'Total Items Sold (per Category)') + 
  theme_void()

ggsave('Proportion of Items sold per Category.png')

df$sum <- NULL

#function to Sales by weekday
get_weekday_sales <- function(data_frame, column_name){
  data_frame <- merge(cal_df, data_frame, by = 'd')
  data_frame <- aggregate(data_frame[column_name], 
                          by = list(data_frame$weekday), 
                          sum)
  names(data_frame) <- c('weekday', 'sales')
  return(data_frame)
}

total_df <- cbind(d = names(total_sold), total_sold)

rownames(total_df) <- c(1: length(total_sold))

total_df <- data.frame(total_df)

total_df$total_sold <- as.numeric(as.character(total_df$total_sold))

#Total number of items sold by weekday
total_by_weekday <- get_weekday_sales(total_df, 'total_sold')

#Total Sales by weekday
total_sales_by_weekday <- get_weekday_sales(cost_df, 'Sales')

#Total Sales by weekday in California
ca_by_weekday <- get_weekday_sales(total_sales, 'California')

#Total Sales by weekday in Wisconsin
wi_by_weekday <- get_weekday_sales(total_sales, 'Wisconsin')

#Total Sales by weekday in Texas
tx_by_weekday <- get_weekday_sales(total_sales, 'Texas')

#function to Sales by weekday
plot_weekday_sales <- function(weekly_sales, plot_title){
  plot_title <- paste(plot_title, 'Sales by Weekday')
  ggplot(data = weekly_sales, 
         aes(x = weekday, y = sales / 1000000)) + 
    geom_bar(stat="identity", fill = '#05C8C7') + 
    labs(title = plot_title, y = 'Sales by weekday (in millions)')
}

#Plotting Sales by weekday
plot_weekday_sales(total_sales_by_weekday, 'Total')

ggsave('Total Sales by weekday.png')

#Plotting Sales by weekday in California
plot_weekday_sales(ca_by_weekday, 'California')

ggsave('Total Sales for California by weekday.png')

#Plotting Sales by weekday in Wisconsin
plot_weekday_sales(wi_by_weekday, 'Wisconsin')

ggsave('Total Sales for Wisconsin by weekday.png')

#Plotting Sales by weekday in Texas
plot_weekday_sales(tx_by_weekday, 'Texas')

ggsave('Total Sales for Texas by weekday.png')

#function to get Sales by month
get_monthly_sales <- function(data_frame, column_name){
  data_frame <- merge(cal_df, data_frame, by = 'd')
  data_frame <- aggregate(data_frame[column_name], 
                          by = list(data_frame$month), 
                          sum)
  names(data_frame) <- c('month_number', 'sales')
  data_frame$month <- month.name[data_frame$month_number]
  return(data_frame)
}

#Total Sales by month
total_by_month <- get_monthly_sales(cost_df, 'Sales')

#Total Sales by month in California
ca_by_month <- get_monthly_sales(total_sales, 'California')

#Total Sales by month in Wisconsin
wi_by_month <- get_monthly_sales(total_sales, 'Wisconsin')

#Total Sales by month in Texas
tx_by_month <- get_monthly_sales(total_sales, 'Texas')

#function to plot Sales by month
plot_month_sales <- function(sales_per_month, plot_title){
  plot_title <- paste(plot_title, 'Sales by Month')
  ggplot(sales_per_month, 
         aes(x = month, y = sales / 1000000)) + 
    geom_bar(stat = 'identity', fill = '#05C8C7') + 
    labs(title = plot_title, y = 'Sales by month (in millions)') + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
}

#Plotting Sales by month
plot_month_sales(total_by_month, 'Total')

ggsave('Total Sales by month.png')

#Plotting Sales by month in California
plot_month_sales(ca_by_month, 'California')

ggsave('Total Sales for California by month.png')

#Plotting Sales by month in Wisconsin
plot_month_sales(wi_by_month, 'Wisconsin')

ggsave('Total Sales for Wisconsin by month.png')

#Plotting Sales by month in Texas
plot_month_sales(tx_by_month, 'Texas')

ggsave('Total Sales for Texas by month.png')

#function to get Sales weekly
get_sales_over_weeks <- function(data_frame, column_name){
  col_name <- paste('sales_', column_name, sep = '')
  data_frame <- merge(cal_df, data_frame, by = 'd')
  data_frame <- aggregate(data_frame[column_name], 
                          by = list(data_frame$wm_yr_wk), 
                          sum)
  names(data_frame) <- c('week', col_name)
  return(data_frame)
}

#Total Sales over the weeks
total_over_the_weeks <- get_sales_over_weeks(cost_df, 'Sales')

#Total Sales over the weeks in California
ca_over_the_weeks <- get_sales_over_weeks(total_sales, 'California')

#Total Sales over the weeks in Wisconsin
wi_over_the_weeks <- get_sales_over_weeks(total_sales, 'Wisconsin')

#Total Sales over the weeks in Texas
tx_over_the_weeks <- get_sales_over_weeks(total_sales, 'Texas')

#Combining over the weeks
sales_over_the_weeks <- merge(total_over_the_weeks, 
                              merge(tx_over_the_weeks, 
                                    merge(ca_over_the_weeks, wi_over_the_weeks, 
                                          by = 'week'), 
                                    by = 'week'), 
                              by = 'week')

names(sales_over_the_weeks) <- c('week', 
                                 'Total', 'Texas', 'California', 'Wisconsin')

sales_over_the_weeks <- melt(sales_over_the_weeks, id = 'week')

#Plotting Sales over the weeks
ggplot(sales_over_the_weeks, 
       aes(x = week, y = value)) + 
  geom_line(aes(color = variable)) + 
  labs(title = "Sales over the weeks", x = 'Week', 
       y = 'Sales')

ggsave('Total Sales over the weeks.png')

#function to get Sales monthly
get_sales_over_months <- function(data_frame, column_name){
  col_name <- paste('sales_', column_name, sep = '')
  data_frame <- merge(cal_df, data_frame, by = 'd')
  data_frame <- aggregate(data_frame[column_name], 
                          by = list(data_frame$month, data_frame$year), 
                          sum)
  names(data_frame) <- c('month', 'year', col_name)
  data_frame$date <- as.Date(paste(data_frame$year, data_frame$month, '01', 
                                   sep = '-'), 
                             format = "%Y-%m-%d")
  return(data_frame)
}

#Total Sales over the months
total_over_the_months <- get_sales_over_months(cost_df, 'Sales')

#Total Sales over the months in California
ca_over_the_months <- get_sales_over_months(total_sales, 'California')

#Total Sales over the months in Wisconsin
wi_over_the_months <- get_sales_over_months(total_sales, 'Wisconsin')

#Total Sales over the months in Texas
tx_over_the_months <- get_sales_over_months(total_sales, 'Texas')

#Combining over the months
sales_over_the_months <- merge(total_over_the_months, 
                               merge(tx_over_the_months, 
                                     merge(ca_over_the_months, 
                                           wi_over_the_months, 
                                           by = c('month', 'year', 'date')), 
                                     by = c('month', 'year', 'date')), 
                               by = c('month', 'year', 'date'))

names(sales_over_the_months) <- c('month', 'year', 'date', 
                                  'Total', 'Texas', 'California', 'Wisconsin')

sales_over_the_months <- melt(sales_over_the_months, 
                              id = c('month', 'year', 'date'))

#Plotting Sales over the months
ggplot(sales_over_the_months, 
       aes(x = date, y = value)) + 
  geom_line(aes(color = variable)) + 
  labs(title = "Sales over the months", x = 'Date', 
       y = 'Sales')

ggsave('Total Sales over the months.png')

#function to get Sales yearly
get_sales_over_years <- function(data_frame, column_name){
  col_name <- paste('sales_', column_name, sep = '')
  data_frame <- merge(cal_df, data_frame, by = 'd')
  data_frame <- aggregate(data_frame[column_name], 
                          by = list(data_frame$year), 
                          sum)
  names(data_frame) <- c('year', col_name)
  return(data_frame)
}

#Total Sales over the years
total_over_the_years <- get_sales_over_years(cost_df, 'Sales')

#Total Sales over the years in California
ca_over_the_years <- get_sales_over_years(total_sales, 'California')

#Total Sales over the years in Wisconsin
wi_over_the_years <- get_sales_over_years(total_sales, 'Wisconsin')

#Total Sales over the years in Texas
tx_over_the_years <- get_sales_over_years(total_sales, 'Texas')

#Combining over the years
sales_over_the_years <- merge(total_over_the_years, 
                               merge(tx_over_the_years, 
                                     merge(ca_over_the_years, 
                                           wi_over_the_years, 
                                           by = 'year'), 
                                     by = 'year'), 
                               by = 'year')

names(sales_over_the_years) <- c('year', 
                                  'Total', 'Texas', 'California', 'Wisconsin')

sales_over_the_years <- melt(sales_over_the_years, id = 'year')

#Plotting  over the years
ggplot(sales_over_the_years, 
       aes(x = year, y = value)) + 
  geom_line(aes(color = variable)) + 
  labs(title = "Sales over the Years", x = 'Year', 
       y = 'Sales')

ggsave('Total Sales over the years.png')