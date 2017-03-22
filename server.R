
# The main business logic to generate AWS billing dashboard data
# 
# Author: Charlie Chen
# https://github.com/carolusian
# Copyright (c) 2017, Charlie Chen. All rights reserved
# Copyrights licensed under the New BSD License.
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read all billing details in a single dataframe
files <- list.files('~/billings')
files <- files[grepl('aws-billing-detailed-line-items-20.*csv$', files) == T]
df_raw <- read.csv(paste('~/billings/', files[1], sep=''), stringsAsFactors = F)
for (f in files[-1]) {
  full_path <- paste('~/billings/', f, sep = '')
  print(full_path)
  df_raw <- rbind(df_raw, read.csv(full_path))
}

# Find usages of EC2, RDS and ElastiCache, CloudFront 
df <- filter(df_raw, ProductName %in% c('Amazon Elastic Compute Cloud',
                                        'Amazon RDS Service',
                                        'Amazon ElastiCache',
                                        'Amazon CloudFront'))

shinyServer(function(input, output) {
  
  ##################################################
  # Display daily spendings of EC2, ElastiCache, RDS
  ##################################################
  output$daily_spend <- renderPlot({
    
    # Time of span of the billings to be included
    days <- ifelse(exists('input'), input$days, 30)
    # The billing date have a delay, so we minus 3
    today <- Sys.Date() - as.difftime(3, units = 'days')
    start_date <- today - as.difftime(days, units = 'days')
    
    
    
    # Remove rows UsageType of which is empty
    df <- filter(df, UsageType != '')
    
    # Annotate with UsageDate in '2017-01-01' format
    df <- df %>% mutate(UsageDate = as.Date(UsageStartDate, format='%Y-%m-%d'))
    
    # Remove data not in selected time range
    df <- filter(df, UsageDate > start_date & UsageDate <= today)
    
    
    # Get total cost, total hours, pricing rate
    #df_sum <- df %>% group_by(UsageType) %>%
    #  summarise(TotalCosts = sum(Cost),
    #            TotalHours = sum(UsageQuantity),
    #            rate = mean(Rate)) 
    
    # Find only instance usage, filter out data transfers, storage usages, etc
    # df_sum <- filter(df_sum, grepl('BoxUsage', UsageType) | grepl('InstanceUsage:db', UsageType) | grepl('NodeUsage:cache', UsageType))
    # pie(df_sum$TotalCosts, labels = df_sum$UsageType)
    
    df_daily <- df %>% group_by(UsageDate, UsageType, ProductName) %>%
      summarise(TotalCosts = sum(Cost),
                TotalHours = sum(UsageQuantity))
     
    ggplot(df_daily, aes(x=UsageDate, y=TotalCosts, fill = ProductName)) + 
      geom_bar(stat='identity') + xlab('Date') + ylab('Costs in US$') +
      ggtitle(paste('DAILY SPENDING LAST ', days, 'DAYS'))
  })
})