
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
library(stringr)
library(ggplot2)

# Read all billing details in a single dataframe
files <- list.files('~/billings')
files <- files[grepl('aws-billing-detailed-line-items-20.*csv$', files) == T]
df_raw <- read.csv(paste('~/billings/', files[1], sep=''), stringsAsFactors = F)

# The longest span is 90 days, so we just need reports for the last 4 months
for (f in tail(files, 4)) {
  full_path <- paste('~/billings/', f, sep = '')
  print(full_path)
  df_raw <- rbind(df_raw, read.csv(full_path))
}

# Find usages of EC2, RDS and ElastiCache, CloudFront 
df_raw <- filter(df_raw, ProductName %in% c('Amazon Elastic Compute Cloud',
                                        'Amazon RDS Service',
                                        'Amazon ElastiCache',
                                        'Amazon CloudFront'))


# Given the dataframe with all data
# it return the latest data for last n days
report_for <- function(days, df_full) {
  # The billing date have a delay, so we minus 3
  today <- Sys.Date() - as.difftime(0, units = 'days')
  start_date <- today - as.difftime(days, units = 'days')
  
  # Remove rows UsageType of which is empty
  df <- filter(df_full, UsageType != '')
  
  # Annotate with UsageDate in '2017-01-01' format
  df <- df %>% mutate(UsageDate = as.Date(UsageStartDate, format='%Y-%m-%d'))
  
  # Remove data not in selected time range
  filter(df, UsageDate > start_date & UsageDate <= today)
}

shinyServer(function(input, output) {
  
  #################################################################
  # Display daily spendings of EC2, ElastiCache, RDS and CloudFront
  #################################################################
  output$daily_spend <- renderPlot({
    # Time of span of the billings to be included
    days <- ifelse(exists('input'), input$days, 30)
    df <- report_for(days, df_raw)
    
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
  
  
  ###############################################
  # Display a pie chart of top EC2 instance types
  ###############################################
  output$top_ec2 <- renderPlot({
    # Time of span of the billings to be included
    days <- ifelse(exists('input'), input$days, 30)
    df <- report_for(days, df_raw)
    
    # Get only EC2 type spendings
    df_ec2 <- filter(df, ProductName %in% c('Amazon Elastic Compute Cloud'))  
    # We are only interested in instance count and running hours
    df_ec2 <- filter(df_ec2, grepl('BoxUsage', UsageType))
    
    # Get only instance types, strip region info
    df_ec2$InstanceType <- sub("^.*:", "\\1", df_ec2$UsageType)
    
    # Get the total span of hours
    hours_span <- difftime(max(df_ec2$UsageEndDate), min(df_ec2$UsageStartDate), units = 'hours')
    
    df_ec2_sum <- df_ec2 %>% group_by(InstanceType) %>%
      summarise(TotalQuantity = sum(UsageQuantity))
    
    # Averge count of instances for each EC2 type
    df_ec2_sum$AverageQuantity <- round(df_ec2_sum$TotalQuantity / as.integer(hours_span), digits = 1)
    
    df_ec2_top <- df_ec2_sum %>% arrange(desc(AverageQuantity)) %>% head
    pie(df_ec2_top$TotalQuantity, 
        labels = paste(df_ec2_top$InstanceType, df_ec2_top$AverageQuantity),
        main = 'AVERAGE COUNT OF TOP EC2 INSTANCE TYPES')
  })
  
  
  ###############################################
  # RI Planner
  ###############################################
  output$ri_planner <- renderPlot({
    days <- ifelse(exists('input'), input$days, 10)
    df <- filter(report_for(days, df_raw), 
                 grepl('BoxUsage', UsageType) | 
                 (grepl('HeavyUsage', UsageType) & AvailabilityZone != '') | 
                   grepl('Usage:db', UsageType) | 
                   grepl('NodeUsage:cache', UsageType))
    df_sum <- df %>% group_by(UsageType, AvailabilityZone, ReservedInstance) %>%
      summarise(TotalQuantity = sum(UsageQuantity))
    View(df_sum)
  })
})