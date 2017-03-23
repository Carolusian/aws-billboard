
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


# Planner function for to get matrics if EC2 instance reserved
# Return a dataframe include the costs if the EC2 instance if reserved
ec2_ri_planner <- function (days, df_full) {
  df <- filter(report_for(days, df_full), 
               grepl('BoxUsage', UsageType) | 
                 (grepl('HeavyUsage', UsageType) & AvailabilityZone != '') | 
                 grepl('Usage:db', UsageType) | 
                 grepl('NodeUsage:cache', UsageType))
  
  # Sum up the total cost
  df_sum <- df %>% group_by(UsageType, AvailabilityZone, ReservedInstance, Rate) %>%
    summarise(TotalQuantity = sum(UsageQuantity),
              TotalCost = sum(Cost),
              RateSum = sum(Rate * UsageQuantity))
  
  # Compare with Reserved Instances for EC2
  df_ec2 <- filter(df_sum, grepl('BoxUsage', UsageType) | 
                     (grepl('HeavyUsage', UsageType) & AvailabilityZone != '')) 
  
  # Read quotation for EC2 reserved instances
  # Suppose we have 365 x 24 = 8760 billings hours each year 
  hours_1yr <- 365 * 24
  ri_ec2_quot <- read.csv('Consolidated-RI-Quotation-EC2.csv', stringsAsFactors = F)
  ri_ec2_hrs <- filter(ri_ec2_quot, tolower(Unit) == 'hrs')[c('usageType', 'PricePerUnit')]
  ri_ec2_upfront <- filter(ri_ec2_quot, tolower(Unit) == 'quantity')[c('usageType', 'PricePerUnit')]
  df_ec2_quot <- merge(df_ec2, ri_ec2_hrs, by.x = 'UsageType', by.y = 'usageType')
  df_ec2_quot <- merge(df_ec2_quot, ri_ec2_upfront, by.x = 'UsageType', by.y = 'usageType')
  df_ec2_quot <- df_ec2_quot %>% mutate(RateIfReserved = round(PricePerUnit.x + PricePerUnit.y / hours_1yr, digits = 3))
  df_ec2_quot <- df_ec2_quot %>% mutate(TotalIfReserved = RateIfReserved * TotalQuantity)
  df_ec2_quot <- df_ec2_quot %>% mutate(SaveIfReserved = TotalCost - TotalIfReserved) 
  df_ec2_quot <- df_ec2_quot %>% mutate(SaveRate = round(SaveIfReserved / TotalCost, digits = 2))
  
  # Get only instance types, strip region info
  df_ec2_quot$InstanceType <- sub("^.*:", "\\1", df_ec2_quot$UsageType)
  df_ec2_quot[c('InstanceType', 'AvailabilityZone', 'ReservedInstance', 'TotalCost', 'TotalIfReserved', 'SaveIfReserved', 'SaveRate')]
}

# Planner function for to get matrics if RDS instance reserved
# Return a dataframe include the costs if the RDS instance if reserved
rds_ri_planner <- function (days, df_full) {
  df <- filter(report_for(days, df_full), 
               grepl('BoxUsage', UsageType) | 
                 (grepl('HeavyUsage', UsageType) & AvailabilityZone != '') | 
                 grepl('Usage:db', UsageType) | 
                 grepl('NodeUsage:cache', UsageType))
  
  # Sum up the total cost
  df_sum <- df %>% group_by(UsageType, AvailabilityZone, ReservedInstance, Rate) %>%
    summarise(TotalQuantity = sum(UsageQuantity),
              TotalCost = sum(Cost),
              RateSum = sum(Rate * UsageQuantity))
  
  # Compare with Reserved Instances for RDS
  df_rds <- filter(df_sum, grepl('Usage:db', UsageType)) 
  
  # Read quotation for RDS reserved instances
  # Suppose we have 365 x 24 = 8760 billings hours each year 
  hours_1yr <- 365 * 24
  ri_rds_quot <- read.csv('Consolidated-RI-Quotation-RDS.csv', stringsAsFactors = F)
  ri_rds_hrs <- filter(ri_rds_quot, tolower(Unit) == 'hrs')[c('usageType', 'PricePerUnit')]
  ri_rds_upfront <- filter(ri_rds_quot, tolower(Unit) == 'quantity')[c('usageType', 'PricePerUnit')]
  df_rds_quot <- merge(df_rds, ri_rds_hrs, by.x = 'UsageType', by.y = 'usageType')
  df_rds_quot <- merge(df_rds_quot, ri_rds_upfront, by.x = 'UsageType', by.y = 'usageType')
  df_rds_quot <- df_rds_quot %>% mutate(RateIfReserved = round(PricePerUnit.x + PricePerUnit.y / hours_1yr, digits = 3))
  df_rds_quot <- df_rds_quot %>% mutate(TotalIfReserved = RateIfReserved * TotalQuantity)
  df_rds_quot <- df_rds_quot %>% mutate(SaveIfReserved = TotalCost - TotalIfReserved) 
  df_rds_quot <- df_rds_quot %>% mutate(SaveRate = round(SaveIfReserved / TotalCost, digits = 2))
  
  # Get only instance types, strip region info
  df_rds_quot$InstanceType <- sub("^.*:", "\\1", df_rds_quot$UsageType)
  df_rds_quot[c('InstanceType', 'AvailabilityZone', 'ReservedInstance', 'TotalCost', 'TotalIfReserved', 'SaveIfReserved', 'SaveRate')]
}

cache_ri_planner <- function(days, df_full) {
  
  df <- filter(report_for(days, df_full), 
               grepl('BoxUsage', UsageType) | 
                 (grepl('HeavyUsage', UsageType) & AvailabilityZone != '') | 
                 grepl('Usage:db', UsageType) | 
                 grepl('NodeUsage:cache', UsageType))
  
  # Sum up the total cost
  df_sum <- df %>% group_by(UsageType, AvailabilityZone, ReservedInstance, Rate) %>%
    summarise(TotalQuantity = sum(UsageQuantity),
              TotalCost = sum(Cost),
              RateSum = sum(Rate * UsageQuantity))
  
  # Compare with Reserved Instances for ElastiCache
  df_cache <- filter(df_sum, grepl('NodeUsage:cache', UsageType)) 
  
  # Read quotation for ElastiCache reserved instances
  # Suppose we have 365 x 24 = 8760 billings hours each year 
  hours_1yr <- 365 * 24
  ri_cache_quot <- read.csv('Consolidated-RI-Quotation-ElastiCache.csv', stringsAsFactors = F)
  ri_cache_hrs <- filter(ri_cache_quot, tolower(Unit) == 'hrs')[c('usageType', 'PricePerUnit')]
  ri_cache_upfront <- filter(ri_cache_quot, tolower(Unit) == 'quantity')[c('usageType', 'PricePerUnit')]
  df_cache_quot <- merge(df_cache, ri_cache_hrs, by.x = 'UsageType', by.y = 'usageType')
  df_cache_quot <- merge(df_cache_quot, ri_cache_upfront, by.x = 'UsageType', by.y = 'usageType')
  df_cache_quot <- df_cache_quot %>% mutate(RateIfReserved = round(PricePerUnit.x + PricePerUnit.y / hours_1yr, digits = 3))
  df_cache_quot <- df_cache_quot %>% mutate(TotalIfReserved = RateIfReserved * TotalQuantity)
  df_cache_quot <- df_cache_quot %>% mutate(SaveIfReserved = TotalCost - TotalIfReserved) 
  df_cache_quot <- df_cache_quot %>% mutate(SaveRate = round(SaveIfReserved / TotalCost, digits = 2))
  
  # Get only instance types, strip region info
  df_cache_quot$InstanceType <- sub("^.*:", "\\1", df_cache_quot$UsageType)
  df_cache_quot[c('InstanceType', 'AvailabilityZone', 'ReservedInstance', 'TotalCost', 'TotalIfReserved', 'SaveIfReserved', 'SaveRate')]
  
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
    df_ec2 <- filter(df_ec2, grepl('BoxUsage', UsageType) |
                       (grepl('HeavyUsage', UsageType) & AvailabilityZone != '')) 
    
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
  output$ec2_ri_metrics <- renderTable({
    days <- ifelse(exists('input'), input$days, 30)
    ec2_ri_planner(days, df_raw)
    
  })
  
  output$ec2_ri_saved <- renderPrint({
    days <- ifelse(exists('input'), input$days, 30)
    paste('TOTAL EC2 COST CAN SAVE:', sum(ec2_ri_planner(days, df_raw)$SaveIfReserved))
  })
  
  output$rds_ri_metrics <- renderTable({
    days <- ifelse(exists('input'), input$days, 30)
    rds_ri_planner(days, df_raw)
  })
  
  output$rds_ri_saved <- renderPrint({
    days <- ifelse(exists('input'), input$days, 30)
    paste('TOTAL RDS COST CAN SAVE:', sum(rds_ri_planner(days, df_raw)$SaveIfReserved))
  })
  
  output$cache_ri_metrics <- renderTable({
    days <- ifelse(exists('input'), input$days, 30)
    cache_ri_planner(days, df_raw)
  })
  
  output$cache_ri_saved <- renderPrint({
    days <- ifelse(exists('input'), input$days, 30)
    paste('TOTAL ELASTICACHE COST CAN SAVE:', sum(cache_ri_planner(days, df_raw)$SaveIfReserved))
  })
})