
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(tidyr)

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2]
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    # Read all billing details in a single dataframe
    files <- list.files('~/billings')
    files <- files[grepl('aws-billing-detailed-line-items-2017-02.*csv$', files) == T]
    df <- read.csv(paste('~/billings/', files[1], sep=''), stringsAsFactors = F)
    for (f in files[-1]) {
      full_path <- paste('~/billings/', f, sep = '')
      print(full_path)
      df <- rbind(df, read.csv(full_path))
    }
    
    # Find usages of EC2, RDS and ElastiCache 
    df <- filter(df, ProductName %in% c('Amazon Elastic Compute Cloud',
                                        'Amazon RDS Service',
                                        'Amazon ElastiCache'))
    
    # Get total cost, total hours, pricing rate
    df_sum <- df %>% group_by(UsageType) %>%
      summarise(TotalCosts = sum(Cost),
                TotalHours = sum(UsageQuantity),
                rate = mean(Rate)) 
    
    # Find only instance usage, filter out data transfers, storage usages, etc
    df_sum <- filter(df_sum, grepl('BoxUsage', UsageType) | grepl('InstanceUsage:db', UsageType) | grepl('NodeUsage:cache', UsageType))
    pie(df_sum$TotalCosts, labels = df_sum$UsageType)
    View(df_sum)
  })
  
})