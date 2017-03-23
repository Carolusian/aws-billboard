
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel('AWS Billing Dashboard'),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput('days',
                  'Number of days:',
                  min = 1,
                  max = 90,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel('Daily Spending', plotOutput('daily_spend')),
        tabPanel('Top EC2', plotOutput('top_ec2')),
        # tabPanel('EC2 Hours', plotOutput('')),
        # Also need to calculate instance per hour to find minimum number of instances 
        # Add place holders for ploting instances per hour
        tabPanel('RI Planner', 
                 tableOutput('ec2_ri_metrics'), 
                 verbatimTextOutput('ec2_ri_saved'),
                 tableOutput('rds_ri_metrics'), 
                 verbatimTextOutput('rds_ri_saved'),
                 tableOutput('cache_ri_metrics'), 
                 verbatimTextOutput('cache_ri_saved'))
        # tabPanel('Cash Flow Comparison', plotOutput('cash_ri_1yr')
      )
    )
  )
))
