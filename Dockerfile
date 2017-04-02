FROM rocker/shiny:latest

RUN R -e "install.packages(c('dplyr', 'tidyr', 'stringr', 'ggplot2'), repos='https://cran.rstudio.com/')" 
