FROM rocker/shiny:latest

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Download and install library
RUN R -e "install.packages(c('plotly', 'sqldf','dplyr','officer','reshape','data.table','shinydashboard','rvg','flextable','RColorBrewer'))"

RUN mkdir /root/app
COPY pca /root/shiny_save


# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/root/shiny_save', host = '0.0.0.0', port = 3838)"]
