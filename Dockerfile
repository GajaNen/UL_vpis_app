FROM rocker/shiny:4.3.3
RUN install2.r rsconnect data.table plotly shiny reshape2 shinyWidgets DT bslib bsplus remotes
RUN R -e 'remotes::install_github("dreamRs/shinytreeview")'
WORKDIR /home/UL_vpis_2324
COPY app.R app.R
COPY www /usr/local/www
COPY js4checkbox.js js4checkbox.js
COPY data_final.csv data_final.csv
COPY deploy.R deploy.R
CMD Rscript deploy.R
