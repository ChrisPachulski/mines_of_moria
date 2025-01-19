FROM rocker/verse:latest

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('tidyverse','rvest','jsonlite','devtools','googlesheets4','googledrive','httr','bigrquery','RSelenium','janitor','googleAuthR'), dependencies=TRUE)"

RUN mkdir -p /home/cujo253/mines_of_moria
RUN mkdir -p /home/cujo253/mines_of_moria/Essential_Referential_CSVS

WORKDIR /home/cujo253/mines_of_moria

COPY ./rewrite.R /home/cujo253/mines_of_moria/
COPY ./gaeas-cradle.json /home/cujo253/mines_of_moria/Essential_Referential_CSVS/
COPY ./pachun95_a.json /home/cujo253/mines_of_moria/Essential_Referential_CSVS/
COPY ./pachun95_service_a.json /home/cujo253/mines_of_moria/Essential_Referential_CSVS/

ENTRYPOINT ["Rscript", "rewrite.R"]