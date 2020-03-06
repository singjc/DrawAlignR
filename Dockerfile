# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest


# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev
    
# install R packages required 
# (change it dependeing on the packages you need)
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('Roestlab/DrawAlignR')"

# copy the app to the image
COPY DrawAlignR.Rproj /srv/shiny-server/
COPY /inst/shiny-script/app.R /srv/shiny-server/
COPY R /srv/shiny-server/R
s

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server.sh"]