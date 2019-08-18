FROM rocker/tidyverse:3.6.0

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libcurl-ocaml \
    libxml2-dev \
    libpq-dev

RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN R -e 'remotes::install_cran("shiny")'
RUN R -e 'remotes::install_cran("shinyjs")'
RUN R -e 'remotes::install_cran("tidyverse")'
RUN R -e 'remotes::install_cran("lubridate")'
RUN R -e 'remotes::install_cran("RPostgreSQL")'
RUN R -e 'remotes::install_github("Thinkr-open/golem")'
RUN R -e 'remotes::install_cran("processx")'
RUN R -e 'remotes::install_cran("attempt")'
RUN R -e 'remotes::install_cran("DT")'
RUN R -e 'remotes::install_cran("glue")'
RUN R -e 'remotes::install_cran("htmltools")'
COPY Tarot_*.tar.gz /app.tar.gz
COPY .Rprofile /.Rprofile
RUN R -e 'remotes::install_local("/app.tar.gz")'
EXPOSE 3838
CMD  R -e "options('shiny.port'=3838,shiny.host='0.0.0.0'); Tarot::run_app()"
