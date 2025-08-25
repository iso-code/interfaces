FROM rocker/shiny:4.5.1

ARG http_proxy
ARG https_proxy

ENV http_proxy="${http_proxy}"
ENV https_proxy="${https_proxy}"
# Systemabhängigkeiten für Kompilierung (falls nötig)

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2 
    
RUN apt-get clean 
RUN rm -rf /var/lib/apt/lists/*

RUN Rscript -e "install.packages(c('pacman','devtools', 'plumber','curl', 'tidyverse', 'tidyjson', 'jsonlite','vroom'))" 
RUN Rscript -e "library(devtools); install_github('iso-code/interfaces')"

RUN mkdir /app

WORKDIR /app
COPY api.R /app/api.R
COPY page_tree_W.rds /app/page_tree_W.rds

EXPOSE 8010

CMD ["R", "-e", "pr <- plumber::plumb('/app/api.R'); pr$run(host='0.0.0.0', port=8010)"]



