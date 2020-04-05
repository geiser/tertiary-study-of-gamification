FROM rocker/shiny-verse:latest AS prod
MAINTAINER Geiser Chalco <geiser@usp.br>

LABEL org.label-schema.license="GPL-3.0" \
      org.label-schema.vcs-url="https://github.com/geiser/docker" \
      org.label-schema.vendor="Tertiary study of gamification through R-shiny" \
      maintainer="Geiser Chalco <geiser@usp.br>"

# Install java and rJava
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libbz2-dev liblzma-dev openjdk-8-jdk \
    && R CMD javareconf -e \
	&& R -e "install.packages('rJava')" \
    && apt-get clean && apt-get autoclean

# Install packages to back-end
RUN install2.r -s qdap  
RUN install2.r -s SemNetCleaner
RUN install2.r -s digest
RUN install2.r -s plotly
RUN install2.r -s pivottabler
RUN install2.r -s XML

RUN install2.r -s tidyverse \
	&& install2.r -s stringr \
	&& install2.r -s stringi

RUN install2.r -s tools
RUN install2.r -s jsonlite
RUN install2.r -s tidyr
RUN install2.r -s dplyr
RUN install2.r -s abbrevr
RUN install2.r -s magrittr

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libv8-dev librdf0-dev \
    && install2.r -s rdflib \
    && apt-get clean && apt-get autoclean

RUN install2.r Hmisc

# Install packages to front-end
RUN install2.r -s DT \
    && install2.r -s shinyTree \
    && install2.r -s rpivotTable

RUN install2.r -s RColorBrewer 

#RUN install2.r -s bibliometrix

# Making executable
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
