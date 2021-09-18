FROM rocker/binder:3.6.0
LABEL maintainer='Qingqing Chen'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/spatialnetworkslab/entangledfootprints/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2021-09-18/')); devtools::install_deps(); devtools::install()"

RUN rm DESCRIPTION.1; exit 0
