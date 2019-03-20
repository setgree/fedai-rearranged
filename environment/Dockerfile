FROM registry.codeocean.com/codeocean/stata:15.0-r3.4.2-ubuntu16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN Rscript -e 'devtools::install_version("ri", version = "0.9")'
