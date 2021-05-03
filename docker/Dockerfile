FROM rocker/tidyverse
LABEL maintainer="Amrit Singh <asingh.analytics@gmail.com>"

RUN apt-get update -qq && apt-get install -y \ 
    git-core \ 
    libssl-dev \ 
    libcurl4-gnutls-dev \
    python3.7 \
    python3-pip

RUN pip3 install awscli

WORKDIR /tmp/workdir

COPY /run_analysis.sh /tmp/workdir/run_analysis.sh
COPY /code/ /tmp/workdir/code
COPY /data/ /tmp/workdir/data

## set ENV variables
ENV S3_BUCKET ${S3_BUCKET}
ENV File ${File}

RUN echo ${S3_BUCKET}
RUN echo ${File}

## install R-packages
RUN Rscript /tmp/workdir/code/install_r_packages.R

## run analyses
RUN chmod +x ./run_analysis.sh
ENTRYPOINT ["./run_analysis.sh"]
CMD ["$S3_BUCKET", "$File"]
