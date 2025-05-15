FROM ubuntu:16.04

MAINTAINER Sam Murphy <samsammurphy@gmail.com>

RUN apt-get update                              && \
                                                   \
    apt-get install -y --no-install-recommends     \
    bzip2                                          \
    build-essential                                \
    git                                            \
    libssl-dev                                     \
    libffi-dev                                     \
    python3                                        \
    python3-dev                                    \
    python3-pip                                    \
    wget                                           \
                                                && \
    apt-get clean                               && \
    rm -rf /var/lib/apt/lists/*

RUN wget --quiet https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh && \
    /bin/bash /Miniconda3-latest-Linux-x86_64.sh -b -p /miniconda                      && \
    rm Miniconda3-latest-Linux-x86_64.sh                                               

ENV PATH=/miniconda/bin:${PATH}          

RUN conda update -y conda                       && \
    conda config --add channels conda-forge     && \
    conda install -y py6s

RUN conda install -c anaconda pip               && \
    pip install earthengine-api oauth2client
    
RUN conda install -y jupyter

#RUN git clone https://github.com/gee-community/ee-jupyter-contrib.git
#RUN cd ee-jupyter-contrib/docker/atmcorr-ee/
#RUN docker run -i -t -p 8080:8080 atmcorr-ee
RUN conda install -c conda-forge earthengine-api=0.1.329    &&\
    apt-get update                                          &&\
    apt-get install -y apt-transport-https ca-certificates gnupg curl sudo     &&\
    curl https://dl.google.com/dl/cloudsdk/release/google-cloud-sdk.tar.gz > /tmp/google-cloud-sdk.tar.gz   &&\
    mkdir -p /usr/local/gcloud \
  && tar -C /usr/local/gcloud -xvf /tmp/google-cloud-sdk.tar.gz \
  && /usr/local/gcloud/google-cloud-sdk/install.sh

ENV PATH $PATH:/usr/local/gcloud/google-cloud-sdk/bin
#RUN gcloud auth login

#Oka ate aqui

#gcloud config get-value project
#gcloud builds submit --region=us-west2 --tag us-west2-docker.pkg.dev/ee-curuai/quickstart-docker-repo/quickstart-image:tag1

#RUN gcloud auth login //TO AQUI
#RUN clone https://github.com/ndminhhus/geeguide.git
#RUN cd geeguide/jupyer_notebooks/

#jupyter notebook --ip='*' --NotebookApp.token='' --NotebookApp.password=''

#IMPORTANTE
#copiar do >
#cd gee-atmcorr-S2/geeguide/jupyter_notebooks/02.Atm-corr-Landsat.ipynb
#to (dentro do docker) 
#gee-atmcorr-S2/jupyer_notebooks
#usando: cp /gee-atmcorr-S2/geeguide/jupyter_notebooks/02.Atm-corr-Sentinel2.ipynb /gee-atmcorr-S2/jupyer_notebooks

#jupyter-notebook 02.Atm-corr-Landsat7.ipynb --ip='*' --port=8080 --allow-root
#ou 8888 esse ultimo
EXPOSE 8888



####################################################################
#ORIGINAL
#FROM ubuntu:16.04

#MAINTAINER Sam Murphy <samsammurphy@gmail.com>

#RUN apt-get update                              && \
#                                                   \
#    apt-get install -y --no-install-recommends     \
#    bzip2                                          \
#    build-essential                                \
#    git                                            \
#    libssl-dev                                     \
#    libffi-dev                                     \
#    python3                                        \
#    python3-dev                                    \
#    python3-pip                                    \
#    wget                                           \
#                                                && \
#    apt-get clean                               && \
#    rm -rf /var/lib/apt/lists/*

#RUN wget --quiet https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh && \
#    /bin/bash /Miniconda3-latest-Linux-x86_64.sh -b -p /miniconda                      && \
#    rm Miniconda3-latest-Linux-x86_64.sh                                               

#ENV PATH=/miniconda/bin:${PATH}          

#RUN conda update -y conda                       && \
#    conda config --add channels conda-forge     && \
#    conda install -y py6s

#RUN conda install -c anaconda pip               && \
#    pip install earthengine-api oauth2client
    
#RUN conda install -y jupyter

#EXPOSE 8888




