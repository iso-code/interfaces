#!/bin/bash

IMAGE="api_opendata_nrw"
CONTAINER="api_opendata_nrw"

sudo docker container stop $CONTAINER
sudo docker container rm $CONTAINER
sudo docker rmi $IMAGE

docker build --tag $IMAGE --build-arg http_proxy=$http_proxy --build-arg https_proxy=$https_proxy .
docker run -d -p 8010:8010 -e "http_proxy=$http_proxy"  -e "https_proxy=$http_proxy"  --name $CONTAINER --restart=unless-stopped \
    -v /home/lanuv.nrw.de/gaj/data_latest/:/home/var/ \
     $IMAGE 
