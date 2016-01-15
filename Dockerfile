FROM phusion/baseimage:0.9.11
# c.f. https://github.com/phusion/baseimage-docker
MAINTAINER Akimichi Tatsukawa <akimichi.tatsukawa@gmail.com>
ENV REFRESHED_AT 2016-1-15(Fri)
## Use baseimage-docker's init system.
CMD ["/sbin/my_init"]

ENV HOME /root
RUN /etc/my_init.d/00_regen_ssh_host_keys.sh

## apt-get update
RUN sed -i~ -e 's;http://archive.ubuntu.com/ubuntu;http://ftp.jaist.ac.jp/pub/Linux/ubuntu;' /etc/apt/sources.list
RUN apt-get -yqq update

## Japanese Environment
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y language-pack-ja
ENV LANG ja_JP.UTF-8
RUN update-locale LANG=ja_JP.UTF-8
RUN (mv /etc/localtime /etc/localtime.org && ln -s /usr/share/zoneinfo/Asia/Tokyo /etc/localtime)

## Development Environment
ENV EDITOR vim
RUN update-alternatives --set editor /usr/bin/vim.basic
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y git wget curl unzip build-essential python-dev rake

## node.js Environment
RUN add-apt-repository ppa:chris-lea/node.js
RUN apt-get update -qq
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y nodejs npm
RUN npm install -g node-gyp &&\
    npm install -g mocha &&\
    npm install -g gulp &&\
    npm install -g coffee-script

## sbt

## haskell


RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

CMD /bin/sh

