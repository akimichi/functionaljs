FROM phusion/baseimage:0.9.11
# c.f. https://github.com/phusion/baseimage-docker
MAINTAINER Akimichi Tatsukawa <akimichi.tatsukawa@gmail.com>
ENV REFRESHED_AT 2016-10-24(Mon)
## Use baseimage-docker's init system.
CMD ["/sbin/my_init"]

RUN /etc/my_init.d/00_regen_ssh_host_keys.sh

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

COPY .profile /root
RUN mkdir -p /workspace/nodejs && \
    mkdir -p /workspace/scala && \
    mkdir -p /workspace/haskell


#########################
## sbt インストール
#########################
ENV SCALA_VERSION 2.11.7
ENV SBT_VERSION 0.13.8

COPY build.sbt /workspace/scala
COPY project /workspace/scala/project
COPY src /workspace/scala/src

# INSTALL JAVA 7 add webupd8 repository
RUN \
    echo "===> add webupd8 repository..."  && \
    echo "deb http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main" | tee /etc/apt/sources.list.d/webupd8team-java.list  && \
    echo "deb-src http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main" | tee -a /etc/apt/sources.list.d/webupd8team-java.list  && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EEA14886  && \
    apt-get update  && \
    \
    \
    echo "===> install Java"  && \
    echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections  && \
    echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections  && \
    DEBIAN_FRONTEND=noninteractive  apt-get install -y --force-yes oracle-java7-installer oracle-java7-set-default  && \
    \
    \
    echo "===> clean up..."  && \
    rm -rf /var/cache/oracle-jdk7-installer  && \
    apt-get clean  && \
    rm -rf /var/lib/apt/lists/*

# scala
RUN \
  cd /root && \
  curl -o scala-$SCALA_VERSION.tgz http://downloads.typesafe.com/scala/$SCALA_VERSION/scala-$SCALA_VERSION.tgz && \
  tar -xf scala-$SCALA_VERSION.tgz && \
  rm scala-$SCALA_VERSION.tgz 
#  sbt
WORKDIR /workspace/scala
RUN \
  curl  -L -o sbt-$SBT_VERSION.deb https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb && \
  dpkg -i sbt-$SBT_VERSION.deb && \
  rm sbt-$SBT_VERSION.deb && \
  apt-get update 
RUN sbt update

###############################
# Install nvm with node and npm
###############################
ENV NODE_VERSION 0.12.0

# COPY test /workspace/nodejs/test
# COPY lib /workspace/nodejs/lib
# COPY .nvmrc gulpfile.js package.json /workspace/nodejs/
RUN touch $HOME/.ssh/known_hosts
RUN ssh-keyscan github.com >> $HOME/.ssh/known_hosts
RUN git clone https://github.com/akimichi/functionaljs.git /workspace/nodejs

# install node.js 
WORKDIR /workspace/nodejs
RUN add-apt-repository ppa:chris-lea/node.js && \
    apt-get update -qq
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y nodejs 
# Replace shell with bash so we can source files
RUN rm /bin/sh && ln -s /bin/bash /bin/sh

WORKDIR /root
# setup the nvm environment
# Install nvm with node and npm
RUN git clone https://github.com/creationix/nvm.git $HOME/.nvm
RUN bash \
    && source $HOME/.nvm/nvm.sh \
    && nvm install v$NODE_VERSION \
    && nvm alias default v$NODE_VERSION \
    && nvm use default

ENV NODE_PATH $NVM_DIR/v$NODE_VERSION/lib/node_modules
ENV PATH      $NVM_DIR/v$NODE_VERSION/bin:$PATH

RUN npm install -g node-gyp && \
    npm install -g mocha && \
    npm install -g gulp && \
    npm install -g coffee-script
WORKDIR /workspace
RUN cd /workspace/nodejs && npm install

#################
# install haskell
#################
RUN export DEBIAN_FRONTEND=noninteractive && \
  apt-get update && \
  apt-get dist-upgrade -qqy && \
  apt-get install -qqy --no-install-recommends software-properties-common && \
  add-apt-repository -y ppa:hvr/ghc && \
  apt-get update && \
  apt-get install -qqy cabal-install-1.22 ghc-7.10.2 happy-1.19.5 alex-3.1.4 && \
  apt-get autoremove -qqy && \
  apt-get clean && apt-get autoclean && \
  rm -rf /usr/share/man/?? && rm -rf /usr/share/man/??_*

ENV PATH="${HOME}/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.2/bin:${PATH}"

WORKDIR /workspace/haskell
RUN wget https://www.stackage.org/lts/cabal.config

# install stackage
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
RUN echo 'deb http://download.fpcomplete.com/ubuntu trusty main' | tee /etc/apt/sources.list.d/fpco.list
RUN apt-get update && apt-get install stack -y
COPY src /workspace/haskell/src/
COPY functionaljs.cabal Setup.hs LICENSE /workspace/haskell/
RUN stack init
RUN stack setup

# cabal
RUN cabal update
RUN cabal install 'cabal-install >= 0.10'
RUN cabal install

RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

VOLUME /workspace

CMD ["bash"]
