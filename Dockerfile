FROM --platform=linux/amd64 ubuntu

WORKDIR /tmp

RUN apt update
RUN apt install -y curl wget gcc gnat libncurses5-dev

# FreeBasic

RUN curl -L -o basic.tar.gz https://sinalbr.dl.sourceforge.net/project/fbc/FreeBASIC-1.10.0/Binaries-Linux/FreeBASIC-1.10.0-ubuntu-22.04-x86_64.tar.gz
RUN tar -xf basic.tar.gz
RUN mv FreeBASIC-1.10.0-ubuntu-22.04-x86_64 /freebasic

# Clojure

RUN curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
RUN chmod +x linux-install.sh
RUN ./linux-install.sh
RUN curl -L -o jdk.tar.gz https://download.java.net/java/GA/jdk21.0.1/415e3f918a1f4062a0074a2794853d0d/12/GPL/openjdk-21.0.1_linux-x64_bin.tar.gz
RUN tar -xvf jdk.tar.gz
ENV JAVA_HOME=/tmp/jdk-21.0.1
RUN clojure -P

CMD ["bash"]
