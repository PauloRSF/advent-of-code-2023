FROM ubuntu

WORKDIR /tmp

RUN apt update
RUN apt install -y curl wget gcc gnat libncurses5-dev

RUN curl -L -o basic.tar.gz https://sinalbr.dl.sourceforge.net/project/fbc/FreeBASIC-1.10.0/Binaries-Linux/FreeBASIC-1.10.0-ubuntu-22.04-x86_64.tar.gz
RUN tar -xf basic.tar.gz
RUN mv FreeBASIC-1.10.0-ubuntu-22.04-x86_64 /freebasic

CMD ["bash"]
