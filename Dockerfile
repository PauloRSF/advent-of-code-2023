FROM debian

RUN apt update
RUN apt install -y gcc gnat

CMD ["bash"]
