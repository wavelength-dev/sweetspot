FROM ubuntu:16.04
RUN mkdir -p /opt/supple
WORKDIR /opt/supple
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev \
  postgresql
COPY --from=supple/build:latest /opt/build/.stack-work/install/x86_64-linux/lts-13.5/8.6.3/bin .
CMD ["/opt/supple/supple-exe"]