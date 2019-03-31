FROM haskell:8 AS build
WORKDIR /opt/build

# We depend on postgres
RUN apt-get update --quiet && apt-get install -y --quiet libpq-dev

# Install deps first for improved caching
COPY ./api/stack.yaml .
COPY ./api/package.yaml .
RUN stack --system-ghc build --only-dependencies --verbosity warn

COPY ./api /opt/build
RUN stack --system-ghc build --verbosity warn

# Leave only the executable in the second stage
FROM debian:stretch
WORKDIR /opt/supple
RUN apt-get update --quiet && apt-get install -y --quiet \
  ca-certificates \
  libgmp-dev \
  libpq-dev
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-13.5/8.6.3/bin/supple-exe .
COPY --from=build /opt/build/migrations ./migrations
COPY ./dist /opt/dist
CMD ["/opt/supple/supple-exe"]
