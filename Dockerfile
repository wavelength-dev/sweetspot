FROM haskell AS build-api
WORKDIR /opt/build

# We depend on libpq to build
RUN apt-get --quiet update \
  && apt-get --quiet install --yes \
  libpq-dev

# Install deps first for improved caching
COPY api/stack.yaml .
COPY api/stack.yaml.lock .
COPY api/sweetspot.cabal .
RUN stack setup
RUN stack build --only-dependencies --verbosity warn

# Copy code and build our binary
COPY api /opt/build
RUN stack build --verbosity warn --copy-bins

# Build Fulcrum
FROM node:13 AS build-fulcrum
WORKDIR /opt/build

# PureScript installer depends on libtinfo.so.5
RUN apt update && apt install --yes libncurses5

# Install build dependencies
RUN yarn global add purescript spago terser parcel@next
COPY fulcrum/spago.dhall fulcrum/packages.dhall fulcrum/package.json fulcrum/yarn.lock .
RUN yarn install --frozen-lockfile
RUN spago install --global-cache=skip

# Compile, test, bundle and uglify our scripts
COPY fulcrum/src src
COPY fulcrum/test test
RUN spago test
RUN spago bundle-app --main Fulcrum.Main --to fulcrum.js
RUN parcel build fulcrum.js
RUN mv dist/fulcrum.js dist/fulcrum.min.js
RUN mv fulcrum.js dist/fulcrum.js

# Build Dashboard
FROM node:13 AS build-dashboard
WORKDIR /opt/build

# PureScript installer depends on libtinfo.so.5
RUN apt update && apt install --yes libncurses5

# Install build dependencies
RUN yarn global add purescript spago parcel-bundler
COPY dashboard/spago.dhall .
COPY dashboard/packages.dhall .
COPY dashboard/package.json .
COPY dashboard/yarn.lock .
RUN spago install --global-cache=skip
RUN yarn install

# Compile, test, bundle and uglify our scripts

# Use the remote .env file because builds are for use with shopify
COPY dashboard/.env.remote .env
COPY dashboard/src src
COPY dashboard/test test
RUN spago test
RUN spago build
RUN parcel build --public-url /dashboard src/index.html

# Leave only the build artifacts in the final image
FROM debian:buster-slim
WORKDIR /opt/sweetspot
RUN apt-get --quiet update \
  && apt-get --quiet install --yes \
  libpq-dev \
  # Shopify ca auth is unknown without installing this package
  ca-certificates
COPY --from=build-api /root/.local/bin/sweetspot-exe .
COPY --from=build-api /opt/build/migrations ./migrations
COPY --from=build-fulcrum /opt/build/dist/* /opt/sweetspot/dist/fulcrum/
COPY --from=build-dashboard /opt/build/dist/* /opt/sweetspot/dist/dashboard/

EXPOSE 8082/tcp
CMD ["/opt/sweetspot/sweetspot-exe"]
