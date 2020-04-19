FROM gcr.io/sweetspot-255522/sweetspot-build AS build-api

# Copy code and build our binary
COPY ./api /opt/build
RUN stack build --verbosity warn --copy-bins

# Build Fulcrum
FROM node:13 AS build-fulcrum

# PureScript installer depends on libtinfo.so.5
RUN apt update && apt install --yes libncurses5

# Install build dependencies
RUN yarn global add purescript spago uglify-js
COPY ./fulcrum/spago.dhall ./fulcrum/packages.dhall ./
RUN spago install

# Compile, test, bundle and uglify our scripts
COPY ./fulcrum/src ./src
COPY ./fulcrum/test ./test
RUN spago test
RUN spago bundle-app --to ./dist/fulcrum.js
RUN uglifyjs --compress --mangle --output ./dist/fulcrum.min.js ./dist/fulcrum.js
# RUN spago bundle-app --main Fulcrum.Checkout --to ./fulcrum-checkout.js
# RUN uglifyjs --compress --mangle --output ./fulcrum-checkout.min.js ./fulcrum-checkout.js

# Build Dashboard
FROM node:13 AS build-dashboard

# PureScript installer depends on libtinfo.so.5
RUN apt update && apt install --yes libncurses5

# Install build dependencies
RUN yarn global add purescript spago parcel-bundler
COPY ./dashboard/spago.dhall ./
COPY ./dashboard/packages.dhall ./
COPY ./dashboard/package.json ./
COPY ./dashboard/yarn.lock ./
# TODO: TEST IF BUILD IS FASTER WITHOUT COPYING CACHES
COPY --from=build-fulcrum /root/.cache/spago /root/.cache/
COPY --from=build-fulcrum /usr/local/share/.config/yarn/global /usr/local/share/.config/yarn/global
RUN spago install
RUN yarn install

# Compile, test, bundle and uglify our scripts
COPY ./dashboard/src ./src
COPY ./dashboard/test ./test
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
