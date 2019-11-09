FROM haskell:8.6.5 AS build

WORKDIR /opt/build

# We depend on postgres
RUN apt-get update --quiet && apt-get install --yes --quiet libpq-dev

# Install deps first for improved caching
COPY ./api/stack.yaml .
COPY ./api/sweetspot.cabal .
RUN stack build --only-dependencies --verbosity warn

# Copy code and build our binary
COPY ./api /opt/build
RUN stack build --verbosity warn --copy-bins

# Build the fulcrum
FROM node:12-buster AS build-dist
WORKDIR /opt/build-dist

# See: https://github.com/spacchetti/spago/issues/104
RUN apt-get --quiet update && apt-get --quiet install --yes libncurses5 git

# Install build dependencies
RUN yarn global add purescript spago uglify-js
COPY ./fulcrum/spago.dhall ./fulcrum/packages.dhall ./
RUN spago install --global-cache skip

# Compile, test, bundle and uglify our scripts
COPY ./fulcrum/src ./src
COPY ./fulcrum/test ./test
RUN spago test
RUN spago bundle-app --to ./sweetspot-main.js
RUN uglifyjs --compress --mangle --output ./sweetspot-main.min.js ./sweetspot-main.js
# RUN spago bundle-app --main Fulcrum.Checkout --to ./sweetspot-checkout.js
# RUN uglifyjs --compress --mangle --output ./sweetspot-checkout.min.js ./sweetspot-checkout.js

# Leave only the executable in the second stage
FROM debian:buster-slim
WORKDIR /opt/sweetspot
RUN apt-get --quiet update \
  && apt-get --quiet install --yes \
  libpq-dev \
  # Shopify ca auth is unknown without installing this package
  ca-certificates
COPY --from=build /root/.local/bin/sweetspot-exe .
COPY --from=build /opt/build/migrations ./migrations
COPY --from=build-dist /opt/build-dist/sweetspot*.js /opt/dist/

EXPOSE 8082/tcp
CMD ["/opt/sweetspot/sweetspot-exe"]
