FROM fpco/stack-build:lts-14.4 AS build

WORKDIR /opt/build

# We depend on postgres
RUN apt-get update --quiet && apt-get install -y --quiet libpq-dev

# Install deps first for improved caching
COPY ./api/stack.yaml .
COPY ./api/package.yaml .
COPY ./api/sweetspot.cabal .
RUN stack setup
RUN stack build --only-dependencies --verbosity warn

COPY ./api /opt/build
RUN stack build --verbosity warn --copy-bins

# Build the PureScript injectables
FROM node:12 AS build-dist
WORKDIR /opt/build-dist

# Install build dependencies
RUN yarn global add purescript spago uglify-js
COPY ./injectable/spago.dhall ./injectable/packages.dhall ./
RUN spago install --global-cache skip

# Compile, bundle and uglify our scripts
COPY ./injectable/src ./src
RUN spago bundle-app --main SweetSpot.Main --to ./sweetspot-main.js
RUN uglifyjs --compress --mangle --output ./sweetspot-main.min.js ./sweetspot-main.js
RUN spago bundle-app --main SweetSpot.Checkout --to ./sweetspot-checkout.js
RUN uglifyjs --compress --mangle --output ./sweetspot-checkout.min.js ./sweetspot-checkout.js

# Leave only the executable in the second stage
FROM ubuntu
WORKDIR /opt/sweetspot
RUN apt-get update --quiet && apt-get install -y --quiet \
  ca-certificates \
  libgmp-dev \
  libpq-dev
COPY --from=build /root/.local/bin/sweetspot-exe .
COPY --from=build /opt/build/migrations ./migrations
COPY --from=build-dist /opt/build-dist/sweetspot*.js /opt/dist/

EXPOSE 8082/tcp
CMD ["/opt/sweetspot/sweetspot-exe"]
