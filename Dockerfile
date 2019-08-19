FROM fpco/stack-build:lts-13.24 AS build

WORKDIR /opt/build

# We depend on postgres
RUN apt-get update --quiet && apt-get install -y --quiet libpq-dev

# Install deps first for improved caching
RUN stack --system-ghc build --only-dependencies --verbosity warn
COPY ./api/stack.yaml .
COPY ./api/package.yaml .
RUN stack --system-ghc build --only-dependencies --verbosity warn

COPY ./api /opt/build
RUN stack --system-ghc build --verbosity warn

# Build the PureScript injectables
FROM node:12 AS build-dist
WORKDIR /opt/build-dist

# Install build dependencies
RUN yarn global add purescript spago
COPY ./injps/package.json ./injps/yarn.lock ./
RUN yarn install
COPY ./injps/spago.dhall ./injps/packages.dhall ./
RUN spago install

# Compile, bundle and uglify our scripts
COPY ./injps/src ./src
COPY ./injps/test ./test
RUN spago bundle-app --main SweetSpot.Main --to ./output/sweetspot-main.js
RUN yarn browserify --transform envify --outfile ./sweetspot-main.js ./output/sweetspot-main.js
RUN yarn uglifyjs --compress --mangle --output ./sweetspot-main.min.js ./sweetspot-main.js
RUN spago bundle-app --main SweetSpot.Checkout --to ./output/sweetspot-checkout.js
RUN yarn browserify --transform envify --outfile ./sweetspot-checkout.js ./output/sweetspot-checkout.js
RUN yarn uglifyjs --compress --mangle --output ./sweetspot-checkout.min.js ./sweetspot-checkout.js

# Leave only the executable in the second stage
FROM debian:stretch
WORKDIR /opt/sweetspot
RUN apt-get update --quiet && apt-get install -y --quiet \
  ca-certificates \
  libgmp-dev \
  libpq-dev
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-13.24/8.6.5/bin/sweetspot-exe .
COPY --from=build /opt/build/migrations ./migrations
COPY --from=build-dist /opt/build-dist/sweetspot*.js /opt/dist/

EXPOSE 8082/tcp
CMD ["/opt/sweetspot/sweetspot-exe"]
