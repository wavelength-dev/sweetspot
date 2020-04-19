#!/bin/sh
set -e
docker build --tag sweetspot-build --file Dockerfile.base .
docker tag sweetspot-build gcr.io/sweetspot-255522/sweetspot-build
docker push gcr.io/sweetspot-255522/sweetspot-build
