#!/usr/local/bin/bash
set -e

# add gnu date for date parsing
apk add coreutils

image="gcr.io/$PROJECT_ID/sweetspot-build"
docker pull $image
created=$(docker inspect -f '{{.Created}}' $image)
last_run=$(date -d$created +%s)
now=$(date +%s)
seconds_since_last_run=$(($now - $last_run))
seconds_in_day=86400

if [ $seconds_since_last_run -gt $seconds_in_day ]; then
  # We should rebuild the image
  echo "We should rebuild the image, exiting with success to continue build"
  exit 0
else
  echo "Shouldn't rebuild the image yet, exiting with error to stop build"
  gcloud builds cancel $BUILD_ID
  exit 1
fi
