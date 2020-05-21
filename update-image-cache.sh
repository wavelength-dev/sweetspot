#!/bin/sh

gcloud builds submit --config cloudbuild-cache.yaml .
