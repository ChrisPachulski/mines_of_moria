#!/bin/bash

set -e

# Authenticate with BigQuery
gcloud auth activate-service-account --key-file="$GOOGLE_APPLICATION_CREDENTIALS"

# Define variables
PROJECT_ID="gaeas-cradle"
DATASET_LISTINGS="docker_listings_lorcana-tcg"
DATASET_PRODUCTS="docker_product_lorcana-tcg"
DATASET_SELLERS="docker_sellers_lorcana-tcg"
EVENT_DATE=$(date +%Y_%m_%d)

# Upload function
upload_to_bq() {
  local file_path=$1
  local dataset=$2
  local table_name=$3

  if [[ -f "$file_path" ]]; then
    echo "Uploading $file_path to $dataset.$EVENT_DATE_$table_name in BigQuery..."
    bq load \
      --autodetect \
      --source_format=CSV \
      --replace \
      "${PROJECT_ID}:${dataset}.${EVENT_DATE}_${table_name}" \
      "$file_path"
    echo "Upload of $file_path completed!"
  else
    echo "File $file_path does not exist. Skipping..."
  fi
}

# Upload files
upload_to_bq "./lorcana-tcg/listings.csv" "$DATASET_LISTINGS" "listings"
upload_to_bq "./lorcana-tcg/product_data.csv" "$DATASET_PRODUCTS" "product_data"
upload_to_bq "./lorcana-tcg/sellers.csv" "$DATASET_SELLERS" "sellers"
