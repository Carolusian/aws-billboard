#!/bin/bash

dir=$(cd -P -- "$(dirname -- "$0")" && pwd -P)
echo $dir

# Get qutation for elasticache
wget https://pricing.us-east-1.amazonaws.com/offers/v1.0/aws/AmazonElastiCache/current/index.csv -O ElastiCache.csv
tail -n +6 ElastiCache.csv  | tee RI-Quotation-ElastiCache.csv > /dev/null

# Get qutation for RDS
wget https://pricing.us-east-1.amazonaws.com/offers/v1.0/aws/AmazonRDS/current/index.csv -O RDS.csv
tail -n +6 RDS.csv  | tee RI-Quotation-RDS.csv > /dev/null

# Get qutation for EC2
wget https://pricing.us-east-1.amazonaws.com/offers/v1.0/aws/AmazonEC2/current/index.csv -O EC2.csv
tail -n +6 EC2.csv  | tee RI-Quotation-EC2.csv > /dev/null

Rscript ri_quotation.R
