#!/bin/sh

echo "Copying data from Upload Bucket"
aws s3 cp s3://$S3_BUCKET/ ./data/ --recursive

echo "Running python script"
python phate.py $File

echo "Running R scripts"
Rscript ./code/bioinformatics_analyses.R $File

echo "Copying results to Download Bucket"
aws s3 cp output.rds s3://geomx-download-bucket/$File-output.rds
