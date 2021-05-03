#!/bin/sh
echo "S3_BUCKET: " $S3_BUCKET
echo "File: " $File
echo s3://$S3_BUCKET/$File

aws s3 cp s3://$S3_BUCKET/ ./data/ --recursive

# curl -i -H "Authorization: token '$GITHUB_TOKEN'" https://api.github.com/user/repos -d '{"name":"geomx_app", "auto_int": true}'

echo "Running R scripts"
# Rscript ./myScript.R
Rscript ./code/bioinformatics_analyses.R $File


aws s3 cp output.rds s3://geomx-download-bucket/$File-output.rds
