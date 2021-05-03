STACK_NAME=geomx
S3_BUCKET=geomx-s3-bucket
UPLOAD_BUCKET=geomx-upload-bucket
DOWNLOAD_BUCKET=geomx-download-bucket
TEMPLATE_FILE=./template.yml
OUTPUT_FILE=output.yaml
PARAMETERS_FILE=parameters.json
REGION=us-east-1

bucket:
	aws s3 mb s3://$(S3_BUCKET) --region $(REGION)
	aws s3 cp ./infrastructure/ s3://$(S3_BUCKET) --recursive

validate:
	aws cloudformation validate-template --template-body file://template.yml

package:
	aws cloudformation package --template-file template.yml --s3-bucket $(S3_BUCKET) --s3-prefix geomx --output-template-file output.yaml

stack:
	aws cloudformation create-stack --stack-name $(STACK_NAME) --template-body file://$(OUTPUT_FILE) --parameters file://$(PARAMETERS_FILE) --region=$(REGION) --capabilities CAPABILITY_NAMED_IAM

update:
	aws cloudformation update-stack --stack-name $(STACK_NAME) --template-body file://$(OUTPUT_FILE) --parameters file://$(PARAMETERS_FILE) --region=$(REGION) --capabilities CAPABILITY_NAMED_IAM

delete: 
	aws s3 rb s3://$(S3_BUCKET) --force
	aws s3 rb s3://$(UPLOAD_BUCKET) --force
	aws s3 rb s3://$(DOWNLOAD_BUCKET) --force
	aws cloudformation delete-stack --stack-name $(STACK_NAME)


