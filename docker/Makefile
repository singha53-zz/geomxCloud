USER_NAME=singha53
IMAGE_NAME=geomx
VERSION=0.95

build: 
	docker build --no-cache -t $(USER_NAME)/$(IMAGE_NAME):v$(VERSION) .

push: 
	docker push $(USER_NAME)/$(IMAGE_NAME):v$(VERSION)