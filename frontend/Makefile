.PHONY: build build-app build-app-prod build-prod build-ps clean install test watch watch-ps watch-app
all: build

build: build-ps build-app
	pulp build

build-app:
	webpack --progress --profile --colors

build-app-prod:
	NODE_ENV=production webpack --progress --profile --colors

build-ps:
	pulp build

build-prod: build-ps build-app

clean:
	rm -rf ./static/js/dist/
	rm -rf ./output/

install:
	npm install

test:
	npm test

watch: build-ps
	$(MAKE) watch-ps & $(MAKE) watch-app

watch-app:
	if [ -z "$${WEBPACK_DEV_SERVER_HOST}" ]; then \
		webpack-dev-server \
			--progress; \
	else \
		webpack-dev-server \
			--progress \
			--host "$${WEBPACK_DEV_SERVER_HOST}"; \
	fi 

watch-ps:
	pulp --watch build
