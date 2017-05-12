.PHONY: build build-prod clean install test watch
all: build

build: 
	npm run build

build-prod:
	NODE_ENV=production npm run build

clean:
	npm run clean

install:
	npm install

test:
	npm test

watch:
	if [ -z "$${WEBPACK_DEV_SERVER_HOST}" ]; then \
		webpack-dev-server \
			--watch \
			--progress \
			--hot \
			--content-base static/ ; \
	else \
		webpack-dev-server \
			--watch \
			--progress \
			--hot \
			--host "$${WEBPACK_DEV_SERVER_HOST}" \
			--content-base static/ ; \
	fi 
