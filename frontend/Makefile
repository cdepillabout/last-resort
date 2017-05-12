.PHONY: build clean install test watch
all: build

build: 
	npm run build

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
			--content-base static/ ; \
	else \
		webpack-dev-server \
			--watch \
			--progress \
			--host "$${WEBPACK_DEV_SERVER_HOST}" \
			--content-base static/ ; \
	fi 
