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
			--progress; \
	else \
		webpack-dev-server \
			--progress \
			--host "$${WEBPACK_DEV_SERVER_HOST}"; \
	fi 
