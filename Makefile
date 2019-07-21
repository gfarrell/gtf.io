GIT_COMMIT_HASH = $(shell git rev-parse HEAD)

clean:
	if [ -d public ]; then rm -r public; fi

build: clean
	hugo
	find ./public -name "*.html" -exec sed -i -e "s/%REV%/${GIT_COMMIT_HASH}/g" {} +

deploy: build
	rsync --delete --recursive ./public $(SERVER_ADDRESS):/var/www/gtf.io
