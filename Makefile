clean:
	if [ -d public ]; then rm -r public; fi

build: clean
	hugo

deploy: build
	rsync --delete --recursive ./public $(SERVER_ADDRESS):/var/www/gtf.io
