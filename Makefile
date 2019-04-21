clean:
	if [ -d public ]; then rm -r public; fi

build: clean
	hugo
