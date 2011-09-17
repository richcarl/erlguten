
all: config.status

config.status: configure
	./configure

configure: configure.ac
	autoconf

clean:
	-rm -f configure include.mk \
	config.cache config.log config.status
	-rm -rf autom4te.cache
