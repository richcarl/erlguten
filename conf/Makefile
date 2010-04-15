
all: config.status

config.status: configure
	./configure

configure: configure.ac
	autoconf

clean:
	-rm -f config.cache config.log config.status configure $(MK_INCLUDE) $(SYSWATCH_START)


