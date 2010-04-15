SUBDIRS=src priv/src
APPNAME=erlguten
DOC_OPTS={def,{version,\"$(EG_VSN)\"}}
ERL=erl

include vsn.mk

.PHONY: all conf test $(SUBDIRS)

all: conf $(SUBDIRS) test docs

conf test:
	cd $@ && $(MAKE)

$(SUBDIRS):
	cd $@ && $(MAKE)

docs: doc/index.html

doc/index.html: doc/overview.edoc $(wildcard src/*.erl)
	$(ERL) -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop

clean:
	cd src && $(MAKE) clean
	cd test && $(MAKE) clean
