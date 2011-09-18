include vsn.mk
APPNAME=erlguten
DOC_OPTS={def,{version,\"$(VSN)\"}}
ERLGUTEN=./erlguten
ERL=erl
ERLC=erlc
ERLC_FLAGS=+debug_info +nowarn_unused_vars
PERL=perl
APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

HYPHEN_DICTS = $(wildcard priv/hyphenation/hyph_*.dic)
HYPHEN_SOURCES = $(HYPHEN_DICTS:priv/hyphenation/hyph_%.dic=priv/src/eg_hyphen_rules_%.erl)
HYPHEN_OBJECTS = $(HYPHEN_SOURCES:priv/src/%.erl=ebin/%.beam)

ERL_SOURCES = $(wildcard src/*.erl)
MODULES = $(ERL_SOURCES:src/%.erl=%)
ERL_OBJECTS = $(ERL_SOURCES:src/%.erl=ebin/%.beam) \
	ebin/eg_hyphen_rules.beam ebin/eg_font_map.ebin $(HYPHEN_OBJECTS)

TEST_SOURCES = $(wildcard test/*.erl)
TEST_PDFS = $(TEST_SOURCES:test/%.erl=test/%.pdf)
TEST_OBJECTS = $(TEST_SOURCES:test/%.erl=test/%.beam)

.PHONY: all test doc

all: $(ERL_OBJECTS) app

test: all $(TEST_PDFS)

docs: doc
doc: doc/index.html

app: ebin/$(APPNAME).app

clean:
	-rm -f $(ERL_OBJECTS) ebin/$(APPNAME).app
	-cd priv/src && $(MAKE) clean
	-rm -f $(TEST_OBJECTS) $(TEST_PDFS)
	-rm -f doc/*.html doc/stylesheet.css doc/erlang.png doc/edoc-info

doc/index.html: doc/overview.edoc $(ERL_SOURCES)
	$(ERL) -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop

ebin/%.beam: src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

ebin/%.app: src/%.app.src vsn.mk
	$(PERL) -e $(APPSCRIPT) "$(VSN)" $(MODULES) < $< > $@

# .dic file got updated so recreate the matching eg_hyphen_rules_*.erl file
# by calling eg_mk_hyphen:start/1

priv/src/eg_hyphen_rules_%.erl: priv/hyphenation/hyph_%.dic
	$(ERL) -noshell -pa ebin -s eg_mk_hyphen start $* -s erlang halt

# eg_mk_hyphen.erl recompiled so regenerate all the eg_hyphen_rules_*.erl
# files by calling eg_mk_hyphen:start/0
priv/src/eg_hyphen_rules.erl: ebin/eg_mk_hyphen.beam priv/hyphenation/ukhyphen.tex
	 $(ERL) -noshell -pa ebin -s eg_mk_hyphen start -s erlang halt

# eg_hyphen_rules_*.erl file was created so recompile it
ebin/eg_hyphen_rules_%.beam: priv/src/eg_hyphen_rules_%.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

ebin/eg_hyphen_rules.beam: priv/src/eg_hyphen_rules.erl

ebin/eg_font_map.ebin: priv/src/Makefile
	-cd priv/src && $(MAKE)

priv/src/Makefile: ebin/eg_afm.beam priv/font_locations
	$(ERL) -noshell -pa ebin -s eg_afm make -s erlang halt

test1.pdf: test1.map galley_001.gal content1.con
	$(ERLGUTEN) test1.map

test2.pdf: test2.xml galley_002.gal
	$(ERLGUTEN) test2.xml

tmo_doc.pdf: tmo_doc.xml

test/%.pdf: test/%.beam
	cd test && erl -pa . -pa ../ebin -noshell -s $* test -s erlang halt

test/%.beam: test/%.erl
	$(ERLC) $(ERLC_FLAGS) -o test $<
