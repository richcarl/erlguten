include ../include.mk

HYPHEN_DICTS = $(wildcard ../priv/hyphenation/hyph_*.dic)
HYPHEN_SOURCES = $(HYPHEN_DICTS:../priv/hyphenation/hyph_%.dic=../priv/src/eg_hyphen_rules_%.erl)
HYPHEN_OBJECTS = $(HYPHEN_SOURCES:../priv/src/%.erl=../ebin/%.beam)

ERL_OBJECTS += ../ebin/eg_hyphen_rules.beam $(HYPHEN_OBJECTS)

all: $(ERL_OBJECTS) fontmap

# .dic file got updated so recreate the matching eg_hyphen_rules_*.erl file
# by calling eg_mk_hyphen:start/1

../priv/src/eg_hyphen_rules_%.erl: ../priv/hyphenation/hyph_%.dic
	$(ERL) -noshell -pa ../ebin -s eg_mk_hyphen start $* -s erlang halt

# eg_mk_hyphen.erl recompiled so regenerate all the eg_hyphen_rules_*.erl 
# files by calling eg_mk_hyphen:start/0
../priv/src/eg_hyphen_rules.erl: ../ebin/eg_mk_hyphen.beam ../priv/hyphenation/ukhyphen.tex
	 $(ERL) -noshell -pa ../ebin -s eg_mk_hyphen start -s erlang halt

# eg_hyphen_rules_*.erl file was created so recompile it
../ebin/eg_hyphen_rules_%.beam: ../priv/src/eg_hyphen_rules_%.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

../ebin/eg_hyphen_rules.beam: ../priv/src/eg_hyphen_rules.erl
../ebin/eg_mk_hyphen.beam : eg_mk_hyphen.erl

fontmap: ../ebin/eg_font_map.ebin

../ebin/eg_font_map.ebin: ../priv/src/Makefile
	-cd ../priv/src && $(MAKE)

../priv/src/Makefile: Makefile ../ebin/eg_afm.beam ../priv/font_locations
	$(ERL) -noshell -pa ../ebin -s eg_afm make -s erlang halt

clean:
	-rm -f $(ERL_OBJECTS)
	-rm -f ../doc/*.html ../doc/stylesheet.css ../doc/erlang.png \
	  ../doc/edoc-info
	-cd ../priv/src && $(MAKE) clean

docs: ../doc/index.html

../doc/index.html: $(ERL_OBJECTS:%.beam:%.erl)
	erl -noshell -run edoc_run application "erlguten" '".."' '[]'

