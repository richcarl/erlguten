ERLC_FLAGS=+nowarn_unused_vars +nowarn_unused_function
include ../conf/include.mk

ERL_OBJECTS += ../ebin/eg_hyphen_rules.beam \
../ebin/eg_hyphen_rules_da_DK.beam \
../ebin/eg_hyphen_rules_en_GB.beam \
../ebin/eg_hyphen_rules_fi_FI.beam \
../ebin/eg_hyphen_rules_nb_NO.beam \
../ebin/eg_hyphen_rules_sv_SE.beam

all: $(ERL_OBJECTS) $(APP_OBJECTS) fontmap

# .dic file got updated so recreate the matching eg_hyphen_rules_*.erl file
# by calling eg_mk_hyphen:start/1
eg_hyphen_rules_da_DK.erl: ../priv/hyphenation/hyph_da_DK.dic
	$(ERL) -noshell -pa ../ebin -s eg_mk_hyphen start da_DK -s erlang halt

eg_hyphen_rules_en_GB.erl: ../priv/hyphenation/hyph_en_GB.dic
	$(ERL) -noshell -pa ../ebin -s eg_mk_hyphen start en_GB -s erlang halt

eg_hyphen_rules_fi_FI.erl: ../priv/hyphenation/hyph_fi_FI.dic
	$(ERL) -noshell -pa ../ebin -s eg_mk_hyphen start fi_FI -s erlang halt

eg_hyphen_rules_nb_NO.erl: ../priv/hyphenation/hyph_nb_NO.dic
	$(ERL) -noshell -pa ../ebin -s eg_mk_hyphen start nb_NO -s erlang halt

eg_hyphen_rules_sv_SE.erl: ../priv/hyphenation/hyph_sv_SE.dic
	$(ERL) -noshell -pa ../ebin -s eg_mk_hyphen start sv_SE -s erlang halt


# eg_mk_hyphen.erl recompiled so regenerate all the eg_hyphen_rules_*.erl 
# files by calling eg_mk_hyphen:start/0
eg_hyphen_rules.erl: ../ebin/eg_mk_hyphen.beam ../priv/hyphenation/ukhyphen.tex
	 $(ERL) -noshell -pa ../ebin -s eg_mk_hyphen start -s erlang halt

# eg_hyphen_rules_*.erl file was created so recompile it
../ebin/eg_hyphen_rules_da_DK.beam: eg_hyphen_rules_da_DK.erl
../ebin/eg_hyphen_rules_en_GB.beam: eg_hyphen_rules_en_GB.erl
../ebin/eg_hyphen_rules_fi_FI.beam: eg_hyphen_rules_fi_FI.erl
../ebin/eg_hyphen_rules_nb_NO.beam: eg_hyphen_rules_nb_NO.erl
../ebin/eg_hyphen_rules_sv_SE.beam: eg_hyphen_rules_sv_SE.erl

../ebin/eg_hyphen_rules.beam: eg_hyphen_rules.erl
../ebin/eg_mk_hyphen.beam : eg_mk_hyphen.erl

fontmap: ../priv/src/Makefile

../priv/src/Makefile: Makefile ../ebin/eg_afm.beam ../priv/font_locations
	$(ERL) -noshell -pa ../ebin -s eg_afm make -s erlang halt

clean:
	-rm ../priv/src/Makefile
	-rm -f $(ERL_OBJECTS) eg_hyphen_rules.erl \
		eg_hyphen_rules_da_DK.erl \
		eg_hyphen_rules_en_GB.erl \
		eg_hyphen_rules_fi_FI.erl \
		eg_hyphen_rules_nb_NO.erl \
		eg_hyphen_rules_sv_SE.erl
	-rm -f ../doc/*.html ../doc/stylesheet.css ../doc/erlang.png \
	  ../doc/edoc-info
	-rm -f erl_crash.dump

docs: ../doc/index.html

../doc/index.html: $(ERL_OBJECTS:%.beam:%.erl)
	erl -noshell -run edoc_run application "erlguten" '".."' '[]'
