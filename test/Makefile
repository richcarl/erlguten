include ../conf/include.mk

PDFS = eg_test1.pdf eg_test2.pdf eg_test3.pdf \
	 eg_test4.pdf eg_test5.pdf process.pdf eg_test7.pdf kd_test1.pdf \
	 eg_test6.pdf
#	 test1.pdf test2.pdf # erlguten.erl was broken :-(

ERLGUTEN = ../erlguten

all:  $(ERL_OBJECTS) $(PDFS)

test1.pdf: test1.map galley_001.gal content1.con
	$(ERLGUTEN) test1.map

test2.pdf: test2.xml galley_002.gal
	$(ERLGUTEN) test2.xml

eg_test1.pdf: ../ebin/eg_test1.beam
	erl -pa . -pa ../ebin -noshell -s eg_test1 test -s erlang halt

kd_test1.pdf: ../ebin/kd_test1.beam
	erl -pa . -pa ../ebin -noshell -s kd_test1 test -s erlang halt

eg_test7.pdf: ../ebin/eg_test7.beam
	erl -pa . -pa ../ebin -noshell -s eg_test7 test -s erlang halt

eg_test2.pdf: ../ebin/eg_test2.beam
	erl -pa . -pa ../ebin -noshell -s eg_test2 test -s erlang halt

eg_test3.pdf: ../ebin/eg_test3.beam
	erl -pa . -pa ../ebin -noshell -s eg_test3 test -s erlang halt

eg_test4.pdf: ../ebin/eg_test4.beam
	erl -pa . -pa ../ebin -noshell -s eg_test4 test -s erlang halt

eg_test5.pdf: ../ebin/eg_test5.beam
	erl -pa . -pa ../ebin -noshell -s eg_test5 test -s erlang halt

eg_test6.pdf: ../ebin/eg_test6.beam
	erl -pa . -pa ../ebin -noshell -s eg_test6 test -s erlang halt

process.pdf: ../ebin/tmo_doc.beam process.xml
	erl -pa . -pa ../ebin -noshell -s tmo_doc file -s erlang halt

clean:
	-rm -f $(ERL_OBJECTS) $(PDFS) 
	rm -f erl_crash.dump
