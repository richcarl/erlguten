include ../include.mk
ERLGUTEN = ../erlguten

PDFS = eg_test1.pdf eg_test2.pdf eg_test3.pdf \
	 eg_test4.pdf eg_test5.pdf eg_test7.pdf kd_test1.pdf \
	 eg_test6.pdf tmo_doc.pdf
#	 test1.pdf test2.pdf # erlguten.erl was broken :-(

all:  $(ERL_OBJECTS) $(PDFS)

test1.pdf: test1.map galley_001.gal content1.con
	$(ERLGUTEN) test1.map

test2.pdf: test2.xml galley_002.gal
	$(ERLGUTEN) test2.xml

tmo_doc.pdf: tmo_doc.xml

%.pdf: ../ebin/%.beam
	erl -pa . -pa ../ebin -noshell -s $* test -s erlang halt

clean:
	-rm -f $(ERL_OBJECTS) $(PDFS)
