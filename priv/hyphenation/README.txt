__How to add additional Hyphenation rules for new country/language ?__

* Get the appropriate .tex file/s from TeX (more info about this below). 

  Open Office has similar (.dic) files that may or may not work (and may in 
  turn be based on .tex files) - the main issue is that they support some
  additional hyphenation rules and sometimes have more than one substring rule
  with the same text (when the numbers are removed) - which our code doesn't
  handle - .tex files don't appear to have this issue.

* The .dic file to create and the ones that already exist, have a format
  inspired by Open Office, as I started out with OO .dic files before I realised
  that they didn't quite work.
  
  The exact format is explained in eg_mk_hyphen:start/2.
  
  It should be fairly easy to create a erlguten .dic file by removing the .tex
  "%" commets, "\xxx" commands, "{", "}" markers, replace all non-ascii 
  escape codes with proper latin-1 letters and add the encoding header and 
  possibly a "\hyphenation" block.

  Note: this may later make it simpler to handle OO .dic files fully.

* The new file should be placed in erlguten/priv/hyphenation.

* It should be named "hyphen_languagecode_COUNTRYCODE.dic" 
  e.g. "hyphen_sv_SE.dic" where sv = the Swedish language and 
  SE = Sweden the country. See iso3166 and iso639 for the codes to use.

* eg_mk_hyphen.erl needs an additional start/1 call and additional start/1 
  clause for the new language dic.

* Additional rules must also be added in the Makefile.
- add an entry in ERL_OBJECTS += ....
- add an eg_hyphen_rules_*.erl: ... entry calling the proper 
  eg_mk_hyphen:start/1 clause
- add a ../ebin/eg_hyphen_rules_*.beam: ... entry 
- add eg_hyphen_rules_*.erl to the clean target

* You might also want to add a entry in eg_hyphenate:test/1 to check how well
  the hyphenation rules work for the new language.

Note: The .dic support is still somewhat hackish (2009-08-07), there are 
      several things that can be done to improve it.
    - Support loading any .dic file in "../hyphenation/" without having to 
      update Makefile and .erl files.
    - Check that erlguten can truely handle the dic files fully, 
      hyphen_fi_FI.dic has some rules with spaces in them which will probably 
      not be used as erlguten seams to mostly pass individual words (space 
      separated char sequences) for hyphenation.    
    - tb87nemeth.pdf - "Automatic Non-Standard Hyphenation in OpenOffice"
      explains a bit about some of the extra features supported by OO .dic
      files which aren't supported by erlgutens TeX based solution.  

===============================================================================

The .tex files used have been extracted from the Ubuntu texlive-lang-all 
package (installed using synaptic). 

The individual .tex files when can be found in:
/usr/share/texmf-texlive/tex/generic/hyphen/ 

Note that some languages use several .tex files.

-------------------------------------------------------------------------------

* hyph_en_GB.dic based on ukhyphen.tex (version 2007.dfsg.4-1), ukhyphen.tex
  is the same as the one in ".../erlguten/priv/"
- Hyphenation mostly works.

* hyph_sv_SE.dic is based on svhyph.tex (version 2007.dfsg.4-1)
- Some hyphenations points end up being wrong, especialy between compound words
  and between double consonats.

* hyph_nb_NO.dic is based on nohyphbx.tex (version 2007.dfsg.4-1) 
- Some hyphenations points end up being wrong, especialy between compound words
  and between double consonats.

* hyph_fi_FI.dic is based on fihyph.tex (version 2007.dfsg.4-1)
- Some hyphenations points end up being wrong, especialy between compound words
  and between double consonats.

* hyph_da_DK.dic is based on a merged of dkhyph.tex, dkspecial.tex and 
  dkcommon.tex (version 2007.dfsg.4-1).
- Some hyphenations points end up being wrong, especialy between compound words
  and between double consonats.




