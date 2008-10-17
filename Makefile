#R = /Applications/StartR.app/RAqua.app/Contents/bin/R
R = R

VERSION = 4.1


## Splus source files, note that options.S MUST come first as it has the 
## configurable settings in it. 

## pick up the source files, note that AAoptions.S should always be the
## first file in this bundle
SFILES = src/*.S 
RFILES = src/*.R
# documentation files in R doc format
RDFILES = man/*.Rd
DATE := $(shell date +%F)

all: R


emudir:  version-info 
	rm -rf emu
	mkdir  emu
	sed -e 's/Version: [0-9.-]*/Version: $(VERSION)/' -e 's/Date: [0-9-]*/Date: $(DATE)/' DESCRIPTION > emu/DESCRIPTION
	mkdir -p emu/R
	mkdir -p emu/man 
	mkdir -p emu/data
	rm -f emu/R/* emu/man/*
	cp $(SFILES) emu/R/
	cp $(RFILES) emu/R/
	cp man/*.Rd emu/man
	cp  data/*.* emu/data

check:	emudir
	$Rcmd check emu
#	echo hier
# note that we need --binary below not because we are building binary
# code but because the resulting module won't load in Windows 
# without this
R: $(SFILES) emudir 
	#rm  emu_*gz emu_*zip
	###$R CMD build --binary emu
	#tar xzf emu_$(VERSION)*.tar.gz
	#gzip -r emu_$(VERSION).zip emu
	#tar czf emu_$(VERSION)_R.tar.gz emu

version-info:
	sed -e 's/\(emu\.version<-\)"[0-9.-]*"/\1"$(VERSION)"/' src/AAoptions.S > tmp
	mv tmp src/AAoptions.S



# emu/INDEX: $(RDFILES)
# 	mkdir -p emu
# 	rm -f emu/INDEX
# 	$Rcmd Rdindex $(RDFILES) > emu/INDEX

emu-R.pdf: $(RDFILES)
#	$Rcmd Rd2dvi --pdf --title="Emu/R Documentation" --output=emu-R.pdf  $(RDFILES)
