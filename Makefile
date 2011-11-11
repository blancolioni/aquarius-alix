ARCH := $(shell arch)

PROJECT=aquarius_$(ARCH)

all: projects/$(PROJECT).gpr aquarius

projects/aquarius_x86_64.gpr: projects/aquarius.gpr
	cp projects/aquarius.gpr projects/aquarius_x86_64.gpr

projects/aquarius_i686.gpr: projects/aquarius.gpr
	sed s/lib64/lib/g projects/aquarius.gpr > projects/aquarius_i686.gpr

aquarius:
	gnatmake -Pprojects/$(PROJECT)

clean:
	rm -f `find . -name "*.ali" -print`
	rm -f `find . -name "*.o" -print`
	rm -f `find . -name "*~" -print`
	rm -f `find . -name "trace.txt" -print`
	rm -f doc/aquarius.{aux,idx,log,dvi,lof,lot,toc,pdf}
	rm -f bin/*
	rm -f obj/*
	rm -f projects/aquarius_x86_64.gpr projects/aquarius_i686.gpr

distclean: clean
	rm -f `find . -name "*~" -print`

