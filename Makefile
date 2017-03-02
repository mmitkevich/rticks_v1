PACKAGE = $(shell grep '^Package: ' DESCRIPTION | awk '{ print $$2 }')
VERSION = $(shell grep '^Version: ' DESCRIPTION | awk '{ print $$2 }')
PARENTDIR = .
TARBALL = $(PACKAGE)_$(VERSION).tar.gz
TARBALL_LOC = $(PARENTDIR)/$(TARBALL)
CHECKDIR = $(PARENTDIR)/$(PACKAGE).Rcheck

all: install cpp_examples
#doc check install cpp deps build 

deps:
	Rscript -e 'devtools::install_deps()'

doc:
	Rscript -e 'devtools::document(roclets=c("rd", "collate", "namespace", "vignette"))'

check:
	Rscript -e 'devtools::check(check_version = TRUE, force_suggests = TRUE)'

test:
	Rscript -e 'devtools::test()'

install:
	Rscript -e 'devtools::install()'

build:
	Rscript -e 'devtools::build()'

build-win:
	Rscript -e 'devtools::build_win()'

vignettes:
	Rscript -e 'devtools::build_vignettes()'

submit:
	Rscript -e 'devtools::submit_cran()'

clean:
	rm -f $(TARBALL_LOC)
	rm -rf $(CHECKDIR)
	rm -rf src/*.o
	rm -rf src/rticks.so
	make --directory=cpp clean

cpp_examples:
	make --directory=cpp all
.PHONY: build
