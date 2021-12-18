# h/t to @jimhester and @yihui for this parse block:
# https://github.com/yihui/knitr/blob/dc5ead7bcfc0ebd2789fe99c527c7d91afb3de4a/Makefile#L1-L4
# Note the portability change as suggested in the manual:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-portable-packages
PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`


all: check

build: install_deps
	R CMD build .

check: build
	R CMD check --no-manual $(PKGNAME)_$(PKGVERS).tar.gz

install_deps:
	Rscript \
	-e 'if (!requireNamespace("remotes")) install.packages("remotes")' \
	-e 'remotes::install_deps(dependencies = TRUE)'

install: build
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	@rm -rf $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck

format:
	Rscript -e 'styler::style_pkg()'

test:
	Rscript -e 'devtools::test()'

lint:
	Rscript -e 'lintr::lint_package()'

coverage:
	Rscript -e 'covr::package_coverage()'

readme:
	Rscript -e 'devtools::build_readme()'

docs: build readme
	Rscript -e 'devtools::document()'
	Rscript -e 'pkgdown::build_site()'

release:
	npx standard-version
	#npx standard-version --release-as minor
	#npx standard-version -f  # first release

releasetest:
	npx standard-version --dry-run
