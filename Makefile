help:
	@echo "devel targets: git-tag, sdist, version, git-push, upload, copy"

README.html: README.md
	pandoc -s $< > $@

sdist: man README.html
	./make-dist $(VERSION)

man: man/cabal-rpm.1

man/cabal-rpm.1: man/cabal-rpm.1.md
	pandoc -s -t man $< > $@

upload:
	cabal upload dist/$(NAME)-$(VERSION).tar.gz

NAME= cabal-rpm
VERSION := $(shell sed -ne 's/^[Vv]ersion:[[:space:]]*//p' $(NAME).cabal)

version:
	@echo $(VERSION)

git-tag:
	git tag $(VERSION)

git-push:
	git push
	git push --tags

copy:
	cp -p dist/$(NAME)-$(VERSION).tar.gz ~/fedora/haskell/cabal-rpm/master
