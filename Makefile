# See LICENSE for licensing information.

PROJECT = $(notdir $(PWD))
TARBALL = $(PROJECT)

REBAR = rebar

all:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

docs: all clean-docs
	@$(REBAR) doc skip_deps=true

clean-docs:
	rm -f doc/*.{css,html,png} doc/edoc-info

info:
	FILES="$(shell git st -uall --porcelain | sed -n '/^??/{s/?? //p}'); \
		if [ -n $$FILES ] ; then echo "Adding: $$FILES"; git add $$FILES ; fi

github-docs:
	git checkout gh-pages
	git checkout master src Makefile rebar.*
	make docs
	make clean
	rm -fr ebin src Makefile erl_crash.dump rebar.*
	mv doc/*.* .
	rm -fr doc
	@for f in $(shell git st -uall --porcelain | sed -n '/^\?\?/{s/\?\? //p}') ; do \
		echo "Adding $$f"; git add $$f; \
	done
	sh -c "ret=0; set +e; \
		if git commit -a --amend; then git push origin +gh-pages; else ret=1; git reset --hard; fi; \
		set -e; git checkout master; exit $$ret"

tar:
	@rm -f $(TARBALL).tgz; \
    tar zcf $(TARBALL).tgz --transform 's|^|$(TARBALL)/|' --exclude="core*" --exclude="erl_crash.dump" \
		--exclude="*.tgz" --exclude="*.swp" --exclude="c_src" \
		--exclude="Makefile" --exclude="rebar.*" --exclude="*.mk" \
		--exclude="*.o" --exclude=".git*" * && \
		echo "Created $(TARBALL).tgz"
