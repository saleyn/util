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
	@FILES="$(shell git st -uall --porcelain | sed -n '/^?? [A-Za-z0-9]/{s/?? //p}')"; \
		if [ -n "$$FILES" ] ; then echo "Adding: $$FILES"; fi

github-docs:
	@if git branch | grep -q gh-pages ; then \
		git checkout gh-pages; \
	else \
		git checkout -b gh-pages; \
	fi
	git checkout master src include Makefile rebar.*
	make docs
	make clean
	rm -fr ebin src include Makefile erl_crash.dump rebar.* README*
	mv doc/*.* .
	rm -fr doc
	@FILES="$(shell git st -uall --porcelain | sed -n '/^?? [A-Za-z0-9]/{s/?? //p}')"; \
	for f in $$FILES ; do \
		echo "Adding $$f"; git add $$f; \
	done
	sh -c "ret=0; set +e; \
		if   git commit -a --amend -m 'Documentation updated'; \
		then git push origin +gh-pages; \
		else ret=1; git reset --hard; \
		fi; \
		set -e; git checkout master; git branch -D gh-pages; exit $$ret"

tar:
	@rm -f $(TARBALL).tgz; \
    tar zcf $(TARBALL).tgz --transform 's|^|$(TARBALL)/|' --exclude="core*" --exclude="erl_crash.dump" \
		--exclude="*.tgz" --exclude="*.swp" --exclude="c_src" \
		--exclude="Makefile" --exclude="rebar.*" --exclude="*.mk" \
		--exclude="*.o" --exclude=".git*" * && \
		echo "Created $(TARBALL).tgz"
