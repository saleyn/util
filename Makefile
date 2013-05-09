# See LICENSE for licensing information.

.PHONY: all all-fast clean clean-docs github-docs tar

PROJECT := $(notdir $(PWD))
TARBALL := $(PROJECT)

REBAR   := rebar

empty   :=
space   := $(empty) $(empty)
delim   := $(empty),\n        $(empty)

all:
	@$(REBAR) compile

# This is just an example of using make instead of rebar to do fast compilation
all-fast: $(patsubst src/%.app.src,ebin/%.app,$(wildcard src/*.app.src))

ebin/%.app: src/%.app.src $(wildcard src/*.erl)
	@sed 's!{modules, *\[.*\]!{modules, [\
        $(subst $(space),$(delim),$(sort $(basename $(notdir $(filter-out $<,$^)))))]!' \
		$< > $@
	erlc +debug_info -I include -o ebin $(filter-out $<,$?)

clean:
	@$(REBAR) clean
	@rm -fr ebin doc

docs: doc ebin clean-docs
	@$(REBAR) doc skip_deps=true

doc ebin:
	mkdir -p $@

clean-docs:
	rm -f doc/*.{css,html,png} doc/edoc-info

github-docs:
	@if git branch | grep -q gh-pages ; then \
		git checkout gh-pages; \
	else \
		git checkout -b gh-pages; \
	fi
	git checkout master src include Makefile rebar.*
	make docs
	mv doc/*.* .
	make clean
	rm -fr src include Makefile erl_crash.dump rebar.* README*
	@FILES=`git st -uall --porcelain | sed -n '/^?? [A-Za-z0-9]/{s/?? //p}'`; \
	for f in $$FILES ; do \
		echo "Adding $$f"; git add $$f; \
	done
	@sh -c "ret=0; set +e; \
		if   git commit -a --amend -m 'Documentation updated'; \
		then git push origin +gh-pages; echo 'Pushed gh-pages to origin'; \
		else ret=1; git reset --hard; \
		fi; \
		set -e; git checkout master && echo 'Switched to master'; exit $$ret"

tar:
	@rm -f $(TARBALL).tgz; \
    tar zcf $(TARBALL).tgz --transform 's|^|$(TARBALL)/|' --exclude="core*" --exclude="erl_crash.dump" \
		--exclude="*.tgz" --exclude="*.swp" --exclude="c_src" \
		--exclude="Makefile" --exclude="rebar.*" --exclude="*.mk" \
		--exclude="*.o" --exclude=".git*" * && \
		echo "Created $(TARBALL).tgz"
