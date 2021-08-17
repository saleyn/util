docs:
	@mkdir -p build-aux
	@for f in edoc.css md-to-edoc.awk md-to-edoc.sh; do \
    [ -f build-aux/$$f ] || curl -s -o build-aux/$$f https://raw.githubusercontent.com/saleyn/util/master/build-aux/$$f; \
   done
	@sh build-aux/md-to-edoc.sh README.md > build-aux/overview.edoc
ifeq (rebar3,$(REBAR))
	@$(REBAR) edoc
else ifeq (rebar,$(REBAR))
	@$(REBAR) doc skip_deps=true
else
	rebar3 edoc
endif

github-docs gh-pages:
	@if git branch | grep -q gh-pages ; then \
		git checkout gh-pages; \
	else \
		git checkout -b gh-pages; \
	fi
	rm -f rebar.lock
	git checkout master -- src $(shell [ -d include ] && echo include)
	git checkout master -- Makefile rebar.* README.md
	git checkout master -- build-aux/docs-addon.mk || \
		curl -s -o build-aux/docs-addon.mk https://raw.githubusercontent.com/saleyn/util/master/build-aux/docs-addon.mk
	@# Create google verification file if one exists in the master
	GOOG=$$(git ls-tree --name-only master build-aux/google*.html)
	[ -n "$$GOOG" ] && git show master:$${GOOG} 2>/dev/null > $$(basename $${GOOG}) || true
	git checkout master -- build-aux/google*.html || true
	make docs
	mv doc/*.* .
	make clean
	rm -fr src c_src include Makefile erl_crash.dump priv rebar.* README* .github .travis* .gitignore _build
	@FILES=`git status -uall --porcelain | sed -n '/^?? [A-Za-z0-9]/{s/?? //p}'`; \
	for f in $$FILES ; do \
		echo "Adding $$f"; git add $$f; \
	done
	@sh -c "ret=0; set +e; \
		if   git commit -a --amend -m 'Documentation updated'; \
		then git push origin +gh-pages; echo 'Pushed gh-pages to origin'; \
		else ret=1; git reset --hard; \
		fi; \
		set -e; git checkout master && echo 'Switched to master'; exit $$ret"
