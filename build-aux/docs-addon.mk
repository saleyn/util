docs:
	$(prep-docs)
ifeq (rebar3,$(REBAR))
	@$(REBAR) edoc
else ifeq (rebar,$(REBAR))
	@$(REBAR) doc skip_deps=true
else
	rebar3 edoc
endif

define prep-docs =
	@mkdir -p build-aux
	@for f in docs-addon.mk edoc.css md-to-edoc.awk md-to-edoc.sh; do \
    [ -f build-aux/$$f ] || curl -s -o build-aux/$$f https://raw.githubusercontent.com/saleyn/util/master/build-aux/$$f; \
   done
	@sh build-aux/md-to-edoc.sh README.md > build-aux/overview.edoc
endef

github-docs gh-pages: GVER=$(shell git ls-tree --name-only -r master build-aux | grep 'google.*\.html')
github-docs gh-pages: LOCAL_GVER=$(notdir $(GVER))
github-docs gh-pages:
	@if git branch | grep -q gh-pages ; then \
		git checkout gh-pages; \
	else \
		git checkout -b gh-pages; \
	fi
	rm -f rebar.lock
	git checkout master -- src $(shell [ -d include ] && echo include)
	git checkout master -- Makefile rebar.* README.md
	$(prep-docs)
	@# Create google verification file if one exists in the master
	[ -n "$(GVER)" ] && git show master:$(GVER) 2>/dev/null > "$(LOCAL_GVER)" || true
	make docs
	mv doc/*.* .
	make clean
	rm -fr src test c_src include Makefile erl_crash.dump priv rebar.* \
		     README* .github .travis* .gitignore _build build-aux
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

