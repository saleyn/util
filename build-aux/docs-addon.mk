docs:
	@mkdir -p build-aux
	@for f in edoc.css md-to-edoc.awk md-to-edoc.sh; do \
    [ -f build-aux/$$f ] || curl -s -o build-aux/$$f https://raw.githubusercontent.com/saleyn/util/master/build-aux/$$f; \
   done
	@sh -c "build-aux/md-to-edoc.sh README.md" > build-aux/overview.edoc
ifneq (,$(filter $(REBAR),rebar3))
	@$(REBAR) edoc
else
	@$(REBAR) doc skip_deps=true
endif

github-docs gh-pages:
	@if git branch | grep -q gh-pages ; then \
		git checkout gh-pages; \
	else \
		git checkout -b gh-pages; \
	fi
	rm -f rebar.lock
	git checkout master -- src include
	git checkout master -- Makefile rebar.*
	make docs
	mv doc/*.* .
	make clean
	rm -fr src c_src include Makefile erl_crash.dump priv rebar.* README* .github .travis* .gitignore
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