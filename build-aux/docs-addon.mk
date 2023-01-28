GIT_ROOT=$(shell A=$$(git rev-parse --show-toplevel); [ -z $$A ] && echo ".git" || echo "$$A/.git")
MASTER=$(shell [ -f $(GIT_ROOT)/refs/heads/master ] && echo master || echo main)

info::
	@echo "make docs                                  - Generate documentation"
	@echo "make get-version                           - Get version of $(PROJECT)"
	@echo "make set-version version=X.Y.Z             - Set version of $(PROJECT)"
	@echo "make gh-pages                              - Generate and push Github pages"
	@echo "make clean-docs                            - Delete generated documentation"

docs::
	$(REBAR) ex_doc

clean-docs::
	@rm -f doc/*.{css,html,png} doc/edoc-info

get-version set-version: APPFILE:=$(shell find . -name $(PROJECT).app.src)
get-version set-version: PROJECT:=$(if $(PROJECT),$(PROJECT),$(notdir $(PWD)))
get-version:
	@echo   "App file: $(APPFILE)"
	@printf "%-20s: %s\n" "$(notdir $(APPFILE))" "$$(sed -n 's/.*{vsn, *\"\([0-9]\+\)\(\(\.[0-9]\+\)\+\)\"}.*/\1\2/p' $(APPFILE))"
	@printf "%-20s: %s\n" "rebar.config" "$$(sed -n 's/.*{$(PROJECT), *\"\([0-9]\+\)\(\(\.[0-9]\+\)\+\)\"}.*/\1\2/p' rebar.config)"

set-version:
	@[ -z $(version) ] && echo "Missing version=X.Y.Z!" && exit 1 || true
	@sed -i "s/{vsn, \"\([0-9]\+\)\(\(\.[0-9]\+\)\+\)\"}/{vsn, \"$(version)\"}/" $(APPFILE)
	@sed -i "s/{$(PROJECT), \"[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+\"}/{$(PROJECT), \"$(version)\"}/" rebar.config

github-docs gh-pages: GVER=$(shell git ls-tree --name-only -r $(MASTER) build-aux | grep 'google.*\.html')
github-docs gh-pages: LOCAL_GVER=$(notdir $(GVER))
github-docs gh-pages:
	@# The git config params must be set when this target is executed by a GitHub workflow
	@[ -z "$(git config user.name)" ] && \
		git config user.name  github-actions && \
		git config user.email github-actions@github.com
	@if git branch | grep -q gh-pages ; then \
		git checkout gh-pages; \
	else \
		git checkout -b gh-pages; \
	fi
	@echo "Git root: $(git rev-parse --show-toplevel)"
	@echo "Main branch: $(MASTER)"
	rm -f rebar.lock
	git checkout $(MASTER) -- src $(shell [ -d include ] && echo include)
	git checkout $(MASTER) -- Makefile rebar.* README.md $(GH_PAGES_FILES)
	git show $(MASTER):LICENSE >2/dev/null > LICENSE
	@# Create google verification file if one exists in the master
	[ -n "$(GVER)" ] && git show $(MASTER):$(GVER) 2>/dev/null > "$(LOCAL_GVER)" || true
	make docs
	mv doc/*.* .
	make clean
	find . -maxdepth 1 -type d -not -name ".git" -a -not -name "." -exec rm -fr {} \;
	find . -maxdepth 1 -type f -not -name ".git" -a -not -name "*.html" -a -not -name "*.css" -a -not -name "*.js" -a -not -name "*.png" -exec rm -f {} \;
	@FILES=`git status -uall --porcelain | sed -n '/^?? [A-Za-z0-9]/{s/?? //p}'`; \
	for f in $$FILES ; do \
		echo "Adding $$f"; git add $$f; \
	done
	@sh -c "ret=0; set +e; \
		if   git commit -a --amend -m 'Documentation updated'; \
		then git push origin +gh-pages; echo 'Pushed gh-pages to origin'; \
		else ret=1; git reset --hard; \
		fi; \
		set -e; git checkout $(MASTER) && echo 'Switched to $(MASTER)'; exit $$ret"

