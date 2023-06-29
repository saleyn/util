# See LICENSE for licensing information.

ifeq ($(shell uname -o), Cygwin)
	EXT=".cmd"
else
  EXT=
endif

.PHONY: all all-fast clean clean-docs github-docs tar

PROJECT := $(notdir $(PWD))
TARBALL := $(PROJECT)

REBAR   := $(shell whereis rebar3 2>/dev/null | awk '{print $$2}')
REBAR   := $(if $(REBAR),$(REBAR),rebar)$(EXT)

empty   :=
space   := $(empty) $(empty)
delim   := $(empty),\n        $(empty)

all: compile

compile:
	@$(REBAR) compile
	@if ! which elixirc &>/dev/null; then \
		true; \
	else \
	 	for f in src/*.ex; do elixirc -o _build/default/lib/util/ebin --ignore-module-conflict $$f; done; \
	fi

test eunit:
	@$(REBAR) eunit

-include build-aux/docs-addon.mk

clean:
	@$(REBAR) clean
	@rm -fr ebin doc

doc:
	$(REBAR) ex_doc

publish cut:
	$(REBAR) hex $@ -r hexpm $(if $(replace),--replace) $(if $(noconfirm),--yes)

tar:
	@rm -f $(TARBALL).tgz; \
    tar zcf $(TARBALL).tgz --transform 's|^|$(TARBALL)/|' --exclude="core*" --exclude="erl_crash.dump" \
		--exclude="*.tgz" --exclude="*.swp" --exclude="c_src" \
		--exclude="Makefile" --exclude="rebar.*" --exclude="*.mk" \
		--exclude="*.o" --exclude=".git*" * && \
		echo "Created $(TARBALL).tgz"

build-aux/docs-addon.mk:
	git co master build-aux/docs-addon.mk

.PHONY: test
.SUFFIX:
