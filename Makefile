# GNU Make workspace makefile autogenerated by Premake

ifndef config
  config=debug
endif

ifndef verbose
  SILENT = @
endif

ifeq ($(config),debug)
  aleph_config = debug
endif
ifeq ($(config),release)
  aleph_config = release
endif
ifeq ($(config),dist)
  aleph_config = dist
endif

PROJECTS := aleph

.PHONY: all clean help $(PROJECTS) 

all: $(PROJECTS)

aleph:
ifneq (,$(aleph_config))
	@echo "==== Building aleph ($(aleph_config)) ===="
	@${MAKE} --no-print-directory -C . -f aleph.make config=$(aleph_config)
endif

clean:
	@${MAKE} --no-print-directory -C . -f aleph.make clean

help:
	@echo "Usage: make [config=name] [target]"
	@echo ""
	@echo "CONFIGURATIONS:"
	@echo "  debug"
	@echo "  release"
	@echo "  dist"
	@echo ""
	@echo "TARGETS:"
	@echo "   all (default)"
	@echo "   clean"
	@echo "   aleph"
	@echo ""
	@echo "For more information, see https://github.com/premake/premake-core/wiki"