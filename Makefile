.PHONY: default
default: all

MAKE_toolsDir ?= tools
MAKE_binDir   ?= bin
MAKE_objDir   ?= obj
MAKE_utilsDir ?= Makefiles

include $(MAKE_utilsDir)/Haskell.mk

vpath %.hs src

# Targets
Main = $(call HASKELL_mkTarget,Main)
Test = $(call HASKELL_mkTarget,Test)

# Cabal dependencies
QuickCheck = $(call HASKELL_mkCabalDep,QuickCheck)

# Dependencies
$(Main): Main.hs
$(Test): Test.hs $(HUnit) $(QuickCheck)

.PHONY: test
test: $(Test)
	$(Test) +RTS -N

.PHONY: all
all: $(Main)

.PHONY: clean
clean: cleanall
