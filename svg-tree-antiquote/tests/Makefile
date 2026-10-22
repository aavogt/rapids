.DEFAULT_GOAL := all
SHELL := bash
.ONESHELL:

RAPIDS_SVG ?= ../../rapids-svg
GHC_ENVIRONMENT ?= $(HOME)/.ghc/x86_64-linux-$(shell ghc --numeric-version)/environments/default
GHC_INCLUDE_FLAGS := -i../lib -i$(RAPIDS_SVG) -i.
GHC_FLAGS := -XCPP -DDEBUG_OFFSET \
							-XQuasiQuotes -XTemplateHaskell -XLambdaCase -XImplicitParams -XNoMonomorphismRestriction -XStandaloneDeriving -XDeriveLift \
							-package=BNFC-meta $(GHC_FLAGS) $(GHC_INCLUDE_FLAGS)
CASE_SOURCES := $(filter-out Common.hs,$(wildcard *.hs))
CASES := $(patsubst %.hs,%,$(CASE_SOURCES))
VOLUME_OUTPUTS := $(addprefix out/,$(addsuffix .out,$(CASES)))
STEP_OUTPUTS := $(addprefix out/,$(addsuffix .step,$(CASES)))
LOG_OUTPUTS := $(addprefix out/,$(addsuffix .log,$(CASES)))

.PHONY: all clean watch configure
all: $(VOLUME_OUTPUTS) $(STEP_OUTPUTS)

WATCH_CASE ?= ell.hs

watch:
	mkdir -p out
	set -o pipefail
	GHC_ENVIRONMENT="$(GHC_ENVIRONMENT)" ghcid -r -W -c "ghci $(GHC_FLAGS)" $(WATCH_CASE) \
		| tee \
				>(sed -u -n "/^\[[0-9eE.+,-]*\]$$/p" > "out/$(basename $(WATCH_CASE)).out") \
				>(sed -u -n "/^\[.*Just.*]$$/p" > "out/$(basename $(WATCH_CASE)).err") \
				>(sed -u -n "/^\[SvgTreeOffset\]/p" > "out/$(basename $(WATCH_CASE)).log")

configure: hie.yaml .ghci

hie.yaml: Makefile
	@{ \
	  printf 'cradle:\n  direct:\n    arguments:\n'; \
	  for flag in $(GHC_FLAGS); do printf '      - "%s"\n' "$$flag"; done; \
	} > $@

.ghci: Makefile
	@printf ':set %s\n' '$(GHC_INCLUDE_FLAGS)' > $@
	@chmod go-w $@ .

out/%.out out/%.step &: %.hs Common.hs ../lib/SvgTreeAntiquote.hs ../lib/SvgTreeOffset.hs $(RAPIDS_SVG)/Rapids/SVG.hs
	@mkdir -p out
	@set -e; \
	  run_log="out/$*.run.log"; \
	  offset_log="out/$*.log"; \
	  status=0; \
	  GHC_ENVIRONMENT="$(GHC_ENVIRONMENT)" runghc $(GHC_FLAGS) $< > "$$run_log" 2>&1 || status=$$?; \
	  sed -n '/^\[SvgTreeOffset\]/p' "$$run_log" | tee "$$offset_log" > /dev/null; \
	  if [ "$$status" -ne 0 ]; then \
	    cat "$$run_log"; rm -f "$$run_log"; exit "$$status"; \
	  fi; \
	  sed -n '/^\[[0-9eE.+,-]*\]$$/p' "$$run_log" > out/$*.out; \
	  rm -f "$$run_log"

clean:
	rm -f $(VOLUME_OUTPUTS) $(STEP_OUTPUTS) $(LOG_OUTPUTS)
