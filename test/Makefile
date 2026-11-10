HS     := $(filter-out Common.hs,$(wildcard *.hs))
LOG := $(HS:.hs=.log)
STEP := $(HS:.hs=.step)

%.step %.log : %.hs
	runghc $< 2>&1 > $(^:.hs=.log)


steps: $(STEP)

.PHONY: steps
