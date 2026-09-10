run: test.cabal
	cabal run fillet
	cabal run infer
	cabal run path_offset
	cabal run section
	cabal run statistics
	cabal run sweep
	cabal run volume_offset

test.cabal: package.yaml
	hpack
	
clean:
	rm -rf dist-newstyle ob.step out.step solid.step test.cabal \
		sweep.step ring.step volume_offset.step ob.step

.PHONY: run clean
