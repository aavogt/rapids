run: test.cabal
	cabal run statistics
	cabal run volume_offset

test.cabal: package.yaml
	hpack
	
clean:
	rm -rf dist-newstyle ob.step out.step solid.step test.cabal

.PHONY: run clean
