all: bin/day5

clean:
	rm -rf bin

bin/day5: AdventOfCode2016.hsproj/Day5.hs
	mkdir -p bin
	ghc $< -main-is Day5 -O2 -o bin/day5

