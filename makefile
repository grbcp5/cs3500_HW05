default: buildAndRun

build:
	flex *.l
	bison *.y
	g++ *.tab.c -o mfpl_eval

run:
	./mfpl_eval testInput.txt

buildAndRun: build run
