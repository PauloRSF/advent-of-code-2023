DOCKER=docker run --rm -i -t -w /tmp 

01:
	@$(DOCKER) -v ./01-trebuchet:/source -v ./01-trebuchet/input.txt:/tmp/input.txt advent-of-code-2023 bash -c 'gnatmake -q -F /source/main.adb && ./main'

02:
	@$(DOCKER) -v ./02-cube-conundrum:/source -v ./02-cube-conundrum/input.txt:/tmp/input.txt advent-of-code-2023 bash -c "/freebasic/bin/fbc -x /tmp/main /source/main.bas && ./main"

.PHONY: 01 02
