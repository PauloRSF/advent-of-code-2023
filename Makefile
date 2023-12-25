DOCKER=docker run --rm -i -t --platform linux/amd64 -w /tmp

01:
	@$(DOCKER) -v ./01-trebuchet:/source -v ./01-trebuchet/input.txt:/tmp/input.txt advent-of-code-2023 bash -c 'gnatmake -q -F /source/main.adb && ./main'

02:
	@$(DOCKER) -v ./02-cube-conundrum:/source -v ./02-cube-conundrum/input.txt:/tmp/input.txt advent-of-code-2023 bash -c "/freebasic/bin/fbc -x /tmp/main /source/main.bas && ./main"

03:
	@$(DOCKER) -v ./03-gear-ratios:/source -v ./03-gear-ratios/input.txt:/tmp/input.txt advent-of-code-2023 bash -c "clojure -M /source/main.clj"

.PHONY: 01 02 03
