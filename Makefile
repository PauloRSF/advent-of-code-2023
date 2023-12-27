DOCKER=docker run --rm

01:
	@$(DOCKER) -w /tmp -v ./01-trebuchet:/source -v ./01-trebuchet/input.txt:/tmp/input.txt matth3wology/ada bash -c 'gnatmake -q -F /source/main.adb && ./main'

02:
	@$(DOCKER) -w /tmp -v ./02-cube-conundrum:/source -v ./02-cube-conundrum/input.txt:/tmp/input.txt primeimages/freebasic bash -c "fbc -x /tmp/main /source/main.bas && ./main"

03:
	@$(DOCKER) -v ./03-gear-ratios:/source -v ./03-gear-ratios/input.txt:/tmp/input.txt clojure bash -c "clojure -M /source/main.clj"

04:
	@$(DOCKER) -w /source -v ./04-scratchcards:/source -v ./04-scratchcards/input.txt:/tmp/input.txt -v ./04-scratchcards/.pub-cache:/root/.pub-cache arm64v8/dart bash -c "dart --disable-analytics run"

.PHONY: 01 02 03 04
