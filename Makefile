DOCKER=docker run --rm -it

01:
	@$(DOCKER) -w /tmp -v ./01-trebuchet:/source -v ./01-trebuchet/input.txt:/tmp/input.txt matth3wology/ada bash -c 'gnatmake -q -F /source/main.adb && ./main'

02:
	@$(DOCKER) -w /tmp -v ./02-cube-conundrum:/source -v ./02-cube-conundrum/input.txt:/tmp/input.txt primeimages/freebasic bash -c "fbc -x /tmp/main /source/main.bas && ./main"

03:
	@$(DOCKER) -v ./03-gear-ratios:/source -v ./03-gear-ratios/input.txt:/tmp/input.txt clojure bash -c "clojure -M /source/main.clj"

04:
	@$(DOCKER) -w /source -v ./04-scratchcards:/source -v ./04-scratchcards/input.txt:/tmp/input.txt -v ./04-scratchcards/.pub-cache:/root/.pub-cache dart bash -c "dart --disable-analytics run"

05:
	@$(DOCKER) -w /source -v ./05-if-you-give-a-seed-a-fertilizer:/source -v ./05-if-you-give-a-seed-a-fertilizer/input.txt:/tmp/input.txt erlang bash -c "erlc main.erl && erl -noshell -s main main -s init stop"

05-format:
	@$(DOCKER) -w /source -v ./05-if-you-give-a-seed-a-fertilizer/main.erl:/source/main.erl erlang bash -c "rebar3 compile && rebar3 format --files main.erl"

06:
	@$(DOCKER) -w /source -v ./06-wait-for-it:/source mcr.microsoft.com/dotnet/sdk bash -c "dotnet run"

07:
	@$(DOCKER) -w /source -v ./07-camel-cards:/source golang bash -c "go run main.go"

07-format:
	@$(DOCKER) -w /source -v ./07-camel-cards:/source golang bash -c "go fmt main.go"

.PHONY: 01 02 03 04 05 05-format 06 07 07-format
