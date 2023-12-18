01:
	@bash -c "docker run --rm -i -t -w /tmp -v ./01-trebuchet:/source -v ./01-trebuchet/input.txt:/tmp/input.txt advent-of-code-2023 bash -c 'gnatmake -gnat12 -q -F /source/main.adb && ./main'"

.PHONY: 01
