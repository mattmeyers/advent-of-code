.SILENT: part1 part2
.PHONY: part1 part2

all: part1 part2

part1:
	echo "Part 1"
	echo "  Running Sample"
	echo "    Expect 142"
	echo -n "    Actual "
	jq -f p1.jq -Rs sample.txt

	echo "  Running Input"
	echo "    Expect 55816"
	echo -n "    Actual "
	jq -f p1.jq -Rs input.txt

part2:
	echo "Part 2"
	echo "  Running Sample"
	echo "    Expect 281"
	echo -n "    Actual "
	jq -f p2.jq -Rs sample2.txt

	echo "  Running Input"
	echo "    Expect 54980"
	echo -n "    Actual "
	jq -f p2.jq -Rs input.txt

