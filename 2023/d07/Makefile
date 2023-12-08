.SILENT: part1 part2
.PHONY: part1 part2

all: part1 part2

part1:
	echo "Part 1"
	echo "  Running Sample"
	echo "    Expect 6440"
	echo -n "    Actual "
	jq -f p1.jq -Rs sample.txt

	echo "  Running Input"
	echo "    Expect 255048101"
	echo -n "    Actual "
	jq -f p1.jq -Rs input.txt

part2:
	echo "Part 2"
	echo "  Running Sample"
	echo "    Expect 5905"
	echo -n "    Actual "
	jq -f p2.jq -Rs sample.txt

	echo "  Running Input"
	echo "    Expect 253718286"
	echo -n "    Actual "
	jq -f p2.jq -Rs input.txt

