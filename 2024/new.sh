#!/usr/bin/env bash

set -e

day="$1"
if [ $# = 0 ]; then
    echo "Day number required"
    exit 1
elif [[ $day -le 9 ]]; then
    day="0$day"
fi

directory="d$day"

mkdir "$directory"
touch "$directory/sample.txt"
touch "$directory/input.txt"
cat > "$directory/index.php"  <<CODE
<?php

/** @return array<int, string> */
function parseInput(string \$filename): array
{
    \$lines = [];
    foreach (file(\$filename) as \$line) {
        \$lines[] = trim(\$line);
    }

    return \$lines;
}

/** @param array<int, string> \$values */
function part1(array \$values): int
{
    return 0;
}

/** @param array<int, string> \$values */
function part2(array \$values): int
{
    return 0;
}

\$sampleValues = parseInput("sample.txt");
\$inputValues = parseInput("input.txt");

\$p1Sample = part1(\$sampleValues);
\$p1Input = part1(\$inputValues);
assert(\$p1Sample == 0);
assert(true || \$p1Input == 0);

\$p2Sample = part2(\$sampleValues);
\$p2Input = part2(\$inputValues);
assert(true || \$p2Sample == 0);
assert(true || \$p2Input == 0);

printf("PART 1:\n");
printf("    Sample: %d\n", \$p1Sample);
printf("    Input:  %d\n", \$p1Input);

printf("PART 2:\n");
printf("    Sample: %d\n", \$p2Sample);
printf("    Input:  %d\n", \$p2Input);
CODE

