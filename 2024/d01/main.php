<?php

function parseInput(string $file): array
{
    $leftList = [];
    $rightList = [];
    foreach (file($file) as $line) {
        $values = explode("   ", $line);
        $leftList[] = (int)$values[0];
        $rightList[] = (int)$values[1];
    }

    return [$leftList, $rightList];
}

/**
 * @param array<int> $l
 * @param array<int> $r
 */
function part1(array $l, array $r): int
{
    sort($l);
    sort($r);

    $sum = 0;
    foreach ($l as $i => $v) {
        $sum += abs($v - $r[$i]);
    }

    return $sum;
}

/**
 * @param array<int> $l
 * @param array<int> $r
 */
function part2(array $l, array $r): int
{
    $counts = [];
    foreach ($r as $v) {
        if (!array_key_exists($v, $counts)) {
            $counts[$v] = 0;
        }

        $counts[$v]++;
    }

    $sum = 0;
    foreach ($l as $v) {
        $sum += $v * ($counts[$v] ?? 0);
    }
    return $sum;
}

[$sampleLeft, $sampleRight] = parseInput("sample.txt");
[$inputLeft, $inputRight] = parseInput("input.txt");

$p1Sample = part1($sampleLeft, $sampleRight);
$p1Input = part1($inputLeft, $inputRight);
assert($p1Sample == 11);
assert($p1Input == 2970687);

$p2Sample = part2($sampleLeft, $sampleRight);
$p2Input = part2($inputLeft, $inputRight);
assert($p2Sample == 31);
assert($p2Input == 23963899);

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
printf("    Input:  %d\n", $p1Input);
printf("PART 2:\n");
printf("    Sample: %d\n", $p2Sample);
printf("    Input:  %d\n", $p2Input);
