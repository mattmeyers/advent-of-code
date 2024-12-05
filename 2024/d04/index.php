<?php

// I intentionally allow undefined array key access to account for looking at
// values outside of the bounds of the arrays without constantly checking bounds.
// However, this prints warnings which clutter the output. This function call
// suppresses these warnings.
error_reporting(E_ERROR);

/** @return array<int, array<int, string>> */
function parseInput(string $filename): array
{
    $values = [];
    foreach (file($filename) as $line) {
        $values[] = str_split(trim($line));
    }
    return $values;
}

/** @param array<int, array<int, string>> $values */
function checkDirection1(array $values, int $i, int $j, int $di, int $dj): bool
{
    return $values[$i + $di][$j + $dj] === "M"
        && $values[$i + (2 * $di)][$j + (2 * $dj)] === "A"
        && $values[$i + (3 * $di)][$j + (3 * $dj)] === "S";
}

/** @param array<int, array<int, string>> $values */
function countXmas1(array $values, int $i, int $j): int
{
    $count = 0;
    $count += (int)checkDirection1($values, $i, $j, 0, 1);
    $count += (int)checkDirection1($values, $i, $j, 0, -1);
    $count += (int)checkDirection1($values, $i, $j, 1, 0);
    $count += (int)checkDirection1($values, $i, $j, -1, 0);
    $count += (int)checkDirection1($values, $i, $j, 1, 1);
    $count += (int)checkDirection1($values, $i, $j, 1, -1);
    $count += (int)checkDirection1($values, $i, $j, -1, 1);
    $count += (int)checkDirection1($values, $i, $j, -1, -1);

    return $count;
}

/** @param array<int, array<int, string>> $values */
function countXmas2(array $values, int $i, int $j): int
{
    return (int)(
        (
            ($values[$i + 1][$j + 1] === "M" && $values[$i - 1][$j - 1] === "S") ||
            ($values[$i + 1][$j + 1] === "S" && $values[$i - 1][$j - 1] === "M")
        ) && (
            ($values[$i + 1][$j - 1] === "M" && $values[$i - 1][$j + 1] === "S") ||
            ($values[$i + 1][$j - 1] === "S" && $values[$i - 1][$j + 1] === "M")
        )
    );
}

/** @param array<int, array<int, string>> $values */
function part1(array $values): int
{
    $count = 0;

    foreach ($values as $i => $row) {
        foreach ($row as $j => $c) {
            $count += $c === "X" ? countXmas1($values, $i, $j) : 0;
        }
    }

    return $count;
}

/** @param array<int, array<int, string>> $values */
function part2(array $values): int
{
    $count = 0;

    foreach ($values as $i => $row) {
        foreach ($row as $j => $c) {
            $count += $c === "A" ? countXmas2($values, $i, $j) : 0;
        }
    }

    return $count;
}

$sampleValues = parseInput("sample.txt");
$inputValues = parseInput("input.txt");

$p1Sample = part1($sampleValues);
$p1Input = part1($inputValues);
assert($p1Sample == 18);
assert($p1Input == 2573);

$p2Sample = part2($sampleValues);
$p2Input = part2($inputValues);
assert($p2Sample == 9);
assert($p2Input == 1850);

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
printf("    Input:  %d\n", $p1Input);

printf("PART 2:\n");
printf("    Sample: %d\n", $p2Sample);
printf("    Input:  %d\n", $p2Input);
