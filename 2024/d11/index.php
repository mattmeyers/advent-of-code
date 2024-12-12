<?php

/** @return array<int, string> */
function parseInput(string $filename): array
{
    return explode(" ", trim(file($filename)[0]));
}

/** @param array<int, string> $values */
function part1(array $values): int
{
    return solve($values, 25);
}

/** @param array<int, string> $values */
function part2(array $values): int
{
    return solve($values, 75);
}

/** @param array<int, string> $values */
function solve(array $values, int $blinks): int
{
    $values = array_count_values($values);
    for ($i = 0; $i < $blinks; $i++) {
        $newValues = [];
        foreach ($values as $val => $count) {
            if ($val == "0") {
                $newValues["1"] ??= 0;
                $newValues["1"] += $count;
            } elseif (($len = strlen($val)) % 2 === 0) {
                $k1 = substr($val, 0, $len / 2);
                $k2 = (string)(int)substr($val, $len / 2);

                $newValues[$k1] ??= 0;
                $newValues[$k2] ??= 0;

                $newValues[$k1] += $count;
                $newValues[$k2] += $count;
            } else {
                $k = (string)(2024 * (int)$val);
                $newValues[$k] ??= 0;
                $newValues[$k] += $count;
            }
        }

        $values = $newValues;
    }

    return array_sum(array_values($values));
}

$sampleValues = parseInput("sample.txt");
$inputValues = parseInput("input.txt");

$p1Sample = part1($sampleValues);
$p1Input = part1($inputValues);
assert($p1Sample == 55312);
assert($p1Input == 207683);

$p2Sample = part2($sampleValues);
$p2Input = part2($inputValues);
assert($p2Sample == 65601038650482);
assert($p2Input == 244782991106220);

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
printf("    Input:  %d\n", $p1Input);

printf("PART 2:\n");
printf("    Sample: %d\n", $p2Sample);
printf("    Input:  %d\n", $p2Input);
