<?php

function parseInput(string $filename): string
{
    return file_get_contents($filename);
}

function part1(string $input): int
{
    preg_match_all('/mul\((\d+),(\d+)\)/', $input, $matches);

    $sum = 0;
    for ($i = 0; $i < count($matches[0]); $i++) {
        $sum += $matches[1][$i] * $matches[2][$i];
    }

    return $sum;
}

function part2(string $input): int
{
    preg_match_all("/(?:mul\((\d+),(\d+)\))|(do(?:n\'t)?\(\))/", $input, $matches);

    $sum = 0;
    $doMultiply = true;
    for ($i = 0; $i < count($matches[0]); $i++) {
        if ($matches[0][$i] === "do()") {
            $doMultiply = true;
        } elseif ($matches[0][$i] === "don't()") {
            $doMultiply = false;
        } elseif ($doMultiply) {
            $sum += $matches[1][$i] * $matches[2][$i];
        }
    }

    return $sum;
}

$sample1 = parseInput("sample.txt");
$sample2 = parseInput("sample2.txt");
$input = parseInput("input.txt");

$p1Sample = part1($sample1);
$p1Input = part1($input);
assert($p1Sample == 161);
assert($p1Input == 175015740);

$p2Sample = part2($sample2);
$p2Input = part2($input);
assert($p2Sample == 48);
assert($p2Input == 112272912);

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
printf("    Input:  %d\n", $p1Input);

printf("PART 2:\n");
printf("    Sample: %d\n", $p2Sample);
printf("    Input:  %d\n", $p2Input);
