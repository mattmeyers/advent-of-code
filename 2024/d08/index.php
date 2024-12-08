<?php

class Position
{
    function __construct(
        public readonly int $x,
        public readonly int $y,
    ) {}
}

/** @return array<int, array<int, string>> */
function parseInput(string $filename): array
{
    $lines = [];
    foreach (file($filename) as $line) {
        $lines[] = str_split(trim($line));
    }

    return $lines;
}

/** @param array<int, array<int, string>> $values */
function part1(array $values): int
{
    $nodes = [];
    foreach ($values as $y => $row) {
        foreach ($row as $x => $col) {
            if ($col !== ".") {
                $nodes[$col][] = new Position($x, $y);
            }
        }
    }

    $maxX = count($values[0]);
    $maxY = count($values);

    $antinodes = [];
    foreach (array_values($nodes) as $positions) {
        for ($i = 0; $i < count($positions); $i++) {
            for ($j = $i + 1; $j < count($positions); $j++) {
                $dx = $positions[$j]->x - $positions[$i]->x;
                $dy = $positions[$j]->y - $positions[$i]->y;

                $nx = $positions[$i]->x - $dx;
                $ny = $positions[$i]->y - $dy;
                if ($nx >= 0 && $nx < $maxX && $ny >= 0 && $ny < $maxY) {
                    $antinodes[sprintf("%d,%d", $nx, $ny)] = true;
                }

                $nx = $positions[$j]->x + $dx;
                $ny = $positions[$j]->y + $dy;
                if ($nx >= 0 && $nx < $maxX && $ny >= 0 && $ny < $maxY) {
                    $antinodes[sprintf("%d,%d", $nx, $ny)] = true;
                }
            }
        }
    }

    return count($antinodes);
}

/** @param array<int, array<int, string>> $values */
function part2(array $values): int
{
    $nodes = [];
    foreach ($values as $y => $row) {
        foreach ($row as $x => $col) {
            if ($col !== ".") {
                $nodes[$col][] = new Position($x, $y);
            }
        }
    }

    $maxX = count($values[0]);
    $maxY = count($values);

    $antinodes = [];
    foreach (array_values($nodes) as $positions) {
        for ($i = 0; $i < count($positions); $i++) {
            for ($j = $i + 1; $j < count($positions); $j++) {
                $dx = $positions[$j]->x - $positions[$i]->x;
                $dy = $positions[$j]->y - $positions[$i]->y;

                $m = 0;
                while (true) {
                    $nx = $positions[$i]->x - ($m * $dx);
                    $ny = $positions[$i]->y - ($m * $dy);
                    if ($nx >= 0 && $nx < $maxX && $ny >= 0 && $ny < $maxY) {
                        $antinodes[sprintf("%d,%d", $nx, $ny)] = true;
                    } else {
                        break;
                    }

                    $m++;
                }

                $m = 0;
                while (true) {
                    $nx = $positions[$j]->x + ($m * $dx);
                    $ny = $positions[$j]->y + ($m * $dy);
                    if ($nx >= 0 && $nx < $maxX && $ny >= 0 && $ny < $maxY) {
                        $antinodes[sprintf("%d,%d", $nx, $ny)] = true;
                    } else {
                        break;
                    }

                    $m++;
                }
            }
        }
    }

    return count($antinodes);
}

$sampleValues = parseInput("sample.txt");
$inputValues = parseInput("input.txt");

$p1Sample = part1($sampleValues);
$p1Input = part1($inputValues);
assert($p1Sample == 14);
assert($p1Input == 323);

$p2Sample = part2($sampleValues);
$p2Input = part2($inputValues);
assert($p2Sample == 34);
assert($p2Input == 1077);

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
printf("    Input:  %d\n", $p1Input);

printf("PART 2:\n");
printf("    Sample: %d\n", $p2Sample);
printf("    Input:  %d\n", $p2Input);
