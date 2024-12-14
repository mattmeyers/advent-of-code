<?php

class Position
{
    function __construct(
        public readonly int $row,
        public readonly int $col,
    ) {}

    function __toString(): string
    {
        return "$this->row,$this->col";
    }
}

class Region
{
    /** @var array<string, Position> */
    public array $positions = [];

    function __construct(public readonly string $value) {}

    function contains(Position $pos): bool
    {
        return array_key_exists((string)$pos, $this->positions);
    }

    function addPosition(Position $pos): void
    {
        $this->positions[(string)$pos] = $pos;
    }
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
function valueAt(array $values, int $i, int $j): string
{
    return $values[$i][$j] ?? "";
}


/** @param array<string, int> &$counts */
function incrementCount(array &$counts, string $k): void
{
    $counts[$k] = ($counts[$k] ?? 0) + 1;
}

function calculateRegionPrice(array $values): int
{
    $areas = [];
    $perimeters = [];
    $deltas = [[-1, 0], [0, 1], [1, 0], [0, -1]];

    foreach ($values as $i => $row) {
        foreach ($row as $j => $c) {
            incrementCount($areas, $c);
            foreach ($deltas as $delta) {
                $neighbor = valueAt($values, $i + $delta[0], $j + $delta[1]);
                if ($neighbor != $c) {
                    incrementCount($perimeters, $c);
                }
            }
        }
    }

    $price = 0;
    foreach ($areas as $k => $v) {
        $price += $v * $perimeters[$k];
        printf("%s | %d | %d\n", $k, $v, $perimeters[$k]);
    }

    return $price;
}

/** @param array<int, array<int, string>> $values */
function part1(array $values): int
{
    /** @var array<string, array<int, Region>> */
    $regions = [];

    foreach ($values as $i => $row) {
        foreach ($row as $j => $c) {
            if (!array_key_exists($c, $regions)) {
                $r = new Region($c);
                $r->addPosition(new Position($i, $j));
                $regions[$c] = [$r];
                continue;
            }

            foreach ($regions[$c] as $region) {
                // Look up
                $neighbor = new Position($i - 1, $j);
                if ($region->contains($neighbor)) {
                    $region->addPosition(new Position($i, $j));
                    continue 2;
                }

                // Look left
                $neighbor = new Position($i, $j - 1);
                if ($region->contains($neighbor)) {
                    $region->addPosition(new Position($i, $j));
                    continue 2;
                }
            }

            $r = new Region($c);
            $r->addPosition(new Position($i, $j));
            $regions[$c][] = $r;
        }
    }

    foreach ($regions as $c => $r) {
        printf("%s -> %d regions\n", $c, count($r));
    }
    return 0;
}

/** @param array<int, string> $values */
function part2(array $values): int
{
    return 0;
}

$sampleValues = parseInput("sample.txt");
/* $inputValues = parseInput("input.txt"); */

$p1Sample = part1($sampleValues);
/* $p1Input = part1($inputValues); */
/* assert($p1Sample == 0); */
/* assert(true || $p1Input == 0); */

/* $p2Sample = part2($sampleValues); */
/* $p2Input = part2($inputValues); */
/* assert(true || $p2Sample == 0); */
/* assert(true || $p2Input == 0); */

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
/* printf("    Input:  %d\n", $p1Input); */

/* printf("PART 2:\n"); */
/* printf("    Sample: %d\n", $p2Sample); */
/* printf("    Input:  %d\n", $p2Input); */
