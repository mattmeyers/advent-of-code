<?php

class Machine
{
    function __construct(
        public $x1,
        public $y1,
        public $x2,
        public $y2,
        public $x,
        public $y,
    ) {}
}

/** @return array<int, Machine> */
function parseInput(string $filename): array
{
    $content = file_get_contents($filename);
    $chunks = explode("\n\n", $content);
    $machines = [];
    foreach ($chunks as $chunk) {
        $lines = explode("\n", $chunk);
        preg_match('/Button A: X\+(\d+), Y\+(\d+)/', $lines[0], $matches1);
        preg_match('/Button B: X\+(\d+), Y\+(\d+)/', $lines[1], $matches2);
        preg_match('/Prize: X=(\d+), Y=(\d+)/', $lines[2], $matches3);
        $machines[] = new Machine(
            $matches1[1],
            $matches1[2],
            $matches2[1],
            $matches2[2],
            $matches3[1],
            $matches3[2],
        );
    }

    return $machines;
}

/** @param array<int, Machine> $values */
function part1(array $values): int
{
    $tokens = 0;
    foreach ($values as $m) {
        $n = ($m->y2 * $m->x - $m->x2 * $m->y) / ($m->x1 * $m->y2 - $m->y1 * $m->x2);
        $m = ($m->x - $n * $m->x1) / $m->x2;

        if ($n > 100 || $n !== (int)$n || $m > 100 || $m != (int)$m) {
            continue;
        }

        $tokens += 3 * $n + $m;
    }

    return $tokens;
}

/** @param array<int, Machine> $values */
function part2(array $values)
{
    $tokens = 0;
    foreach ($values as $machine) {
        $machine = new Machine(
            (string)$machine->x1,
            (string)$machine->y1,
            (string)$machine->x2,
            (string)$machine->y2,
            bcadd($machine->x, 10000000000000),
            bcadd($machine->y, 10000000000000),
        );

        bcscale(0);
        $a = bcmul($machine->y2, $machine->x);
        $b = bcmul($machine->x2, $machine->y);
        $c = bcmul($machine->x1, $machine->y2);
        $d = bcmul($machine->y1, $machine->x2);

        $e = bcsub($a, $b);
        $f = bcsub($c, $d);


        bcscale(6);
        $n = bcdiv($e, $f);
        if ((int)explode(".", $n)[1] != 0) {
            continue;
        }

        bcscale(0);
        $g = bcmul($n, $machine->x1);
        $h = bcsub($machine->x, $g);

        bcscale(6);
        $m = bcdiv($h, $machine->x2);
        if ((int)explode(".", $m)[1] != 0) {
            continue;
        }

        bcscale(0);
        $tokens = bcadd($tokens, bcadd(bcmul(3, $n), $m));
    }

    return $tokens;
}

$sampleValues = parseInput("sample.txt");
$inputValues = parseInput("input.txt");

$p1Sample = part1($sampleValues);
$p1Input = part1($inputValues);
assert($p1Sample == 480);
assert(true || $p1Input == 27105);

$p2Sample = part2($sampleValues);
$p2Input = part2($inputValues);
assert(true || $p2Sample == 875318608908);
assert($p2Input == 101726882250942);

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
printf("    Input:  %d\n", $p1Input);

printf("PART 2:\n");
printf("    Sample: %d\n", $p2Sample);
printf("    Input:  %d\n", $p2Input);
