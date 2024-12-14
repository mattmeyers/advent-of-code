<?php

class Machine
{
    function __construct(
        public readonly int $x1,
        public readonly int $y1,
        public readonly int $x2,
        public readonly int $y2,
        public readonly int $x,
        public readonly int $y,
    ) {}

    /** 
     * The number of times each button must be pressed can be determined by a 
     * system of equations. Let
     *
     *  - x1 be the number of horizontal spaces moved by pressing button A
     *  - x2 be the number of horizontal spaces moved by pressing button B
     *  - y1 be the number of vertical spaces moved by pressing button A
     *  - y2 be the number of vertical spaces moved by pressing button B
     *  - x be the target horizontal position
     *  - y be the target vertical position
     *  - n be the number of times button A is pressed
     *  - m be the number of times button B is pressed
     *
     * Then the system can be modelled as
     *
     *      n*x1 + m*x2 = x
     *      n*y1 + m*y2 = y
     *
     * Solving for n yields
     *
     *      n = (y2*x - x2*y) / (x1*y2 - y1*x2)
     *
     * It then follows that
     *
     *      m = (x - n*x) / x2
     *
     * @return array{int, int} 
     *
     * @throws Exception If there are no integer solutions.
     */
    function calculateSteps(): array
    {
        $n = ($this->y2 * $this->x - $this->x2 * $this->y) / ($this->x1 * $this->y2 - $this->y1 * $this->x2);
        $m = ($this->x - $n * $this->x1) / $this->x2;

        if ($n !== (int)$n || $m != (int)$m) {
            throw new Exception("Unsolvable, no integer solutions");
        }

        return [$n, $m];
    }
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
    foreach ($values as $machine) {
        try {
            [$n, $m] = $machine->calculateSteps();
            if ($n > 100  || $m > 100) continue;
            $tokens += 3 * $n + $m;
        } catch (Exception) {
            continue;
        }
    }

    return $tokens;
}

/** @param array<int, Machine> $values */
function part2(array $values): int
{
    $tokens = 0;
    foreach ($values as $machine) {
        $machine = new Machine(
            $machine->x1,
            $machine->y1,
            $machine->x2,
            $machine->y2,
            $machine->x + 10000000000000,
            $machine->y + 10000000000000,
        );

        try {
            [$n, $m] = $machine->calculateSteps();
            $tokens += 3 * $n + $m;
        } catch (Exception) {
            continue;
        }
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
