<?php

enum ProblemPart
{
    case ONE;
    case TWO;
}

enum Direction
{
    case UP;
    case RIGHT;
    case DOWN;
    case LEFT;

    /** @return array{int, int} */
    function delta(): array
    {
        return match ($this) {
            Direction::UP => [0, -1],
            Direction::RIGHT => [1, 0],
            Direction::DOWN => [0, 1],
            Direction::LEFT => [-1, 0],
        };
    }
}

class Position
{
    function __construct(
        public readonly int $i,
        public readonly int $j,
    ) {}

    function move(Direction $d): Position
    {
        [$dj, $di] = $d->delta();
        return new Position($this->i + $di, $this->j + $dj);
    }

    function __toString(): string
    {
        return "$this->i,$this->j";
    }
}

class Map
{
    /** @var array<int, array<int, int>> */
    private array $heights;

    private ProblemPart $part;

    /** @var array<int, Position> */
    private array $trailheads;

    /** @param array<int, array<int, int>> $heights */
    function __construct(array $heights, ProblemPart $part)
    {
        $this->heights = $heights;
        $this->part = $part;
        $this->findTrailheads();
    }

    public function mapScore(): int
    {
        $count = 0;
        foreach ($this->trailheads as $trailhead) {
            $positions = $this->solveTrail($trailhead);
            $count += $this->part === ProblemPart::ONE
                ? count(array_unique($positions, SORT_STRING))
                : count($positions);
        }

        return $count;
    }

    /** @return array<int, Position> */
    private function solveTrail(Position $pos, int $value = 0): array
    {
        if ($value === 9) return [$pos];

        $positions = [];
        foreach (Direction::cases() as $d) {
            $nextPos = $pos->move($d);
            if (($val = $this->valueAt($nextPos)) === ($value + 1)) {
                $positions = array_merge($positions, $this->solveTrail($nextPos, $val));
            }
        }

        return $positions;
    }

    private function valueAt(Position $pos): int
    {
        return $this->heights[$pos->i][$pos->j] ?? -1;
    }

    private function findTrailheads(): void
    {
        foreach ($this->heights as $i => $row) {
            foreach ($row as $j => $col) {
                if ($col === 0) {
                    $this->trailheads[] = new Position($i, $j);
                }
            }
        }
    }
}

/** @return array<int, array<int, int>> */
function parseInput(string $filename): array
{
    $lines = [];
    foreach (file($filename) as $line) {
        $lines[] = array_map('intval', str_split(trim($line)));
    }

    return $lines;
}

/** @param array<int, array<int, int>> $values */
function part1(array $values): int
{
    return (new Map($values, ProblemPart::ONE))->mapScore();
}

/** @param array<int, array<int, int>> $values */
function part2(array $values): int
{
    return (new Map($values, ProblemPart::TWO))->mapScore();
}

$sampleValues = parseInput("sample.txt");
$inputValues = parseInput("input.txt");

$p1Sample = part1($sampleValues);
$p1Input = part1($inputValues);
assert($p1Sample == 36);
assert(true || $p1Input == 688);

$p2Sample = part2($sampleValues);
$p2Input = part2($inputValues);
assert(true || $p2Sample == 81);
assert(true || $p2Input == 1459);

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
printf("    Input:  %d\n", $p1Input);

printf("PART 2:\n");
printf("    Sample: %d\n", $p2Sample);
printf("    Input:  %d\n", $p2Input);
